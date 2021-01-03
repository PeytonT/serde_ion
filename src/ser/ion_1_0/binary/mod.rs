mod subfield;

use self::subfield::*;
use crate::binary::{type_descriptor, LengthCode, TypeCode};
use crate::error::{Error, SymbolError};
use crate::parser::parse::BVM_1_0;
use crate::symbols::{SymbolToken, SYSTEM_SYMBOL_TABLE_V1_SIZE};
use crate::value::{Blob, Clob, Data, Decimal, List, Sexp, Struct, Timestamp, Value};
use crate::Version;
use itertools::Itertools;
use num_bigint::{BigInt, Sign};
use num_traits::Zero;
use std::borrow::BorrowMut;
use std::collections::HashMap;
use std::mem::replace;

/// A binary writer takes a stream of Ion values and produces the corresponding binary bytestream.
///
/// For maximum compactness, write from the writer only once. This ensures that only a single local
/// symbol table is written, and that symbol IDs are efficiently distributed.
/// To minimize writes, use only one Ion version. Each time the version of written values changes,
/// the writer must write the new IVM and a new symbol table.
///
/// The writer retains the current symbol table between writes. For this reason, multiple writes
/// from the same writer will usually be more compact than joining the output of multiple writers.
pub struct Writer {
    // serialized bytes
    bytes: Vec<u8>,
    // local symbol table
    local_symbol_table: Option<LocalSymbolTable>,
    // symbols encountered in the current segment
    symbol_buffer: SymbolAccumulator,
    // values for the current segment
    value_buffer: Vec<Value>,
    // current segment Ion version
    current_version: Version,
}

impl Writer {
    pub fn new(current_version: Version) -> Writer {
        Writer {
            bytes: vec![],
            local_symbol_table: None,
            symbol_buffer: SymbolAccumulator::new(),
            value_buffer: vec![],
            current_version,
        }
    }

    // Writes the current segment to bytes, consisting of the IVM for the current Ion version,
    // the accumulated additions to the symbol table, and the buffered values.
    pub fn write(&mut self) {
        // There's only one version currently, but perhaps someday there will be more.
        match self.current_version {
            Version::Ion_1_0 => {
                self.bytes.extend_from_slice(&BVM_1_0);
                // Absorb the symbols accumulated in the symbol_buffer into the local_symbol_table.
                self.update_symbol_table_v1_0();
                // TODO: Serialize the symbol table
                // TODO: Serialize the buffered values using the symbol table
            }
        }
    }

    pub fn extend(&mut self, version: Version, values: Vec<Value>) -> Result<(), Error> {
        if self.current_version != version {
            self.write();
            self.current_version = version;
        }
        for value in &values {
            self.symbol_buffer.accumulate_symbols(value)?;
        }
        self.value_buffer.extend(values);
        Ok(())
    }

    pub fn append(&mut self, version: Version, value: Value) -> Result<(), Error> {
        if self.current_version != version {
            self.write();
            self.current_version = version;
        }
        self.symbol_buffer.accumulate_symbols(&value)?;
        self.value_buffer.push(value);
        Ok(())
    }

    // Absorbs the accumulated symbols in the symbol buffer into the local symbol table.
    // If there is a current symbol table it will be imported and extended, and otherwise a new
    // local symbol table will be created.
    fn update_symbol_table_v1_0(&mut self) {
        // Steal the accumulator's counting map and reset the accumulator.
        let mut symbol_counts: HashMap<String, i32> = replace(
            self.symbol_buffer.symbol_counts.borrow_mut(),
            HashMap::new(),
        );

        match &mut self.local_symbol_table {
            // Initialize a local symbol table from scratch if there is no previous table to import.
            None => {
                // Remove Ion 1.0 system symbols. These symbols have fixed symbol IDs, so if they managed to
                // find their way into the count we don't want to unnecessarily include them in the local
                // symbol table's symbol list.
                symbol_counts.remove("$ion");
                symbol_counts.remove("$ion_1_0");
                symbol_counts.remove("$ion_symbol_table");
                symbol_counts.remove("name");
                symbol_counts.remove("version");
                symbol_counts.remove("imports");
                symbol_counts.remove("symbols");
                symbol_counts.remove("max_id");
                symbol_counts.remove("$ion_shared_symbol_table");

                // Sort the accumulated symbols in reverse order by count. Earlier symbols get represented
                // with shorter VarUInts, so it is space-efficient to put the most common symbols first.
                let ordered_symbols: Vec<String> = symbol_counts
                    .into_iter()
                    .sorted_by_key(|x| -x.1)
                    .map(|x| x.0)
                    .collect();

                // Holds the symbol->offset map for efficiently serializing symbols.
                let mut symbol_offsets: HashMap<String, usize> = HashMap::new();

                // Add the Ion 1.0 system symbols to the offset map.
                symbol_offsets.insert("$ion".to_owned(), 1);
                symbol_offsets.insert("$ion_1_0".to_owned(), 2);
                symbol_offsets.insert("$ion_symbol_table".to_owned(), 3);
                symbol_offsets.insert("name".to_owned(), 4);
                symbol_offsets.insert("version".to_owned(), 5);
                symbol_offsets.insert("imports".to_owned(), 6);
                symbol_offsets.insert("symbols".to_owned(), 7);
                symbol_offsets.insert("max_id".to_owned(), 8);
                symbol_offsets.insert("$ion_shared_symbol_table".to_owned(), 9);

                // Add the accumulated symbols to the offset map.
                for (index, value) in ordered_symbols.iter().enumerate() {
                    symbol_offsets.insert(value.to_owned(), index);
                }

                let max_id = ordered_symbols.len() + SYSTEM_SYMBOL_TABLE_V1_SIZE;

                self.local_symbol_table = Some(LocalSymbolTable {
                    symbol_offsets,
                    ordered_symbols,
                    max_id,
                    import_previous_table: false,
                });
            }
            // Update the current local symbol table if one exists.
            Some(table) => {
                // Sort the accumulated symbols in reverse order by count. There's never a downside
                // to doing this, though it's much less likely to save space when extending an existing table.
                // Filter out any symbols that already exist in the table.
                let ordered_symbols: Vec<String> = symbol_counts
                    .into_iter()
                    .filter(|x| table.symbol_offsets.contains_key(&x.0))
                    .sorted_by_key(|x| -x.1)
                    .map(|x| x.0)
                    .collect();

                // Add the new symbols to the offset map.
                for (index, value) in ordered_symbols.iter().enumerate() {
                    table
                        .symbol_offsets
                        .insert(value.to_owned(), index + table.max_id);
                }

                table.max_id += ordered_symbols.len();
                table.import_previous_table = true;
            }
        }
    }
}

// Serialize a Value into the corresponding bytes in the context of the Local Symbol Table.
fn append_value(bytestream: &mut Vec<u8>, value: &Value, symbol_table: &LocalSymbolTable) {
    if !value.annotations.is_empty() {
        append_annotation(bytestream, value, symbol_table)
    } else {
        append_data(bytestream, &value.value, symbol_table);
    }
}

fn append_data(bytestream: &mut Vec<u8>, data: &Data, symbol_table: &LocalSymbolTable) {
    match data {
        Data::Null => append_null(bytestream),
        Data::Bool(x) => append_bool(bytestream, *x),
        Data::Int(x) => append_int(bytestream, x),
        Data::Float(x) => append_float(bytestream, *x),
        Data::Decimal(x) => append_decimal(bytestream, x),
        Data::Timestamp(x) => append_timestamp(bytestream, x),
        Data::String(x) => append_string(bytestream, x),
        Data::Symbol(x) => append_symbol(bytestream, x, symbol_table),
        Data::Blob(x) => append_blob(bytestream, x),
        Data::Clob(x) => append_clob(bytestream, x),
        Data::Struct(x) => append_struct(bytestream, x, symbol_table),
        Data::List(x) => append_list(bytestream, x, symbol_table),
        Data::Sexp(x) => append_sexp(bytestream, x, symbol_table),
    }
}

// ### 0: null
//
// ```text
//             7       4 3       0
//            +---------+---------+
// Null value |    0    |    15   |
//            +---------+---------+
// ```
//
// Values of type null always have empty lengths and representations.
// The only valid L value is 15, representing the only value of this type, null.null.
//
// NOP Padding
//
// ```text
//          7       4 3       0
//         +---------+---------+
// NOP Pad |    0    |    L    |
//         +---------+---------+======+
//         :     length [VarUInt]     :
//         +--------------------------+
//         |      ignored octets      |
//         +--------------------------+
// ```
//
// In addition to null.null, the null type code is used to encode padding that has no operation
// (NOP padding). This can be used for “binary whitespace” when alignment of octet boundaries is
// needed or to support in-place editing. Such encodings are not considered values and are ignored
// by the processor.
//
// In this encoding, L specifies the number of octets that should be ignored.
//
// The following is a single byte NOP pad. The NOP padding typedesc bytes are counted as padding:
//
// 0x00
//
// The following is a two byte NOP pad:
//
// 0x01 0xFE
//
// Note that the single byte of “payload” 0xFE is arbitrary and ignored by the parser.
//
// The following is a 16 byte NOP pad:
//
// 0x0E 0x8E 0x00 ... <12 arbitrary octets> ... 0x00
//
// NOP padding is valid anywhere a value can be encoded, except for within an annotation wrapper.
// NOP padding in struct requires additional encoding considerations.
fn append_null(bytestream: &mut Vec<u8>) {
    bytestream.push(0b0000_1111);
}

fn append_nop_pad(bytestream: &mut Vec<u8>, size: usize) {
    // Size is notably the total size of the padding, not the size of the payload.
    // As a result the VarUInt transition is at 15, rather than at 14, and the value of the
    // descriptor byte up to the transition is one less than the value of size.

    // Maximum NOP Pad size that can be represented with a one-byte VarUInt length, taking into account that one byte
    // will be needed for the type descriptor and one byte will be needed for the length.
    const ONE_BYTE_VARUINT_CUTOFF: usize = ONE_BYTE_VARUINT_RANGE_UPPER + 1 + 1;

    // NOP Pad size range that can be represented with a two-byte VarUInt length, taking into account that one byte
    // will be needed for the type descriptor and two bytes will be needed for the length.
    const TWO_BYTE_VARUINT_START: usize = ONE_BYTE_VARUINT_CUTOFF + 1;
    const TWO_BYTE_VARUINT_CUTOFF: usize = TWO_BYTE_VARUINT_RANGE_UPPER + 1 + 2;

    // NOP Pad size range that can be represented with a three-byte VarUInt length, taking into account that one byte
    // will be needed for the type descriptor and three bytes will be needed for the length.
    const THREE_BYTE_VARUINT_START: usize = TWO_BYTE_VARUINT_CUTOFF + 1;
    const THREE_BYTE_VARUINT_CUTOFF: usize = THREE_BYTE_VARUINT_RANGE_UPPER + 1 + 3;

    // NOP Pad size range that can be represented with a four-byte VarUInt length, taking into account that one byte
    // will be needed for the type descriptor and four bytes will be needed for the length.
    const FOUR_BYTE_VARUINT_FOUR: usize = THREE_BYTE_VARUINT_CUTOFF + 1;
    const FOUR_BYTE_VARUINT_CUTOFF: usize = FOUR_BYTE_VARUINT_RANGE_UPPER + 1 + 4;

    // TODO: Clean up this mess with a const fn once min_const_generics is stable.
    match size {
        0 => {}
        #[rustfmt::skip]
        1  => bytestream.extend_from_slice(&[0b0000_0000]),
        #[rustfmt::skip]
        2  => bytestream.extend_from_slice(&[0b0000_0001, 0b0000_0000]),
        #[rustfmt::skip]
        3  => bytestream.extend_from_slice(&[0b0000_0010, 0b0000_0000, 0b0000_0000]),
        #[rustfmt::skip]
        4  => bytestream.extend_from_slice(&[0b0000_0011, 0b0000_0000, 0b0000_0000, 0b0000_0000]),
        #[rustfmt::skip]
        5  => bytestream.extend_from_slice(&[0b0000_0100, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000]),
        #[rustfmt::skip]
        6  => bytestream.extend_from_slice(&[0b0000_0101, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000]),
        #[rustfmt::skip]
        7  => bytestream.extend_from_slice(&[0b0000_0110, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000]),
        #[rustfmt::skip]
        8  => bytestream.extend_from_slice(&[0b0000_0111, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000]),
        #[rustfmt::skip]
        9  => bytestream.extend_from_slice(&[0b0000_1000, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000]),
        #[rustfmt::skip]
        10 => bytestream.extend_from_slice(&[0b0000_1001, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000]),
        #[rustfmt::skip]
        11 => bytestream.extend_from_slice(&[0b0000_1010, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000]),
        #[rustfmt::skip]
        12 => bytestream.extend_from_slice(&[0b0000_1011, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000]),
        #[rustfmt::skip]
        13 => bytestream.extend_from_slice(&[0b0000_1100, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000]),
        #[rustfmt::skip]
        14 => bytestream.extend_from_slice(&[0b0000_1101, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0000]),
        15..=ONE_BYTE_VARUINT_CUTOFF => {
            bytestream.push(0b0000_1110);
            append_var_uint_usize(bytestream, size - 2);
            bytestream.extend(vec![0; size - 2]);
        }
        TWO_BYTE_VARUINT_START..=TWO_BYTE_VARUINT_CUTOFF => {
            bytestream.push(0b0000_1110);
            append_var_uint_usize(bytestream, size - 3);
            bytestream.extend(vec![0; size - 3]);
        }
        THREE_BYTE_VARUINT_START..=THREE_BYTE_VARUINT_CUTOFF => {
            bytestream.push(0b0000_1110);
            append_var_uint_usize(bytestream, size - 4);
            bytestream.extend(vec![0; size - 4]);
        }
        FOUR_BYTE_VARUINT_FOUR..=FOUR_BYTE_VARUINT_CUTOFF => {
            bytestream.push(0b0000_1110);
            append_var_uint_usize(bytestream, size - 5);
            bytestream.extend(vec![0; size - 5]);
        }
        _ => {
            panic!(
                "Maximum supported size for a NOP Pad is {} bytes. Size of {} requested.",
                FOUR_BYTE_VARUINT_CUTOFF, size
            );
        }
    }
}

// ### 1: bool
//
// ```text
//             7       4 3       0
//            +---------+---------+
// Bool value |    1    |   rep   |
//            +---------+---------+
// ```
//
// Values of type bool always have empty lengths, and their representation is stored in the typedesc
// itself (rather than after the typedesc). A representation of 0 means false; a representation of 1
// means true; and a representation of 15 means null.bool.
fn append_bool(bytestream: &mut Vec<u8>, value: Option<bool>) {
    bytestream.push(match value {
        None => 0b0001_1111,
        Some(false) => 0b0001_0000,
        Some(true) => 0b0001_0001,
    });
}

// ### 2/3: int
//
// Values of type int are stored using two type codes: 2 for positive values and 3 for negative values.
// Both codes use a UInt subfield to store the magnitude.
//
// ```text
//            7       4 3       0
//           +---------+---------+
// Int value |  2 or 3 |    L    |
//           +---------+---------+======+
//           :     length [VarUInt]     :
//           +==========================+
//           :     magnitude [UInt]     :
//           +==========================+
// ```
//
// Zero is always stored as positive; negative zero is illegal.
//
// If the value is zero then T must be 2, L should be zero, and if L is zero there must be no length or magnitude subfields.
// As a result, when T is 3, both L and the magnitude subfield must be non-zero.
//
// With either type code 2 or 3, if L is 15, then the value is null.int and the magnitude is empty.
// Note that this implies there are two equivalent binary representations of null integer values.
fn append_int(bytestream: &mut Vec<u8>, value: &Option<BigInt>) {
    match value {
        // For null.int we can use 0b0010_1111 or 0b0011_1111. There is no difference.
        None => bytestream.push(type_descriptor(TypeCode::PosInt, LengthCode::L15)),
        Some(int) => {
            let type_code: u8 = match int.sign() {
                Sign::NoSign => {
                    // NoSign means the value is zero. Zero is always encoded as T = 2, L = 0.
                    bytestream.push(type_descriptor(TypeCode::PosInt, LengthCode::L0));
                    return;
                }
                Sign::Plus => TypeCode::PosInt.to_byte(),
                Sign::Minus => TypeCode::NegInt.to_byte(),
            };
            let magnitude = serialize_uint(int.magnitude());
            match magnitude.len() {
                0..=13 => {
                    bytestream.push(type_code + (magnitude.len() as u8));
                }
                // As usual, if we need 14 or more bytes then L is set to 14 and the
                // optional length VarUInt is included.
                _ => {
                    bytestream.push(type_code + LengthCode::L14 as u8);
                    append_var_uint_usize(bytestream, magnitude.len());
                }
            }
            bytestream.extend(magnitude);
        }
    }
}

// ### 4: float
//
// ```text
//               7       4 3       0
//             +---------+---------+
// Float value |    4    |    L    |
//             +---------+---------+-----------+
//             |   representation [IEEE-754]   |
//             +-------------------------------+
// ```
//
// Floats are encoded as big endian octets of their IEEE 754 bit patterns.
//
// The L field of floats encodes the size of the IEEE-754 value.
//
// If L is 4, then the representation is 32 bits (4 octets).
// If L is 8, then the representation is 64 bits (8 octets).
// There are two exceptions for the L field:
//
// If L is 0, then the the value is 0e0 and representation is empty.
// Note, this is not to be confused with -0e0 which is a distinct value and in current Ion must be
// encoded as a normal IEEE float bit pattern.
// If L is 15, then the value is null.float and the representation is empty.
// Note: Ion 1.0 only supports 32-bit and 64-bit float values (i.e. L size 4 or 8), but future versions
// of the standard may support 16-bit and 128-bit float values.
#[allow(clippy::float_cmp)]
fn append_float(bytestream: &mut Vec<u8>, value: Option<f64>) {
    match value {
        None => bytestream.push(type_descriptor(TypeCode::Float, LengthCode::L15)),
        Some(float) => {
            match float {
                // Check if our float is positive 0.
                float if float == 0f64 && float.is_sign_positive() => {
                    bytestream.push(type_descriptor(TypeCode::Float, LengthCode::L0));
                }
                // Check if our float can be encoded in 4 bytes.
                float if (float as f32) as f64 == float => {
                    bytestream.push(type_descriptor(TypeCode::Float, LengthCode::L4));
                    bytestream.extend(&(float as f32).to_be_bytes());
                }
                // Encode using 8 bytes.
                _ => {
                    bytestream.push(type_descriptor(TypeCode::Float, LengthCode::L8));
                    bytestream.extend(&float.to_be_bytes());
                }
            }
        }
    }
}

// ### 5: decimal
//
// ```text
//                7       4 3       0
//               +---------+---------+
// Decimal value |    5    |    L    |
//               +---------+---------+======+
//               :     length [VarUInt]     :
//               +--------------------------+
//               |    exponent [VarInt]     |
//               +--------------------------+
//               |    coefficient [Int]     |
//               +--------------------------+
// ```
//
// Decimal representations have two components: exponent (a VarInt) and coefficient (an Int).
// The decimal’s value is coefficient * 10 ^ exponent.
//
// The length of the coefficient subfield is the total length of the representation minus the length of
// exponent. The subfield should not be present (that is, it has zero length) when the coefficient’s
// value is (positive) zero.
//
// If the value is 0. (aka 0d0) then L is zero, there are no length or representation fields, and the
// entire value is encoded as the single byte 0x50.
fn append_decimal(bytestream: &mut Vec<u8>, value: &Option<Decimal>) {
    match value {
        None => bytestream.push(type_descriptor(TypeCode::Decimal, LengthCode::L15)),
        Some(decimal) => {
            if decimal.coefficient.is_zero() && decimal.exponent.is_zero() {
                bytestream.push(type_descriptor(TypeCode::Decimal, LengthCode::L0));
                return;
            }
            let exponent = serialize_var_int(&decimal.exponent);
            let coefficient = match decimal.coefficient.is_zero() {
                true => Vec::new(),
                false => serialize_int(&decimal.coefficient),
            };
            let length = exponent.len() + coefficient.len();
            match length {
                0..=13 => {
                    bytestream.push(TypeCode::Decimal.to_byte() + length as u8);
                }
                // As usual, if we need 14 or more bytes then L is set to 14 and the
                // optional length VarUInt is included.
                _ => {
                    bytestream.push(TypeCode::Decimal.to_byte() + LengthCode::L14 as u8);
                    append_var_uint_usize(bytestream, length);
                }
            }
            bytestream.extend(exponent);
            bytestream.extend(coefficient);
        }
    }
}

// ### 6: timestamp
//
// ```text
//                  7       4 3       0
//                 +---------+---------+
// Timestamp value |    6    |    L    |
//                 +---------+---------+========+
//                 :      length [VarUInt]      :
//                 +----------------------------+
//                 |      offset [VarInt]       |
//                 +----------------------------+
//                 |       year [VarUInt]       |
//                 +----------------------------+
//                 :       month [VarUInt]      :
//                 +============================+
//                 :         day [VarUInt]      :
//                 +============================+
//                 :        hour [VarUInt]      :
//                 +====                    ====+
//                 :      minute [VarUInt]      :
//                 +============================+
//                 :      second [VarUInt]      :
//                 +============================+
//                 : fraction_exponent [VarInt] :
//                 +============================+
//                 : fraction_coefficient [Int] :
//                 +============================+
// ```
//
// Timestamp representations have 7 components, where 5 of these components are optional depending on
// the precision of the timestamp. The 2 non-optional components are offset and year.
// The 5 optional components are (from least precise to most precise): month, day, hour and minute,
// second, fraction_exponent and fraction_coefficient.
// All of these 7 components are in Universal Coordinated Time (UTC).
//
// The offset denotes the local-offset portion of the timestamp, in minutes difference from UTC.
//
// The hour and minute is considered as a single component, that is, it is illegal to have hour but
// not minute (and vice versa).
//
// The fraction_exponent and fraction_coefficient denote the fractional seconds of the timestamp as a
// decimal value. The fractional seconds’ value is coefficient * 10 ^ exponent.
// It must be greater than or equal to zero and less than 1. A missing coefficient defaults to zero.
// Fractions whose coefficient is zero and exponent is greater than -1 are ignored.
// The following hex encoded timestamps are equivalent:
//
// 68 80 0F D0 81 81 80 80 80       //  2000-01-01T00:00:00Z with no fractional seconds
// 69 80 0F D0 81 81 80 80 80 80    //  The same instant with 0d0 fractional seconds and implicit zero coefficient
// 6A 80 0F D0 81 81 80 80 80 80 00 //  The same instant with 0d0 fractional seconds and explicit zero coefficient
// 69 80 0F D0 81 81 80 80 80 C0    //  The same instant with 0d-0 fractional seconds
// 69 80 0F D0 81 81 80 80 80 81    //  The same instant with 0d1 fractional seconds
// Conversely, none of the following are equivalent:
//
// 68 80 0F D0 81 81 80 80 80       //  2000-01-01T00:00:00Z with no fractional seconds
// 69 80 0F D0 81 81 80 80 80 C1    //  2000-01-01T00:00:00.0Z
// 69 80 0F D0 81 81 80 80 80 C2    //  2000-01-01T00:00:00.00Z
// If a timestamp representation has a component of a certain precision, each of the less precise
// components must also be present or else the representation is illegal.
// For example, a timestamp representation that has a fraction_exponent and fraction_coefficient component but not the month component, is illegal.
//
// Note: The component values in the binary encoding are always in UTC, while components in the
// text encoding are in the local time! This means that transcoding requires a conversion between
// UTC and local time.
fn append_timestamp(bytestream: &mut Vec<u8>, value: &Option<Timestamp>) {
    fn append(bytestream: &mut Vec<u8>, contents: Vec<u8>) {
        match contents.len() {
            0..=13 => {
                bytestream.push(TypeCode::Timestamp.to_byte() + (contents.len() as u8));
            }
            // As usual, if we need 14 or more bytes then L is set to 14 and the
            // optional length VarUInt is included.
            length => {
                bytestream.push(TypeCode::Timestamp.to_byte() + LengthCode::L14 as u8);
                append_var_uint_usize(bytestream, length);
            }
        }
        bytestream.extend(contents);
    }

    // TODO: Add specialized subfield functions and reduce casting and allocating of BigInts.
    match value {
        None => bytestream.push(type_descriptor(TypeCode::Timestamp, LengthCode::L15)),
        Some(Timestamp::Year { offset, year }) => {
            let mut contents = serialize_var_int(&BigInt::from(*offset));
            append_var_uint_usize(&mut contents, *year as usize);
            append(bytestream, contents);
        }
        Some(Timestamp::Month {
            offset,
            year,
            month,
        }) => {
            let mut contents = serialize_var_int(&BigInt::from(*offset));
            append_var_uint_usize(&mut contents, *year as usize);
            append_var_uint_usize(&mut contents, *month as usize);
            append(bytestream, contents);
        }
        Some(Timestamp::Day {
            offset,
            year,
            month,
            day,
        }) => {
            let mut contents = serialize_var_int(&BigInt::from(*offset));
            append_var_uint_usize(&mut contents, *year as usize);
            append_var_uint_usize(&mut contents, *month as usize);
            append_var_uint_usize(&mut contents, *day as usize);
            append(bytestream, contents);
        }
        Some(Timestamp::Minute {
            offset,
            year,
            month,
            day,
            hour,
            minute,
        }) => {
            let mut contents = serialize_var_int(&BigInt::from(*offset));
            append_var_uint_usize(&mut contents, *year as usize);
            append_var_uint_usize(&mut contents, *month as usize);
            append_var_uint_usize(&mut contents, *day as usize);
            append_var_uint_usize(&mut contents, *hour as usize);
            append_var_uint_usize(&mut contents, *minute as usize);
            append(bytestream, contents);
        }
        Some(Timestamp::Second {
            offset,
            year,
            month,
            day,
            hour,
            minute,
            second,
        }) => {
            let mut contents = serialize_var_int(&BigInt::from(*offset));
            append_var_uint_usize(&mut contents, *year as usize);
            append_var_uint_usize(&mut contents, *month as usize);
            append_var_uint_usize(&mut contents, *day as usize);
            append_var_uint_usize(&mut contents, *hour as usize);
            append_var_uint_usize(&mut contents, *minute as usize);
            append_var_uint_usize(&mut contents, *second as usize);
            append(bytestream, contents);
        }
        Some(Timestamp::FractionalSecond {
            offset,
            year,
            month,
            day,
            hour,
            minute,
            second,
            fraction_coefficient,
            fraction_exponent,
        }) => {
            let mut contents = serialize_var_int(&BigInt::from(*offset));
            append_var_uint_usize(&mut contents, *year as usize);
            append_var_uint_usize(&mut contents, *month as usize);
            append_var_uint_usize(&mut contents, *day as usize);
            append_var_uint_usize(&mut contents, *hour as usize);
            append_var_uint_usize(&mut contents, *minute as usize);
            append_var_uint_usize(&mut contents, *second as usize);
            contents.extend(serialize_var_int_parts(Sign::Plus, fraction_coefficient));
            contents.extend(serialize_int(&BigInt::from(*fraction_exponent)));
            append(bytestream, contents);
        }
    }
}

// ### 7: symbol
//
// ```text
//               7       4 3       0
//              +---------+---------+
// Symbol value |    7    |    L    |
//              +---------+---------+======+
//              :     length [VarUInt]     :
//              +--------------------------+
//              |     symbol ID [UInt]     |
//              +--------------------------+
// ```
//
// In the binary encoding, all Ion symbols are stored as integer symbol IDs whose text values are
// provided by a symbol table. If L is zero then the symbol ID is zero and the length and symbol ID
// fields are omitted.
fn append_symbol(
    bytestream: &mut Vec<u8>,
    value: &Option<SymbolToken>,
    symbol_table: &LocalSymbolTable,
) {
    match value {
        None => bytestream.push(type_descriptor(TypeCode::Symbol, LengthCode::L15)),
        Some(SymbolToken::Zero) => {
            bytestream.push(type_descriptor(TypeCode::Symbol, LengthCode::L0))
        }
        Some(SymbolToken::Known { text }) => {
            // The symbol will have been accumulated in the symbol_accumulator prior to the write.
            let index: usize = symbol_table.get_symbol_index_unchecked(&text);
            // TODO: Strip off leading 0 bytes.
            let symbol_id = index.to_be_bytes();
            match symbol_id.len() {
                0..=13 => {
                    bytestream.push(TypeCode::Symbol.to_byte() + (symbol_id.len() as u8));
                }
                // As usual, if we need 14 or more bytes then L is set to 14 and the
                // optional length VarUInt is included.
                length => {
                    bytestream.push(TypeCode::Symbol.to_byte() + LengthCode::L14 as u8);
                    append_var_uint_usize(bytestream, length);
                }
            }
            bytestream.extend(&symbol_id);
        }
        Some(SymbolToken::Unknown { import_location }) => {
            // An unknown SymbolToken that cannot be resolved to the writer's imports or catalogue should error.
            // TODO: Import from the writer's import list or catalogue once implemented.
            // Any unresolvable symbol will have already been rejected.
            unreachable!(
                "Unexpected SymbolToken::Unknown with unresolvable ImportLocation {} cannot be written",
                import_location
            );
        }
    }
}

// ### 8: string
//
// ```text
//               7       4 3       0
//              +---------+---------+
// String value |    8    |    L    |
//              +---------+---------+======+
//              :     length [VarUInt]     :
//              +==========================+
//              :  representation [UTF8]   :
//              +==========================+
// ```
//
// These are always sequences of Unicode characters, encoded as a sequence of UTF-8 octets.
fn append_string(bytestream: &mut Vec<u8>, value: &Option<String>) {
    match value {
        None => bytestream.push(type_descriptor(TypeCode::String, LengthCode::L15)),
        Some(string) => {
            match string.as_bytes().len() {
                0..=13 => {
                    bytestream.push(TypeCode::String.to_byte() + string.as_bytes().len() as u8);
                }
                // As usual, if we need 14 or more bytes then L is set to 14 and the
                // optional length VarUInt is included.
                length => {
                    bytestream.push(TypeCode::String.to_byte() + LengthCode::L14 as u8);
                    append_var_uint_usize(bytestream, length);
                }
            }
            bytestream.extend(string.as_bytes());
        }
    }
}

// ### 9: clob
//
// ```text
//             7       4 3       0
//            +---------+---------+
// Clob value |    9    |    L    |
//            +---------+---------+======+
//            :     length [VarUInt]     :
//            +==========================+
//            :       data [Bytes]       :
//            +==========================+
// ```
//
// Values of type clob are encoded as a sequence of octets that should be interpreted as text with
// an unknown encoding (and thus opaque to the application).
// Zero-length clobs are legal, so L may be zero.
fn append_clob(bytestream: &mut Vec<u8>, value: &Option<Clob>) {
    match value {
        None => bytestream.push(type_descriptor(TypeCode::Clob, LengthCode::L15)),
        Some(Clob { data }) => {
            match data.len() {
                0..=13 => {
                    bytestream.push(TypeCode::Clob.to_byte() + data.len() as u8);
                }
                // As usual, if we need 14 or more bytes then L is set to 14 and the
                // optional length VarUInt is included.
                length => {
                    bytestream.push(TypeCode::Clob.to_byte() + LengthCode::L14 as u8);
                    append_var_uint_usize(bytestream, length);
                }
            }
            bytestream.extend(data);
        }
    }
}

// ### 10: blob
//
// ```text
//             7       4 3       0
//            +---------+---------+
// Blob value |   10    |    L    |
//            +---------+---------+======+
//            :     length [VarUInt]     :
//            +==========================+
//            :       data [Bytes]       :
//            +==========================+
// ```
//
// This is a sequence of octets with no interpretation (and thus opaque to the application).
// Zero-length blobs are legal, so L may be zero.
fn append_blob(bytestream: &mut Vec<u8>, value: &Option<Blob>) {
    match value {
        None => bytestream.push(type_descriptor(TypeCode::Blob, LengthCode::L15)),
        Some(Blob { data }) => {
            match data.len() {
                0..=13 => {
                    bytestream.push(TypeCode::Blob.to_byte() + data.len() as u8);
                }
                // As usual, if we need 14 or more bytes then L is set to 14 and the
                // optional length VarUInt is included.
                length => {
                    bytestream.push(TypeCode::Blob.to_byte() + LengthCode::L14 as u8);
                    append_var_uint_usize(bytestream, length);
                }
            }
            bytestream.extend(data);
        }
    }
}

// ### 11: list
//
// ```text
//             7       4 3       0
//            +---------+---------+
// List value |   11    |    L    |
//            +---------+---------+======+
//            :     length [VarUInt]     :
//            +==========================+
//            :           value          :
//            +==========================+
//                          ⋮
// ```
//
// The representation fields of a list value are simply nested Ion values.
//
// When L is 15, the value is null.list and there’s no length or nested values. When L is 0,
// the value is an empty list, and there’s no length or nested values.
fn append_list(bytestream: &mut Vec<u8>, value: &Option<List>, symbol_table: &LocalSymbolTable) {
    match value {
        None => bytestream.push(type_descriptor(TypeCode::List, LengthCode::L15)),
        Some(List { values }) if values.is_empty() => {
            bytestream.push(type_descriptor(TypeCode::List, LengthCode::L0))
        }
        Some(List { values }) => {
            // TODO: Optimize into two passes - first to compute length - second to serialize.
            let mut body: Vec<u8> = vec![];
            for element in values {
                append_value(&mut body, &element, symbol_table)
            }
            match body.len() {
                0..=13 => {
                    bytestream.push(TypeCode::List.to_byte() + body.len() as u8);
                }
                // As usual, if we need 14 or more bytes then L is set to 14 and the
                // optional length VarUInt is included.
                length => {
                    bytestream.push(TypeCode::List.to_byte() + LengthCode::L14 as u8);
                    append_var_uint_usize(bytestream, length);
                }
            }
            bytestream.extend(body);
        }
    }
}

// ### 12: sexp
//
// ```text
//             7       4 3       0
//            +---------+---------+
// Sexp value |   12    |    L    |
//            +---------+---------+======+
//            :     length [VarUInt]     :
//            +==========================+
//            :           value          :
//            +==========================+
//                          ⋮
// ```
//
// Values of type sexp are encoded exactly as are list values, except with a different type code.
fn append_sexp(bytestream: &mut Vec<u8>, value: &Option<Sexp>, symbol_table: &LocalSymbolTable) {
    match value {
        None => bytestream.push(type_descriptor(TypeCode::Sexp, LengthCode::L15)),
        Some(Sexp { values }) if values.is_empty() => {
            bytestream.push(type_descriptor(TypeCode::Sexp, LengthCode::L0))
        }
        Some(Sexp { values }) => {
            // TODO: Optimize into two passes - first to compute length - second to serialize.
            let mut temp: Vec<u8> = vec![];
            for element in values {
                append_value(&mut temp, &element, symbol_table)
            }
            match temp.len() {
                0..=13 => {
                    bytestream.push(TypeCode::Sexp.to_byte() + temp.len() as u8);
                }
                // As usual, if we need 14 or more bytes then L is set to 14 and the
                // optional length VarUInt is included.
                length => {
                    bytestream.push(TypeCode::Sexp.to_byte() + LengthCode::L14 as u8);
                    append_var_uint_usize(bytestream, length);
                }
            }
            bytestream.extend(temp);
        }
    }
}

// ### 13: struct
//
// Structs are encoded as sequences of symbol/value pairs. Since all symbols are encoded as positive
// integers, we can omit the typedesc on the field names and just encode the integer value.
//
// ```text
//               7       4 3       0
//              +---------+---------+
// Struct value |   13    |    L    |
//              +---------+---------+======+
//              :     length [VarUInt]     :
//              +======================+===+==================+
//              : field name [VarUInt] :        value         :
//              +======================+======================+
//                          ⋮                     ⋮
// ```
//
// Binary-encoded structs support a special case where the fields are known to be sorted
// such that the field-name integers are increasing. This state exists when L is one. Thus:
//
// When L is 0, the value is an empty struct, and there’s no length or nested fields.
// When L is 1, the struct has at least one symbol/value pair, the length field exists,
// and the field name integers are sorted in increasing order.
// When L is 15, the value is null.struct, and there’s no length or nested fields.
// When 1 < L < 14 then there is no length field as L is enough to represent the struct size,
// and no assertion is made about field ordering.
// Otherwise, the length field exists, and no assertion is made about field ordering.
//
// NOP Padding in struct Fields
// NOP Padding in struct values requires additional consideration of the field name element.
// If the “value” of a struct field is the NOP pad, then the field name is ignored.
// This means that it is not possible to encode padding in a struct value that is less than two bytes.
//
// Implementations should use symbol ID zero as the field name to emphasize the lack of meaning of
// the field name.
fn append_struct(
    bytestream: &mut Vec<u8>,
    value: &Option<Struct>,
    symbol_table: &LocalSymbolTable,
) {
    match value {
        // When L is 15, the value is null.struct, and there’s no length or nested fields.
        None => bytestream.push(type_descriptor(TypeCode::Struct, LengthCode::L15)),
        // When L is 0, the value is an empty struct, and there’s no length or nested fields.
        Some(Struct { fields }) if fields.is_empty() => {
            bytestream.push(type_descriptor(TypeCode::Struct, LengthCode::L0))
        }
        // TODO: Optimize into two passes - first to compute length - second to serialize?
        Some(Struct { fields }) => {
            let indexed_fields: Vec<(usize, &Value)> = fields
                .iter()
                .map(|(token, value)| match token {
                    // The symbol will have been accumulated in the symbol_accumulator prior to the write.
                    SymbolToken::Known { text } => {
                        (symbol_table.get_symbol_index_unchecked(&text), value)
                    }
                    SymbolToken::Zero => (0usize, value),
                    SymbolToken::Unknown { import_location } => {
                        // An unknown SymbolToken that cannot be resolved to the writer's imports or catalogue should error.
                        // TODO: Import from the writer's import list or catalogue once implemented.
                        // Any unresolvable symbol will have already been rejected.
                        unreachable!(
                            "Unexpected SymbolToken::Unknown with unresolvable ImportLocation {} cannot be written",
                            import_location
                        );
                    }
                })
                .collect();

            // TODO: Use is_sorted once stable. https://github.com/rust-lang/rust/issues/53485
            // Check if the symbol IDs happen to be sorted in ascending order
            let sorted = indexed_fields.windows(2).all(|w| w[0].0 <= w[1].0);

            let fields: Vec<(Vec<u8>, &Value)> = indexed_fields
                .into_iter()
                .map(|(index, value)| (serialize_var_uint_usize(index), value))
                .collect();

            let mut body: Vec<u8> = vec![];
            for (field_name, value) in fields {
                body.extend(field_name);
                append_value(&mut body, value, symbol_table)
            }
            match (sorted, body.len()) {
                // When 1 < L < 14 then there is no length field as L is enough to represent the struct size,
                // and no assertion is made about field ordering.
                (false, 2..=13) => {
                    bytestream.push(TypeCode::Struct.to_byte() + body.len() as u8);
                }
                // As usual, if we need 14 or more bytes then L is set to 14 and the
                // optional length VarUInt is included.
                (false, length) => {
                    bytestream.push(TypeCode::Struct.to_byte() + LengthCode::L14 as u8);
                    append_var_uint_usize(bytestream, length);
                }
                // When L is 1, the struct has at least one symbol/value pair, the length field exists,
                // and the field name integers are sorted in increasing order.
                (true, length) => {
                    bytestream.push(TypeCode::Struct.to_byte() + LengthCode::L1 as u8);
                    append_var_uint_usize(bytestream, length);
                }
            }
            bytestream.extend(body);
        }
    }
}

// ### 14: Annotations
//
// This special type code doesn’t map to an Ion value type,
// but instead is a wrapper used to associate annotations with content.
//
// Annotations are a special type that wrap content identified by the other type codes.
// The annotations themselves are encoded as integer symbol ids.
//
// ```text
//                     7       4 3       0
//                    +---------+---------+
// Annotation wrapper |   14    |    L    |
//                    +---------+---------+======+
//                    :     length [VarUInt]     :
//                    +--------------------------+
//                    |  annot_length [VarUInt]  |
//                    +--------------------------+
//                    |      annot [VarUInt]     |  …
//                    +--------------------------+
//                    |          value           |
//                    +--------------------------+
// ```
//
// The length field L field indicates the length from the beginning of the annot_length field to the
// end of the enclosed value. Because at least one annotation and exactly one content field must exist,
// L is at least 3 and is never 15.
//
// The annot_length field contains the length of the (one or more) annot fields.
//
// It is illegal for an annotation to wrap another annotation atomically, i.e.,
// annotation(annotation(value)).
//
// Furthermore, it is illegal for an annotation to wrap a NOP Pad since this encoding is not an
// Ion value.
//
// The Ion specification notes that in the text format, annotations are denoted by a non-null symbol
// token. Because the text and binary formats are semantically isomorphic, it follows that
// a null symbol cannot appear as an annotation.
fn append_annotation(bytestream: &mut Vec<u8>, value: &Value, symbol_table: &LocalSymbolTable) {
    let annotations: Vec<u8> = value
        .annotations
        .iter()
        .map(|token| match token {
            // The symbol will have been accumulated in the symbol_accumulator prior to the write.
            SymbolToken::Known { text } => symbol_table.get_symbol_index_unchecked(&text),
            SymbolToken::Zero => 0usize,
            SymbolToken::Unknown { import_location } => {
                // An unknown SymbolToken that cannot be resolved to the writer's imports or catalogue should error.
                // TODO: Import from the writer's import list or catalogue once implemented.
                // Any unresolvable symbol will have already been rejected.
                unreachable!(
                    "Unexpected SymbolToken::Unknown with unresolvable ImportLocation {} cannot be written",
                    import_location
                );
            }
        })
        .map(serialize_var_uint_usize)
        .flatten()
        .collect();
    let mut body: Vec<u8> = vec![];
    body.extend(serialize_var_uint_usize(annotations.len()));
    body.extend(annotations);
    append_data(&mut body, &value.value, symbol_table);
    match body.len() {
        0..=13 => {
            bytestream.push(TypeCode::Annotation.to_byte() + body.len() as u8);
        }
        // As usual, if we need 14 or more bytes then L is set to 14 and the optional length VarUInt is included.
        length => {
            bytestream.push(TypeCode::Annotation.to_byte() + LengthCode::L14 as u8);
            append_var_uint_usize(bytestream, length);
        }
    }
    bytestream.extend(body);
}

#[derive(Debug)]
struct SymbolAccumulator {
    // Mapping from each literal symbol string to the number of times it has been encountered.
    symbol_counts: HashMap<String, i32>,
}

impl SymbolAccumulator {
    fn new() -> SymbolAccumulator {
        SymbolAccumulator {
            symbol_counts: HashMap::new(),
        }
    }

    fn increment(&mut self, literal: &str) {
        let counter = self.symbol_counts.entry(literal.to_owned()).or_insert(0);
        *counter += 1;
    }

    // TODO: Consider taking value as mut and updating resolvable Unknown symbols to their resolved text.
    fn accumulate_symbols(&mut self, value: &Value) -> Result<(), SymbolError> {
        // TODO: Depends on implementation of support for shared symbol tables.
        // When encountering an Unknown Symbol for serialization:
        // If a match to the ImportLocation in the system symbol table or the writer’s imports list is
        // * Found, and the resolved SymbolToken has
        //   * Known text: the behavior is the same as calling a raw text API, as described above, with that text.
        //   * Unknown text: calculate the local symbol ID of the resolved SymbolToken’s importLocation in the current local symbol table.
        //     * Binary writers, write this local symbol ID as-is.
        // * Not found, and a match to the importLocation in the writer’s catalog (if present) is
        //   * Found, and the resolved SymbolToken has
        //     * Known text, the behavior is the same as calling a raw text API, as described above, with that text.
        //     * Unknown text, an error must be raised.
        //   * Not found, an error must be raised.
        // In the meantime, a match to the ImportLocation in the system symbol table or the
        // writer’s imports list is necessarily Not Found and a match to the ImportLocation in
        // the writer’s catalog is necessarily Not Found, so an error must be raised.
        for annotation in &value.annotations {
            match annotation {
                SymbolToken::Known { text } => {
                    self.increment(text);
                }
                SymbolToken::Unknown { import_location } => {
                    // TODO: Complete once catalogue lookup is in place.
                    return Err(SymbolError::UnresolvableImport(import_location.clone()));
                }
                SymbolToken::Zero => {} // No-op
            }
        }
        match &value.value {
            Data::Symbol(Some(SymbolToken::Known { text })) => {
                self.increment(text);
            }
            Data::Symbol(Some(SymbolToken::Unknown { import_location })) => {
                // TODO: Complete once catalogue lookup is in place.
                return Err(SymbolError::UnresolvableImport(import_location.clone()));
            }
            Data::Struct(Some(r#struct)) => {
                for (token, value) in &r#struct.fields {
                    match token {
                        SymbolToken::Known { text } => {
                            self.increment(text);
                        }
                        SymbolToken::Unknown { import_location } => {
                            // TODO: Complete once catalogue lookup is in place.
                            return Err(SymbolError::UnresolvableImport(import_location.clone()));
                        }
                        SymbolToken::Zero => {} // No-op
                    }
                    self.accumulate_symbols(value)?
                }
            }
            Data::List(Some(list)) => {
                for value in &list.values {
                    self.accumulate_symbols(&value)?
                }
            }
            Data::Sexp(Some(sexp)) => {
                for value in &sexp.values {
                    self.accumulate_symbols(&value)?
                }
            }
            // No other variants contain symbols that need to be accumulated.
            // These are the symbol-free variants, as well as any Symbol Zero symbols.
            _ => {}
        }
        Ok(())
    }
}

#[derive(Debug)]
struct LocalSymbolTable {
    // Mapping from each literal symbol string to its index in the local symbol table.
    // TODO: The mapping must be regenerated each time the version changes, which is currently never.
    symbol_offsets: HashMap<String, usize>,
    // The 'symbols' list of the serialized local symbol table.
    // Added in addition to any symbols imported from the system table or any imported tables, including the previous table.
    ordered_symbols: Vec<String>,
    // It's useful to keep track of the size of the logical symbol list.
    // This will be more useful if imports other than the current table are ever implemented.
    max_id: usize,
    // Informs table serialization logic of whether there is a preceding table that should be imported.
    import_previous_table: bool,
}

impl LocalSymbolTable {
    /// Returns the local symbol ID for the given text.
    ///
    /// # Panics
    ///
    /// Panics if the symbol text is not mapped in the local symbol table.
    fn get_symbol_index_unchecked(&self, symbol_text: &str) -> usize {
        *self.symbol_offsets.get(symbol_text).unwrap()
    }
}
