mod subfield;

use self::subfield::*;
use super::current_symbol_table::*;
use crate::{
    binary::{LengthCode, TypeCode},
    error::{BinaryFormatError, FormatError, TimeComponent},
    parser::{
        ion_1_0::current_symbol_table::CurrentSymbolTable,
        parse_error::{IonError, IonResult},
    },
    symbols::{SymbolToken, SYSTEM_SYMBOL_TABLE_V1},
    value::{Blob, Clob, Data, Decimal, List, Sexp, Struct, Timestamp, Value},
};
use itertools::Itertools;
use nom::{
    bytes::complete::take,
    combinator::{all_consuming, complete},
    error::{ErrorKind, ParseError},
    multi::{many0, many1},
    number::complete::{double, float},
    sequence::pair,
    Err,
};
use num_bigint::{BigInt, BigUint, Sign, ToBigInt};
use num_traits::{cast::FromPrimitive, identities::Zero, ToPrimitive};

type ParseResult<I, T> = Result<T, Err<IonError<I>>>;

pub fn parse(
    mut table: CurrentSymbolTable,
) -> impl FnMut(&[u8]) -> IonResult<&[u8], Option<Value>> {
    move |i: &[u8]| parse_top_level_value(i, &mut table)
}

/// Take a single top level Value from the head of an Ion byte stream.
///
/// The top level distinction is important, since certain binary representations
/// encountered at the top level result in updates to the current symbol table instead
/// of being parsed as a value.
fn parse_top_level_value<'a, 'b>(
    i: &'a [u8],
    symbol_table: &'b mut CurrentSymbolTable,
) -> IonResult<&'a [u8], Option<Value>> {
    let parse_result = take_value(symbol_table)(i);
    match parse_result {
        Ok((rest, Some(value))) => {
            // If the value is a Struct...
            if let Data::Struct(ion_struct) = &value.value {
                // And the annotations vector contains a first annotation (i.e. is non-empty)...
                if let Some(token) = value.annotations.first() {
                    // And the value of the annotation is "$ion_symbol_table"...
                    if *token == SYSTEM_SYMBOL_TABLE_V1.symbols[3] {
                        // Then it is an update to the local symbol table. Apply it.
                        update_current_symbol_table(symbol_table, ion_struct)
                            .map_err(|e| Err::Failure(IonError::from_symbol_error(i, e)))?;
                        // And return no Value
                        return Ok((rest, None));
                    }
                }
            }
            // In all other cases, it is an Ion value.
            Ok((rest, Some(value)))
        }
        // If there's no Value, it was a NOP pad
        Ok((rest, None)) => Ok((rest, None)),
        Err(err) => Err(err),
    }
}

/// Generate a parser that takes a single Value from the head of an Ion byte stream.
fn take_value(
    symbol_table: &CurrentSymbolTable,
) -> impl Fn(&[u8]) -> IonResult<&[u8], Option<Value>> + '_ {
    move |i: &[u8]| {
        let (rest, typed_value) = take_typed_value(i)?;
        let value = parse_typed_value(typed_value, symbol_table)?;
        Ok((rest, value))
    }
}

/// Parse a TypedValue containing bytes representing an Ion value into the Value data model form.
/// A TypedValue may represent a No-Op pad, so there may not be a Value.
fn parse_typed_value<'a>(
    value: TypedValue<'a>,
    symbol_table: &CurrentSymbolTable,
) -> ParseResult<&'a [u8], Option<Value>> {
    match value.type_code {
        TypeCode::Null => match parse_null(value) {
            None => Ok(None),
            Some(_) => wrap_data(Data::Null),
        },
        TypeCode::Bool => wrap_data(Data::Bool(parse_bool(value)?)),
        TypeCode::PosInt => wrap_data(Data::Int(parse_positive_int(value)?)),
        TypeCode::NegInt => wrap_data(Data::Int(parse_negative_int(value)?)),
        TypeCode::Float => wrap_data(Data::Float(parse_float(value)?)),
        TypeCode::Decimal => wrap_data(Data::Decimal(parse_decimal(value)?)),
        TypeCode::Timestamp => wrap_data(Data::Timestamp(parse_timestamp(value)?)),
        TypeCode::Symbol => wrap_data(Data::Symbol(parse_symbol(value, symbol_table)?)),
        TypeCode::String => wrap_data(Data::String(parse_string(value)?)),
        TypeCode::Clob => wrap_data(Data::Clob(parse_clob(value)?)),
        TypeCode::Blob => wrap_data(Data::Blob(parse_blob(value)?)),
        TypeCode::List => wrap_data(Data::List(parse_list(value, symbol_table)?)),
        TypeCode::Sexp => wrap_data(Data::Sexp(parse_sexp(value, symbol_table)?)),
        TypeCode::Struct => wrap_data(Data::Struct(parse_struct(value, symbol_table)?)),
        TypeCode::Annotation => match parse_annotation(value, symbol_table) {
            Ok(value) => Ok(Some(value)),
            Err(err) => Err(err),
        },
        TypeCode::Reserved => Err(error_reserved(value)),
    }
}

fn wrap_data<'a>(data: Data) -> ParseResult<&'a [u8], Option<Value>> {
    Ok(Some(Value {
        value: data,
        annotations: vec![],
    }))
}

/// ## Typed Value Formats
///
/// A value consists of a one-octet type descriptor, possibly followed by a length in octets,
/// possibly followed by a representation.
///
/// ```text
///        7       4 3       0
///       +---------+---------+
/// value |    T    |    L    |
///       +---------+---------+======+
///       :     length [VarUInt]     :
///       +==========================+
///       :      representation      :
///       +==========================+
/// ```
///
/// The type descriptor octet has two subfields: a four-bit type code T, and a four-bit length L.
///
/// Each value of T identifies the format of the representation, and generally (though not always)
/// identifies an Ion datatype. Each type code T defines the semantics of its length field L as
/// described below.
///
/// The length value – the number of octets in the representation field(s) – is encoded in L and/or
/// length fields, depending on the magnitude and on some particulars of the actual type.
/// The length field is empty (taking up no octets in the message) if we can store the length value
/// inside L itself. If the length field is not empty, then it is a single VarUInt field.
/// The representation may also be empty (no octets) in some cases, as detailed below.
///
/// Unless otherwise defined, the length of the representation is encoded as follows:
///
/// If the value is null (for that type), then L is set to 15.
/// If the representation is less than 14 bytes long, then L is set to the length,
/// and the length field is omitted.
/// If the representation is at least 14 bytes long, then L is set to 14,
/// and the length field is set to the representation length, encoded as a VarUInt field.
///
/// ## Illegal Type Descriptors
///
/// The preceding sections define valid type descriptor octets, composed of a type code (T) in the
/// upper four bits and a length field (L) in the lower four bits. As mentioned,
/// many possible combinations are illegal and must cause parsing errors.
///
/// The following table enumerates the illegal type descriptors in Ion 1.0 data.
///
/// T    L                 Reason
/// 1    [3-14]             For bool values, L is used to encode the value,
///                             and may be 0 (false), 1 (true), or 15 (null.bool).
/// 3    [0]                The int 0 is always stored with type code 2.
///                             Thus, type code 3 with L equal to zero is illegal.
/// 4    [1-3],[5-7],[9-14] For float values, only 32-bit and 64-bit IEEE-754 values are supported.
///                             Additionally, 0e0 and null.float are represented with L equal to 0 and 15,
///                             respectively.
/// 14    [0]*,[1-2],[15]   Annotation wrappers must have one annot_length field, at least one annot
///                             field, and exactly one value field. Null annotation wrappers are illegal.
///                             *Note: Since 0xE0 signals the start of the BVM, encountering this octet
///                             where a type descriptor is expected should only cause parsing errors when
///                             it is not followed by the rest of the BVM octet sequence.
/// 15    [0-15]            The type code 15 is illegal in Ion 1.0 data.

/// A partially-processed value from an Ion stream
///
/// This partially-processed form exists to support skip-searching over a value stream.
/// It contains information required to complete processing or to skip over the value.
#[derive(Clone, Debug, PartialEq)]
struct TypedValue<'a> {
    /// The type code T from the type descriptor
    pub type_code: TypeCode,
    /// The four-bit length L from the type descriptor.
    /// Not to be confused with the length of the representation.
    pub length_code: LengthCode,
    /// Slice of the Ion stream from the starting index of the value
    pub index: &'a [u8],
    /// The sequence of octets comprising the binary value.
    pub value: &'a [u8],
    /// The sequence of octets comprising the representation.
    pub rep: &'a [u8],
}

/// Exists to support skip-searching over a value stream.
// TODO: Add a public API for skip-searching.
fn take_typed_value(index: &[u8]) -> IonResult<&[u8], TypedValue> {
    let (rest, descriptor_byte) = take(1usize)(index)?;
    let type_code: TypeCode = TypeCode::from_u8(descriptor_byte[0] >> 4).unwrap();
    let length_code: LengthCode = LengthCode::from_u8(descriptor_byte[0] & 0b0000_1111).unwrap();
    match (type_code, length_code) {
        // 1-byte NOP pad (L == 0) and null.null (L == 15) both have empty representation
        (TypeCode::Null, LengthCode::L0) | (TypeCode::Null, LengthCode::L15) => Ok((
            rest,
            TypedValue {
                type_code,
                length_code,
                index,
                value: descriptor_byte,
                rep: &[],
            },
        )),
        // bools have only a descriptor octet and store their value in length_code
        (TypeCode::Bool, LengthCode::L0)
        | (TypeCode::Bool, LengthCode::L1)
        | (TypeCode::Bool, LengthCode::L15) => Ok((
            rest,
            TypedValue {
                type_code,
                length_code,
                index,
                value: descriptor_byte,
                rep: &[],
            },
        )),
        // bool length_codes other than 0, 1, and 15 are invalid
        (TypeCode::Bool, invalid) => Err(Err::Failure(IonError::from_format_error(
            index,
            FormatError::Binary(BinaryFormatError::BoolValue(invalid as u8)),
        ))),
        (TypeCode::NegInt, LengthCode::L0) => Err(Err::Failure(IonError::from_format_error(
            index,
            FormatError::Binary(BinaryFormatError::NegativeZero),
        ))),
        // Special cases: 0e0 (L == 0) and null.float (L == 15) both have empty representation
        (TypeCode::Float, LengthCode::L0) | (TypeCode::Float, LengthCode::L15) => Ok((
            rest,
            TypedValue {
                type_code,
                length_code,
                index,
                value: descriptor_byte,
                rep: &[],
            },
        )),
        (TypeCode::Float, LengthCode::L4) => {
            let (end, rep) = take(4usize)(rest)?;
            // TODO: remove unnecessary checks
            // we know these 5 bytes exist from the previous line
            let (end, value) = take(5usize)(index)?;
            Ok((
                end,
                TypedValue {
                    type_code,
                    length_code,
                    index,
                    value,
                    rep,
                },
            ))
        }
        (TypeCode::Float, LengthCode::L8) => {
            let (end, rep) = take(8usize)(rest)?;
            // TODO: remove unnecessary checks
            // we know these 9 bytes exist from the previous line
            let (end, value) = take(9usize)(index)?;
            Ok((
                end,
                TypedValue {
                    type_code,
                    length_code,
                    index,
                    value,
                    rep,
                },
            ))
        }
        (TypeCode::Float, invalid) => Err(Err::Failure(IonError::from_format_error(
            index,
            FormatError::Binary(BinaryFormatError::FloatLength(invalid as u8)),
        ))),
        // For timestamp values, a VarInt offset and VarUInt year are required.
        // Thus, type code 6 with L equal to zero or one is illegal.
        (TypeCode::Timestamp, LengthCode::L0) | (TypeCode::Timestamp, LengthCode::L1) => {
            Err(Err::Failure(IonError::from_format_error(
                index,
                FormatError::Binary(BinaryFormatError::TimestampLength(length_code as u8)),
            )))
        }
        // Binary-encoded structs support a special case where the fields are known to be sorted
        // such that the field-name integers are increasing. This state exists when L is one.
        // When L is 1, the struct has at least one symbol/value pair, the length field exists,
        // and the field name integers are sorted in increasing order.
        (TypeCode::Struct, LengthCode::L1) => {
            let (rest, length) = take_var_uint_as_usize(rest)?;
            let (end, rep) = take(length)(rest)?;
            let (end, value) = take(index.len() - end.len())(index)?;
            Ok((
                end,
                TypedValue {
                    type_code,
                    length_code,
                    index,
                    value,
                    rep,
                },
            ))
        }
        // Because L cannot be zero, the octet 0xE0 is not a valid type descriptor.
        // Instead, that octet signals the start of a binary version marker.
        // This particular invalid representation should be passed back up as an error
        // so that an alternative parse can be attempted.
        (TypeCode::Annotation, LengthCode::L0) => Err(Err::Error(IonError::from_format_error(
            index,
            FormatError::Binary(BinaryFormatError::AnnotationLength(length_code as u8)),
        ))),
        // Because at least one annotation and exactly one content field must exist,
        // L is at least 3 and is never 15.
        (TypeCode::Annotation, LengthCode::L1)
        | (TypeCode::Annotation, LengthCode::L2)
        | (TypeCode::Annotation, LengthCode::L15) => {
            Err(Err::Failure(IonError::from_format_error(
                index,
                FormatError::Binary(BinaryFormatError::AnnotationLength(length_code as u8)),
            )))
        }
        (TypeCode::Reserved, _) => Err(Err::Failure(IonError::from_format_error(
            index,
            FormatError::Binary(BinaryFormatError::ReservedTypeCode),
        ))),
        // All remaining type_codes behave in a standard way
        (type_code, length_code) => {
            let (rest, length) = take_representation_length(length_code)(rest)?;
            let (end, rep) = take(length)(rest)?;
            let (end, value) = take(index.len() - end.len())(index)?;
            Ok((
                end,
                TypedValue {
                    type_code,
                    length_code,
                    index,
                    value,
                    rep,
                },
            ))
        }
    }
}

fn take_representation_length(
    length_code: LengthCode,
) -> impl Fn(&[u8]) -> IonResult<&[u8], usize> {
    move |input: &[u8]| {
        let length: usize = length_code as usize;
        let (rest, length) = match length {
            0..=13 => (input, length),
            14 => take_var_uint_as_usize(input)?,
            _ => (input, 0),
        };
        Ok((rest, length))
    }
}

/// ### 0: null
///
/// ```text
///             7       4 3       0
///            +---------+---------+
/// Null value |    0    |    15   |
///            +---------+---------+
/// ```
///
/// Values of type null always have empty lengths and representations.
/// The only valid L value is 15, representing the only value of this type, null.null.
///
/// NOP Padding
///
/// ```text
///          7       4 3       0
///         +---------+---------+
/// NOP Pad |    0    |    L    |
///         +---------+---------+======+
///         :     length [VarUInt]     :
///         +--------------------------+
///         |      ignored octets      |
///         +--------------------------+
/// ```
///
/// In addition to null.null, the null type code is used to encode padding that has no operation
/// (NOP padding). This can be used for “binary whitespace” when alignment of octet boundaries is
/// needed or to support in-place editing. Such encodings are not considered values and are ignored
/// by the processor.
///
/// In this encoding, L specifies the number of octets that should be ignored.
///
/// The following is a single byte NOP pad. The NOP padding typedesc bytes are counted as padding:
///
/// 0x00
///
/// The following is a two byte NOP pad:
///
/// 0x01 0xFE
///
/// Note that the single byte of “payload” 0xFE is arbitrary and ignored by the parser.
///
/// The following is a 16 byte NOP pad:
///
/// 0x0E 0x8E 0x00 ... <12 arbitrary octets> ... 0x00
///
/// NOP padding is valid anywhere a value can be encoded, except for within an annotation wrapper.
/// NOP padding in struct requires additional encoding considerations.
fn parse_null(typed_value: TypedValue) -> Option<()> {
    match typed_value.length_code {
        LengthCode::L15 => Some(()),
        // Anything else is a NOP pad
        _ => None,
    }
}

/// ### 1: bool
///
/// ```text
///             7       4 3       0
///            +---------+---------+
/// Bool value |    1    |   rep   |
///            +---------+---------+
/// ```
///
/// Values of type bool always have empty lengths, and their representation is stored in the typedesc
/// itself (rather than after the typedesc). A representation of 0 means false; a representation of 1
/// means true; and a representation of 15 means null.bool.
fn parse_bool(typed_value: TypedValue) -> ParseResult<&[u8], Option<bool>> {
    match typed_value.length_code {
        LengthCode::L0 => Ok(Some(false)),
        LengthCode::L1 => Ok(Some(true)),
        LengthCode::L15 => Ok(None),
        catch_invalid => Err(Err::Failure(IonError::from_format_error(
            typed_value.index,
            FormatError::Binary(BinaryFormatError::BoolValue(catch_invalid as u8)),
        ))),
    }
}

/// ### 2: positive int
///
/// Values of type int are stored using two type codes: 2 for positive values and 3 for negative values.
/// Both codes use a UInt subfield to store the magnitude.
///
/// ```text
///            7       4 3       0
///           +---------+---------+
/// Int value |  2 or 3 |    L    |
///           +---------+---------+======+
///           :     length [VarUInt]     :
///           +==========================+
///           :     magnitude [UInt]     :
///           +==========================+
/// ```
///
/// Zero is always stored as positive; negative zero is illegal.
///
/// If the value is zero then T must be 2, L is zero, and there are no length or magnitude subfields.
/// As a result, when T is 3, both L and the magnitude subfield must be non-zero.
///
/// With either type code 2 or 3, if L is 15, then the value is null.int and the magnitude is empty.
/// Note that this implies there are two equivalent binary representations of null integer values.
fn parse_positive_int(typed_value: TypedValue) -> ParseResult<&[u8], Option<BigInt>> {
    match typed_value.length_code {
        LengthCode::L15 => Ok(None),
        LengthCode::L0 => Ok(Some(BigInt::zero())),
        _ => {
            let magnitude = BigUint::from_bytes_be(typed_value.rep);
            Ok(Some(BigInt::from_biguint(Sign::Plus, magnitude)))
        }
    }
}

/// ### 3: negative int
///
/// Values of type int are stored using two type codes: 2 for positive values and 3 for negative values.
/// Both codes use a UInt subfield to store the magnitude.
///
/// ```text
///            7       4 3       0
///           +---------+---------+
/// Int value |  2 or 3 |    L    |
///           +---------+---------+======+
///           :     length [VarUInt]     :
///           +==========================+
///           :     magnitude [UInt]     :
///           +==========================+
/// ```
///
/// Zero is always stored as positive; negative zero is illegal.
///
/// If the value is zero then T must be 2, L is zero, and there are no length or magnitude subfields.
/// As a result, when T is 3, both L and the magnitude subfield must be non-zero.
///
/// With either type code 2 or 3, if L is 15, then the value is null.int and the magnitude is empty.
/// Note that this implies there are two equivalent binary representations of null integer values.
fn parse_negative_int(typed_value: TypedValue) -> ParseResult<&[u8], Option<BigInt>> {
    match typed_value.length_code {
        LengthCode::L15 => Ok(None),
        LengthCode::L0 => Ok(Some(BigInt::zero())),
        _ => {
            let magnitude = BigUint::from_bytes_be(typed_value.rep);
            if magnitude == BigUint::zero() {
                return Err(Err::Failure(IonError::from_format_error(
                    typed_value.index,
                    FormatError::Binary(BinaryFormatError::NegativeZero),
                )));
            }
            Ok(Some(BigInt::from_biguint(Sign::Minus, magnitude)))
        }
    }
}

/// ### 4: float
///
/// ```text
///               7       4 3       0
///             +---------+---------+
/// Float value |    4    |    L    |
///             +---------+---------+-----------+
///             |   representation [IEEE-754]   |
///             +-------------------------------+
/// ```
///
/// Floats are encoded as big endian octets of their IEEE 754 bit patterns.
///
/// The L field of floats encodes the size of the IEEE-754 value.
///
/// If L is 4, then the representation is 32 bits (4 octets).
/// If L is 8, then the representation is 64 bits (8 octets).
/// There are two exceptions for the L field:
///
/// If L is 0, then the the value is 0e0 and representation is empty.
/// Note, this is not to be confused with -0e0 which is a distinct value and in current Ion must be
/// encoded as a normal IEEE float bit pattern.
/// If L is 15, then the value is null.float and the representation is empty.
/// Note: Ion 1.0 only supports 32-bit and 64-bit float values (i.e. L size 4 or 8), but future versions
/// of the standard may support 16-bit and 128-bit float values.
fn parse_float(typed_value: TypedValue) -> ParseResult<&[u8], Option<f64>> {
    match typed_value.length_code {
        LengthCode::L0 => Ok(Some(0e0)),
        LengthCode::L4 => {
            let (_, value) = float(typed_value.rep)?;
            Ok(Some(f64::from(value)))
        }
        LengthCode::L8 => {
            let (_, value) = double(typed_value.rep)?;
            Ok(Some(value))
        }
        LengthCode::L15 => Ok(None),
        catch_invalid => Err(Err::Failure(IonError::from_format_error(
            typed_value.index,
            FormatError::Binary(BinaryFormatError::FloatLength(catch_invalid as u8)),
        ))),
    }
}

/// ### 5: decimal
///
/// ```text
///                7       4 3       0
///               +---------+---------+
/// Decimal value |    5    |    L    |
///               +---------+---------+======+
///               :     length [VarUInt]     :
///               +--------------------------+
///               |    exponent [VarInt]     |
///               +--------------------------+
///               |    coefficient [Int]     |
///               +--------------------------+
/// ```
///
/// Decimal representations have two components: exponent (a VarInt) and coefficient (an Int).
/// The decimal’s value is coefficient * 10 ^ exponent.
///
/// The length of the coefficient subfield is the total length of the representation minus the length of
/// exponent. The subfield should not be present (that is, it has zero length) when the coefficient’s
/// value is (positive) zero.
///
/// If the value is 0. (aka 0d0) then L is zero, there are no length or representation fields, and the
/// entire value is encoded as the single byte 0x50.
fn parse_decimal(typed_value: TypedValue) -> ParseResult<&[u8], Option<Decimal>> {
    match typed_value.length_code {
        LengthCode::L15 => Ok(None),
        LengthCode::L0 => Ok(Some(Decimal {
            coefficient: BigInt::zero(),
            exponent: BigInt::zero(),
        })),
        _ => {
            let (coefficient_index, exponent) = take_var_int(typed_value.rep)?;
            let (_, coefficient) = match coefficient_index.len() {
                0 => (coefficient_index, BigInt::zero()),
                remaining_bytes => take_int(remaining_bytes)(coefficient_index)?,
            };
            Ok(Some(Decimal {
                exponent,
                coefficient,
            }))
        }
    }
}

/// ### 6: timestamp
///
/// ```text
///                  7       4 3       0
///                 +---------+---------+
/// Timestamp value |    6    |    L    |
///                 +---------+---------+========+
///                 :      length [VarUInt]      :
///                 +----------------------------+
///                 |      offset [VarInt]       |
///                 +----------------------------+
///                 |       year [VarUInt]       |
///                 +----------------------------+
///                 :       month [VarUInt]      :
///                 +============================+
///                 :         day [VarUInt]      :
///                 +============================+
///                 :        hour [VarUInt]      :
///                 +====                    ====+
///                 :      minute [VarUInt]      :
///                 +============================+
///                 :      second [VarUInt]      :
///                 +============================+
///                 : fraction_exponent [VarInt] :
///                 +============================+
///                 : fraction_coefficient [Int] :
///                 +============================+
/// ```
///
/// Timestamp representations have 7 components, where 5 of these components are optional depending on
/// the precision of the timestamp. The 2 non-optional components are offset and year.
/// The 5 optional components are (from least precise to most precise): month, day, hour and minute,
/// second, fraction_exponent and fraction_coefficient.
/// All of these 7 components are in Universal Coordinated Time (UTC).
///
/// The offset denotes the local-offset portion of the timestamp, in minutes difference from UTC.
///
/// The hour and minute is considered as a single component, that is, it is illegal to have hour but
/// not minute (and vice versa).
///
/// The fraction_exponent and fraction_coefficient denote the fractional seconds of the timestamp as a
/// decimal value. The fractional seconds’ value is coefficient * 10 ^ exponent.
/// It must be greater than or equal to zero and less than 1. A missing coefficient defaults to zero.
/// Fractions whose coefficient is zero and exponent is greater than -1 are ignored.
/// The following hex encoded timestamps are equivalent:
///
/// 68 80 0F D0 81 81 80 80 80       ///  2000-01-01T00:00:00Z with no fractional seconds
/// 69 80 0F D0 81 81 80 80 80 80    ///  The same instant with 0d0 fractional seconds and implicit zero coefficient
/// 6A 80 0F D0 81 81 80 80 80 80 00 ///  The same instant with 0d0 fractional seconds and explicit zero coefficient
/// 69 80 0F D0 81 81 80 80 80 C0    ///  The same instant with 0d-0 fractional seconds
/// 69 80 0F D0 81 81 80 80 80 81    ///  The same instant with 0d1 fractional seconds
/// Conversely, none of the following are equivalent:
///
/// 68 80 0F D0 81 81 80 80 80       ///  2000-01-01T00:00:00Z with no fractional seconds
/// 69 80 0F D0 81 81 80 80 80 C1    ///  2000-01-01T00:00:00.0Z
/// 69 80 0F D0 81 81 80 80 80 C2    ///  2000-01-01T00:00:00.00Z
/// If a timestamp representation has a component of a certain precision, each of the less precise
/// components must also be present or else the representation is illegal.
/// For example, a timestamp representation that has a fraction_exponent and fraction_coefficient component but not the month component, is illegal.
///
/// Note: The component values in the binary encoding are always in UTC, while components in the
/// text encoding are in the local time! This means that transcoding requires a conversion between
/// UTC and local time.
fn parse_timestamp(typed_value: TypedValue) -> ParseResult<&[u8], Option<Timestamp>> {
    match typed_value.length_code {
        LengthCode::L15 => Ok(None),
        _ => {
            let (rest, offset) = take_var_int(typed_value.rep)?;
            let offset = match offset.to_i32() {
                Some(offset) => offset,
                None => {
                    return Err(Err::Failure(IonError::from_format_error(
                        typed_value.index,
                        FormatError::Binary(BinaryFormatError::TimeComponentRange(
                            TimeComponent::Offset,
                            offset,
                        )),
                    )))
                }
            };

            let (rest, year) = take_var_uint(rest)?;
            let year = match year.to_u16() {
                Some(year) => year,
                None => {
                    return Err(Err::Failure(IonError::from_format_error(
                        typed_value.index,
                        FormatError::Binary(BinaryFormatError::TimeComponentRange(
                            TimeComponent::Year,
                            year.to_bigint().unwrap(),
                        )),
                    )))
                }
            };

            // Parsing complete with precision of Year
            if rest.is_empty() {
                return Ok(Some(Timestamp::Year { offset, year }));
            }

            let (rest, month) = take_var_uint(rest)?;
            let month = match month.to_u8() {
                Some(month) => month,
                None => {
                    return Err(Err::Failure(IonError::from_format_error(
                        typed_value.index,
                        FormatError::Binary(BinaryFormatError::TimeComponentRange(
                            TimeComponent::Month,
                            month.to_bigint().unwrap(),
                        )),
                    )))
                }
            };

            // Parsing complete with precision of Month
            if rest.is_empty() {
                return Ok(Some(Timestamp::Month {
                    offset,
                    year,
                    month,
                }));
            }

            let (rest, day) = take_var_uint(rest)?;
            let day = match day.to_u8() {
                Some(day) => day,
                None => {
                    return Err(Err::Failure(IonError::from_format_error(
                        typed_value.index,
                        FormatError::Binary(BinaryFormatError::TimeComponentRange(
                            TimeComponent::Day,
                            day.to_bigint().unwrap(),
                        )),
                    )))
                }
            };

            // Parsing complete with precision of Day
            if rest.is_empty() {
                return Ok(Some(Timestamp::Day {
                    offset,
                    year,
                    month,
                    day,
                }));
            }

            let (rest, hour) = take_var_uint(rest)?;
            let hour = match hour.to_u8() {
                Some(hour) => hour,
                None => {
                    return Err(Err::Failure(IonError::from_format_error(
                        typed_value.index,
                        FormatError::Binary(BinaryFormatError::TimeComponentRange(
                            TimeComponent::Hour,
                            hour.to_bigint().unwrap(),
                        )),
                    )))
                }
            };
            let (rest, minute) = take_var_uint(rest)?;
            let minute = match minute.to_u8() {
                Some(minute) => minute,
                None => {
                    return Err(Err::Failure(IonError::from_format_error(
                        typed_value.index,
                        FormatError::Binary(BinaryFormatError::TimeComponentRange(
                            TimeComponent::Minute,
                            minute.to_bigint().unwrap(),
                        )),
                    )))
                }
            };

            // Parsing complete with precision of Minute
            if rest.is_empty() {
                return Ok(Some(Timestamp::Minute {
                    offset,
                    year,
                    month,
                    day,
                    hour,
                    minute,
                }));
            }

            let (rest, second) = take_var_uint(rest)?;
            let second = match second.to_u8() {
                Some(second) => second,
                None => {
                    return Err(Err::Failure(IonError::from_format_error(
                        typed_value.index,
                        FormatError::Binary(BinaryFormatError::TimeComponentRange(
                            TimeComponent::Second,
                            second.to_bigint().unwrap(),
                        )),
                    )))
                }
            };

            // Parsing complete with precision of Second
            if rest.is_empty() {
                return Ok(Some(Timestamp::Second {
                    offset,
                    year,
                    month,
                    day,
                    hour,
                    minute,
                    second,
                }));
            }

            let (rest, fraction_exponent) = take_var_int_as_i32(rest)?;
            let fraction_coefficient = if rest.is_empty() {
                // A missing coefficient defaults to zero.
                BigInt::zero()
            } else {
                parse_int(rest)
            };

            // The fractional seconds’ value is (coefficient * 10 ^ exponent).
            // It must be greater than or equal to zero and (FIXME: less than 1).
            let fraction_coefficient = match fraction_coefficient.to_biguint() {
                Some(value) => value,
                None => {
                    return Err(Err::Failure(IonError::from_error_kind(
                        typed_value.index,
                        ErrorKind::Verify,
                    )))
                }
            };

            // Fractions whose coefficient is zero and exponent is greater than -1 are ignored.
            if fraction_coefficient.is_zero() && fraction_exponent > -1 {
                return Ok(Some(Timestamp::Second {
                    offset,
                    year,
                    month,
                    day,
                    hour,
                    minute,
                    second,
                }));
            }

            // Parsing complete with precision of FractionalSecond
            Ok(Some(Timestamp::FractionalSecond {
                offset,
                year,
                month,
                day,
                hour,
                minute,
                second,
                fraction_coefficient,
                fraction_exponent,
            }))
        }
    }
}

/// ### 7: symbol
///
/// ```text
///               7       4 3       0
///              +---------+---------+
/// Symbol value |    7    |    L    |
///              +---------+---------+======+
///              :     length [VarUInt]     :
///              +--------------------------+
///              |     symbol ID [UInt]     |
///              +--------------------------+
/// ```
///
/// In the binary encoding, all Ion symbols are stored as integer symbol IDs whose text values are
/// provided by a symbol table. If L is zero then the symbol ID is zero and the length and symbol ID
/// fields are omitted.
///
/// See Ion Symbols for more details about symbol representations and symbol tables.
fn parse_symbol<'a>(
    typed_value: TypedValue<'a>,
    symbol_table: &CurrentSymbolTable,
) -> ParseResult<&'a [u8], Option<SymbolToken>> {
    match typed_value.length_code {
        LengthCode::L15 => Ok(None),
        LengthCode::L0 => Ok(Some(SymbolToken::Zero)),
        _ => {
            let symbol_id = parse_uint(typed_value.rep);
            // FIXME: This constraint on symbol_id size should probably be made uniform
            let symbol_id = symbol_id.to_usize().ok_or_else(|| {
                Err::Failure(IonError::from_error_kind(
                    typed_value.index,
                    ErrorKind::LengthValue,
                ))
            })?;
            let token = match symbol_table.lookup_sid(symbol_id) {
                Ok(token) => token,
                Err(err) => {
                    return Err(Err::Failure(IonError::from_symbol_error(
                        typed_value.index,
                        err,
                    )))
                }
            };
            Ok(Some(token))
        }
    }
}

/// ### 8: string
///
/// ```text
///               7       4 3       0
///              +---------+---------+
/// String value |    8    |    L    |
///              +---------+---------+======+
///              :     length [VarUInt]     :
///              +==========================+
///              :  representation [UTF8]   :
///              +==========================+
/// ```
///
/// These are always sequences of Unicode characters, encoded as a sequence of UTF-8 octets.
fn parse_string(typed_value: TypedValue) -> ParseResult<&[u8], Option<String>> {
    match typed_value.length_code {
        LengthCode::L15 => Ok(None),
        _ => {
            let representation = match std::str::from_utf8(typed_value.rep) {
                Ok(value) => value,
                Err(err) => {
                    return Err(Err::Failure(IonError::from_format_error(
                        typed_value.index,
                        FormatError::Binary(BinaryFormatError::StringEncoding),
                    )))
                }
            };
            Ok(Some(String::from(representation)))
        }
    }
}

/// ### 9: clob
///
/// ```text
///             7       4 3       0
///            +---------+---------+
/// Clob value |    9    |    L    |
///            +---------+---------+======+
///            :     length [VarUInt]     :
///            +==========================+
///            :       data [Bytes]       :
///            +==========================+
/// ```
///
/// Values of type clob are encoded as a sequence of octets that should be interpreted as text with
/// an unknown encoding (and thus opaque to the application).
///
/// Zero-length clobs are legal, so L may be zero.
fn parse_clob(typed_value: TypedValue) -> ParseResult<&[u8], Option<Clob>> {
    match typed_value.length_code {
        LengthCode::L15 => Ok(None),
        _ => Ok(Some(Clob {
            data: typed_value.rep.to_vec(),
        })),
    }
}

/// ### 10: blob
///
/// ```text
///             7       4 3       0
///            +---------+---------+
/// Blob value |   10    |    L    |
///            +---------+---------+======+
///            :     length [VarUInt]     :
///            +==========================+
///            :       data [Bytes]       :
///            +==========================+
/// ```
///
/// This is a sequence of octets with no interpretation (and thus opaque to the application).
///
/// Zero-length blobs are legal, so L may be zero.
fn parse_blob(typed_value: TypedValue) -> ParseResult<&[u8], Option<Blob>> {
    match typed_value.length_code {
        LengthCode::L15 => Ok(None),
        _ => Ok(Some(Blob {
            data: typed_value.rep.to_vec(),
        })),
    }
}

/// ### 11: list
///
/// ```text
///             7       4 3       0
///            +---------+---------+
/// List value |   11    |    L    |
///            +---------+---------+======+
///            :     length [VarUInt]     :
///            +==========================+
///            :           value          :
///            +==========================+
///                          ⋮
/// ```
///
/// The representation fields of a list value are simply nested Ion values.
///
/// When L is 15, the value is null.list and there’s no length or nested values. When L is 0,
/// the value is an empty list, and there’s no length or nested values.
///
/// Because values indicate their total lengths in octets, it is possible to locate the beginning of
/// each successive value in constant time.
fn parse_list<'a>(
    typed_value: TypedValue<'a>,
    symbol_table: &CurrentSymbolTable,
) -> ParseResult<&'a [u8], Option<List>> {
    match typed_value.length_code {
        LengthCode::L15 => Ok(None),
        LengthCode::L0 => Ok(Some(List { values: vec![] })),
        _ => {
            let (_, values) =
                complete(all_consuming(many0(take_value(symbol_table))))(typed_value.rep)?;
            let values = values
                .into_iter()
                .filter_map(|value| match value {
                    None => None,
                    Some(value) => Some(value),
                })
                .collect();
            Ok(Some(List { values }))
        }
    }
}

/// ### 12: sexp
///
/// ```text
///             7       4 3       0
///            +---------+---------+
/// Sexp value |   12    |    L    |
///            +---------+---------+======+
///            :     length [VarUInt]     :
///            +==========================+
///            :           value          :
///            +==========================+
///                          ⋮
/// ```
///
/// Values of type sexp are encoded exactly as are list values, except with a different type code.
fn parse_sexp<'a>(
    typed_value: TypedValue<'a>,
    symbol_table: &CurrentSymbolTable,
) -> ParseResult<&'a [u8], Option<Sexp>> {
    match typed_value.length_code {
        LengthCode::L15 => Ok(None),
        LengthCode::L0 => Ok(Some(Sexp { values: vec![] })),
        _ => {
            let (_, values) =
                complete(all_consuming(many0(take_value(symbol_table))))(typed_value.rep)?;
            let values = values
                .into_iter()
                .filter_map(|value| match value {
                    None => None,
                    Some(value) => Some(value),
                })
                .collect();
            Ok(Some(Sexp { values }))
        }
    }
}

/// ### 13: struct
///
/// Structs are encoded as sequences of symbol/value pairs. Since all symbols are encoded as positive
/// integers, we can omit the typedesc on the field names and just encode the integer value.
///
/// ```text
///               7       4 3       0
///              +---------+---------+
/// Struct value |   13    |    L    |
///              +---------+---------+======+
///              :     length [VarUInt]     :
///              +======================+===+==================+
///              : field name [VarUInt] :        value         :
///              +======================+======================+
///                          ⋮                     ⋮
/// ```
///
/// Binary-encoded structs support a special case where the fields are known to be sorted
/// such that the field-name integers are increasing. This state exists when L is one. Thus:
///
/// When L is 0, the value is an empty struct, and there’s no length or nested fields.
/// When L is 1, the struct has at least one symbol/value pair, the length field exists,
/// and the field name integers are sorted in increasing order.
/// When L is 15, the value is null.struct, and there’s no length or nested fields.
/// When 1 < L < 14 then there is no length field as L is enough to represent the struct size,
/// and no assertion is made about field ordering.
/// Otherwise, the length field exists, and no assertion is made about field ordering.
/// Note: Because VarUInts depend on end tags to indicate their lengths,
/// finding the succeeding value requires parsing the field name prefix.
/// However, VarUInts are a more compact representation than Int values.
///
///
/// NOP Padding in struct Fields
/// NOP Padding in struct values requires additional consideration of the field name element.
/// If the “value” of a struct field is the NOP pad, then the field name is ignored.
/// This means that it is not possible to encode padding in a struct value that is less than two bytes.
///
/// Implementations should use symbol ID zero as the field name to emphasize the lack of meaning of
/// the field name.
/// For more general details about the semantics of symbol ID zero, refer to Ion Symbols.
///
/// For example, consider the following empty struct with three bytes of padding:
///
/// 0xD3 0x80 0x01 0xAC
/// In the above example, the struct declares that it is three bytes large, and the encoding of the
/// pair of symbol ID zero followed by a pad that is two bytes large
/// (note the last octet 0xAC is completely arbitrary and never interpreted by an implementation).
///
/// The following is an example of struct with a single field with four total bytes of padding:
///
/// 0xD7 0x84 0x81 "a" 0x80 0x02 0x01 0x02
/// The above is equivalent to {name:"a"}.
///
/// The following is also a empty struct, with a two byte pad:
///
/// 0xD2 0x8F 0x00
/// In the above example, the field name of symbol ID 15 is ignored
/// (regardless of if it is a valid symbol ID).
///
/// The following is malformed because there is an annotation “wrapping” a NOP pad,
/// which is not allowed generally for annotations:
///
/// //  {$0:name::<NOP>}
/// 0xD5 0x80 0xE3 0x81 0x84 0x00
fn parse_struct<'a>(
    typed_value: TypedValue<'a>,
    symbol_table: &CurrentSymbolTable,
) -> ParseResult<&'a [u8], Option<Struct>> {
    // Get all entries
    let (_, entries): (_, Vec<(usize, Option<Value>)>) = match typed_value.length_code {
        LengthCode::L15 => return Ok(None),
        LengthCode::L0 => return Ok(Some(Struct { fields: vec![] })),
        _ => parse_struct_entries(symbol_table)(typed_value.rep)?,
    };

    // Strip all of the entries with values containing padding
    let entries: Vec<(usize, Value)> = entries
        .into_iter()
        .filter_map(|(field_name, value)| match value {
            None => None,
            Some(value) => Some((field_name, value)),
        })
        .collect();

    // If the struct is a sorted struct, verify that it is actually sorted
    if typed_value.length_code == LengthCode::L1 {
        if entries.is_empty() {
            return Err(Err::Failure(IonError::from_format_error(
                typed_value.index,
                FormatError::Binary(BinaryFormatError::StructEmpty),
            )));
        }
        let sorted = entries
            .iter()
            .map(|x| x.0)
            .tuple_windows()
            .all(|(a, b)| a < b);
        if !sorted {
            return Err(Err::Failure(IonError::from_format_error(
                typed_value.index,
                FormatError::Binary(BinaryFormatError::StructUnordered),
            )));
        }
    }

    // Map each symbol_id to its associated symbol token
    let entries: ParseResult<&'a [u8], Vec<(SymbolToken, Value)>> = entries
        .into_iter()
        .map(|(field_name, value)| {
            let symbol = match symbol_table.lookup_sid(field_name) {
                Ok(token) => token,
                Err(err) => {
                    return Err(Err::Failure(IonError::from_symbol_error(
                        typed_value.index,
                        err,
                    )))
                }
            };
            Ok((symbol, value))
        })
        .collect();

    let values = entries?;
    Ok(Some(Struct { fields: values }))
}

fn parse_struct_entries(
    symbol_table: &CurrentSymbolTable,
) -> impl Fn(&[u8]) -> ParseResult<&[u8], (&[u8], Vec<(usize, Option<Value>)>)> + '_ {
    move |i: &[u8]| {
        complete(all_consuming(many0(pair(
            take_var_uint_as_usize,
            take_value(symbol_table),
        ))))(i)
    }
}

/// ### 14: Annotations
///
/// This special type code doesn’t map to an Ion value type,
/// but instead is a wrapper used to associate annotations with content.
///
/// Annotations are a special type that wrap content identified by the other type codes.
/// The annotations themselves are encoded as integer symbol ids.
///
/// ```text
///                     7       4 3       0
///                    +---------+---------+
/// Annotation wrapper |   14    |    L    |
///                    +---------+---------+======+
///                    :     length [VarUInt]     :
///                    +--------------------------+
///                    |  annot_length [VarUInt]  |
///                    +--------------------------+
///                    |      annot [VarUInt]     |  …
///                    +--------------------------+
///                    |          value           |
///                    +--------------------------+
/// ```
///
/// The length field L field indicates the length from the beginning of the annot_length field to the
/// end of the enclosed value. Because at least one annotation and exactly one content field must exist,
/// L is at least 3 and is never 15.
///
/// The annot_length field contains the length of the (one or more) annot fields.
///
/// It is illegal for an annotation to wrap another annotation atomically, i.e.,
/// annotation(annotation(value)).
///
/// Furthermore, it is illegal for an annotation to wrap a NOP Pad since this encoding is not an
/// Ion value. Thus, the following sequence is malformed:
///
/// The Ion specification notes that in the text format, annotations are denoted by a non-null symbol
/// token. Because the text and binary formats are semantically isomorphic, it follows that
/// a null symbol cannot appear as an annotation.
fn parse_annotation<'a>(
    typed_value: TypedValue<'a>,
    symbol_table: &CurrentSymbolTable,
) -> ParseResult<&'a [u8], Value> {
    match typed_value.length_code {
        LengthCode::L0 | LengthCode::L1 | LengthCode::L2 | LengthCode::L15 => {
            Err(Err::Failure(IonError::from_format_error(
                typed_value.index,
                FormatError::Binary(BinaryFormatError::AnnotationLength(
                    typed_value.length_code as u8,
                )),
            )))
        }
        _ => {
            let (rest, annot_length) = take_var_uint_as_usize(typed_value.rep)?;
            let (annot_bytes, value_bytes) = rest.split_at(annot_length);
            let (_, value) = all_consuming(take_typed_value)(value_bytes)?;
            // It is illegal for an annotation to wrap another annotation atomically.
            if value.type_code == TypeCode::Annotation {
                return Err(Err::Failure(IonError::from_format_error(
                    typed_value.index,
                    FormatError::Binary(BinaryFormatError::AnnotatedAnnotation),
                )));
            }
            let mut value = match parse_typed_value(value, symbol_table)? {
                // It is illegal for an annotation to wrap a NOP Pad since they are not Ion values.
                None => {
                    return Err(Err::Failure(IonError::from_format_error(
                        typed_value.index,
                        FormatError::Binary(BinaryFormatError::AnnotatedPadding),
                    )));
                }
                Some(value) => value,
            };
            let (_, annotations) = all_consuming(many1(take_var_uint_as_usize))(annot_bytes)?;
            match annotations
                .into_iter()
                .map(|x| match symbol_table.lookup_sid(x) {
                    Ok(token) => Ok(token),
                    Result::Err(error) => Err(error),
                })
                .collect()
            {
                Ok(annotations) => {
                    value.annotations = annotations;
                    Ok(value)
                }
                Result::Err(error) => Err(Err::Failure(IonError::from_symbol_error(
                    typed_value.index,
                    error,
                ))),
            }
        }
    }
}

/// ### 15: reserved
///
/// The remaining type code, 15, is reserved for future use and is not legal in Ion 1.0 data.
fn error_reserved(typed_value: TypedValue) -> Err<IonError<&[u8]>> {
    Err::Failure(IonError::from_format_error(
        typed_value.index,
        FormatError::Binary(BinaryFormatError::ReservedTypeCode),
    ))
}
