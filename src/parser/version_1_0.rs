use crate::ion_types::IonInteger::Integer;
use crate::ion_types::{
    IonBlob, IonBoolean, IonClob, IonDecimal, IonFloat, IonInteger, IonList, IonNull,
    IonSharedSymbolTable, IonString, IonStructure, IonSymbol, IonSymbolicExpression,
    IonSystemSymbolTable, IonTimestamp, IonValue,
};
use bit_vec::BitVec;
use nom::error::VerboseError;
use nom::lib::std::ops::Mul;
use nom::Err::Error;
use nom::{
    bytes::complete::{take, take_while},
    error::ErrorKind,
    number::complete::{double, float},
    sequence::{pair, tuple},
    Err, IResult,
};
use num_bigint::{BigInt, BigUint, Sign, ToBigInt};
use num_traits::cast::ToPrimitive;
use num_traits::identities::Zero;
use num_traits::real::Real;
use num_traits::{One, Signed};

const TYPE_DESCRIPTOR_BYTES: usize = 1;

const SYSTEM_SYMBOL_TABLE: IonSystemSymbolTable = IonSystemSymbolTable {
    name: &"$ion",
    version: 1,
    symbols: [
        "$0",
        "$ion",
        "$ion_1_0",
        "$ion_symbol_table",
        "name",
        "version",
        "imports",
        "symbols",
        "max_id",
        "$ion_shared_symbol_table",
    ],
};

/// Documentation draws extensively on http://amzn.github.io/ion-docs/docs/binary.html.

/// Parse a single IonValue from the head of an Ion byte stream
pub fn parse_value(i: &[u8]) -> IResult<&[u8], IonValue> {
    let descriptor = take_type_descriptor(i);

    match descriptor {
        Ok((rest, info)) => {
            match info.format {
                // null
                0 => parse_null(rest, info.length),
                // bool,
                1 => parse_bool(rest, info.length),
                // 2 positive int
                2 => parse_positive_int(rest, info.length),
                // 3 negative int
                3 => parse_negative_int(rest, info.length),
                // float
                4 => parse_float(rest, info.length),
                // decimal
                5 => parse_decimal(rest, info.length),
                // timestamp
                6 => parse_timestamp(rest, info.length),
                // symbol
                7 => parse_symbol(rest, info.length),
                // string
                8 => parse_string(rest, info.length),
                // clob
                9 => parse_clob(rest, info.length),
                // blob
                10 => parse_blob(rest, info.length),
                // list
                11 => parse_list(rest, info.length),
                // sexp
                12 => parse_sexp(rest, info.length),
                // struct
                13 => parse_struct(rest, info.length),
                // Annotations
                14 => parse_annotation(rest, info.length),
                // reserved
                15 => Err(Err::Failure((i, ErrorKind::LengthValue))),
                _ => Err(Err::Failure((i, ErrorKind::NoneOf))),
            }
        }
        Err(Err::Error(error)) => Err(Err::Error(error)),
        Err(Err::Failure(failure)) => Err(Err::Failure(failure)),
        Err(Err::Incomplete(needed)) => Err(Err::Incomplete(needed)),
    }
}

/// ## Basic Field Formats
///
/// ```text
/// Binary-encoded Ion values are comprised of one or more fields, and the fields use a small number
/// of basic formats (separate from the Ion types visible to users).
/// ```

/// ### UInt and Int Fields
///
/// ```text
/// UInt and Int fields represent fixed-length unsigned and signed integer values.
/// These field formats are always used in some context that clearly indicates the
/// number of octets in the field.
///
///             7                       0
///            +-------------------------+
/// UInt field |          bits           |
///            +-------------------------+
///            :          bits           :
///            +=========================+
///                        ⋮
///            +=========================+
///            :          bits           :
///            +=========================+
///             n+7                     n
///
/// UInts are sequences of octets, interpreted as big-endian.
///
///              7  6                   0
///            +---+---------------------+
/// Int field  |   |      bits           |
///            +---+---------------------+
///              ^
///              |
///              +--sign
///            +=========================+
///            :          bits           :
///            +=========================+
///                        ⋮
///            +=========================+
///            :          bits           :
///            +=========================+
///             n+7                     n
///
/// Ints are sequences of octets, interpreted as sign-and-magnitude big endian integers (with the sign
/// on the highest-order bit of the first octet). This means that the representations of
/// 123456 and -123456 should only differ in their sign bit.
/// ```

fn take_int(length: usize) -> impl Fn(&[u8]) -> IResult<&[u8], num_bigint::BigInt> {
    move |i: &[u8]| {
        let (rest, bytes) = take(length)(i)?;
        Ok((rest, parse_int(bytes)))
    }
}

pub fn parse_int(digits: &[u8]) -> num_bigint::BigInt {
    let sign = match digits.first() {
        Some(v) if *v > 0b0111_1111 => Sign::Minus,
        Some(_) => Sign::Plus,
        None => return BigInt::zero(),
    };

    if sign == Sign::Minus {
        let mut digits = Vec::from(digits);
        digits[0] ^= 0b1000_0000; // clear the high bit to get the magnitude
        BigInt::from_biguint(sign, BigUint::from_bytes_be(&*digits))
    } else {
        BigInt::from_biguint(sign, BigUint::from_bytes_be(digits))
    }
}

fn take_uint(length: usize) -> impl Fn(&[u8]) -> IResult<&[u8], num_bigint::BigUint> {
    move |i: &[u8]| {
        let (input, bytes) = take(length)(i)?;
        Ok((input, BigUint::from_bytes_be(bytes)))
    }
}

/// ## VarUInt and VarInt Fields
///
/// ```text
/// VarUInt and VarInt fields represent self-delimiting, variable-length unsigned and signed integer
/// values. These field formats are always used in a context that does not indicate the number of octets
/// in the field; the last octet (and only the last octet) has its high-order bit set to
/// terminate the field.
///
///                 7  6                   0       n+7 n+6                 n
///               +===+=====================+     +---+---------------------+
/// VarUInt field : 0 :         bits        :  …  | 1 |         bits        |
///               +===+=====================+     +---+---------------------+
///
/// VarUInts are a sequence of octets. The high-order bit of the last octet is one,
/// indicating the end of the sequence. All other high-order bits must be zero.
///
///                7   6  5               0       n+7 n+6                 n
///              +===+                           +---+
/// VarInt field : 0 :       payload          …  | 1 |       payload
///              +===+                           +---+
///                  +---+-----------------+         +=====================+
///                  |   |   magnitude     |  …      :       magnitude     :
///                  +---+-----------------+         +=====================+
///                ^   ^                           ^
///                |   |                           |
///                |   +--sign                     +--end flag
///                +--end flag
///
/// VarInts are sign-and-magnitude integers, like Ints. Their layout is complicated,
/// as there is one special leading bit (the sign) and one special trailing bit (the terminator).
/// In the above diagram, we put the two concepts on different layers.
///
/// The high-order bit in the top layer is an end-of-sequence marker. It must be set on the last octet
/// in the representation and clear in all other octets. The second-highest order bit (0x40) is a sign
/// flag in the first octet of the representation, but part of the extension bits for all other octets.
/// For single-octet VarInt values, this collapses down to:
///
///                             7   6  5           0
///                           +---+---+-------------+
/// single octet VarInt field | 1 |   |  magnitude  |
///                           +---+---+-------------+
///                                 ^
///                                 |
///                                 +--sign
/// ```

pub fn take_var_int(i: &[u8]) -> IResult<&[u8], num_bigint::BigInt> {
    let (input, sequence) = take_while(high_bit_unset)(i)?;
    let (input, terminator) = take(1usize)(input)?;
    Ok((input, parse_var_int(sequence, terminator)))
}

// There are scenarios (ex. timestamp exponent values) where allowing a VarInt that cannot fit in i32 is unreasonable.
// This should not pose an issue for non-pathological use cases.
// TODO: obvious room for performance improvement
fn take_var_int_as_i32(i: &[u8]) -> IResult<&[u8], i32> {
    let (rest, sequence) = take_while(high_bit_unset)(i)?;
    let (rest, terminator) = take(1usize)(rest)?;
    let value = parse_var_int(sequence, terminator);
    match value.to_i32() {
        Some(value) => Ok((rest, value)),
        None => Err(Err::Failure((rest, ErrorKind::TooLarge))),
    }
}

fn parse_var_int(sequence: &[u8], terminator: &[u8]) -> num_bigint::BigInt {
    debug_assert!(
        terminator.len() == 1,
        "VarInt terminator slice must contain exactly 1 byte, found {}!",
        terminator.len()
    );

    let sign = match sequence.first() {
        Some(byte) => {
            // we know that no byte in the sequence has the high bit set
            if *byte > 0b0011_1111 {
                Sign::Minus
            } else {
                Sign::Plus
            }
        }
        None => match terminator.first() {
            Some(byte) => {
                // we know that the terminator byte has the high bit set
                if *byte > 0b1011_1111 {
                    Sign::Minus
                } else {
                    Sign::Plus
                }
            }
            None => unreachable!("VarInt must contain at least 1 byte!"),
        },
    };

    // total number of payload bits in the VarInt
    let payload_bits = 7 * (sequence.len() + 1);

    // round payload_bits up to the nearest multiple of 8
    let bit_capacity = (payload_bits + 8 - 1) & (usize::max_value() << 3);

    let mut bits = BitVec::with_capacity(bit_capacity);

    let leading_bits = bit_capacity - payload_bits;

    // zero the extra leading bits
    for _ in 0..leading_bits {
        bits.push(false);
    }

    // insert all payload bits
    for byte in sequence.iter().chain(terminator) {
        bits.push((byte & 0b0100_0000) != 0);
        bits.push((byte & 0b0010_0000) != 0);
        bits.push((byte & 0b0001_0000) != 0);
        bits.push((byte & 0b0000_1000) != 0);
        bits.push((byte & 0b0000_0100) != 0);
        bits.push((byte & 0b0000_0010) != 0);
        bits.push((byte & 0b0000_0001) != 0);
    }

    // clear the sign bit in the first byte
    bits.set(leading_bits, false);

    BigInt::from_biguint(sign, BigUint::from_bytes_be(&*bits.to_bytes()))
}

pub fn take_var_uint(i: &[u8]) -> IResult<&[u8], num_bigint::BigUint> {
    let (input, sequence) = take_while(high_bit_unset)(i)?;
    let (input, terminator) = take(1usize)(input)?;
    Ok((input, parse_var_uint(sequence, terminator)))
}

// There are scenarios (ex.  byte-length tags) where allowing a VarUint that cannot fit in usize is unreasonable.
// TODO: obvious room for performance improvement
pub fn take_usize_var_uint(i: &[u8]) -> IResult<&[u8], usize> {
    let (rest, sequence) = take_while(high_bit_unset)(i)?;
    let (rest, terminator) = take(1usize)(rest)?;
    let value = parse_var_uint(sequence, terminator);
    match value.to_usize() {
        Some(value) => Ok((rest, value)),
        None => Err(Err::Failure((rest, ErrorKind::TooLarge))),
    }
}

fn parse_var_uint(sequence: &[u8], terminator: &[u8]) -> num_bigint::BigUint {
    debug_assert!(
        terminator.len() == 1,
        "VarUInt terminator slice must contain exactly 1 byte, found {}!",
        terminator.len()
    );

    // total number of payload bits in the VarUInt
    let payload_bits = 7 * (sequence.len() + 1);

    // round payload_bits up to the nearest multiple of 8
    let bit_capacity = (payload_bits + 8 - 1) & (usize::max_value() << 3);

    let mut bits = BitVec::with_capacity(bit_capacity);

    let leading_bits = bit_capacity - payload_bits;

    // zero the extra leading bits
    for _ in 0..leading_bits {
        bits.push(false);
    }

    // insert all payload bits
    for byte in sequence.iter().chain(terminator) {
        bits.push((byte & 0b0100_0000) != 0);
        bits.push((byte & 0b0010_0000) != 0);
        bits.push((byte & 0b0001_0000) != 0);
        bits.push((byte & 0b0000_1000) != 0);
        bits.push((byte & 0b0000_0100) != 0);
        bits.push((byte & 0b0000_0010) != 0);
        bits.push((byte & 0b0000_0001) != 0);
    }

    BigUint::from_bytes_be(&*bits.to_bytes())
}

fn high_bit_unset(byte: u8) -> bool {
    byte < 0b1000_0000
}

/// ## Typed Value Formats
///
/// ```text
/// A value consists of a one-octet type descriptor, possibly followed by a length in octets,
/// possibly followed by a representation.
///
///        7       4 3       0
///       +---------+---------+
/// value |    T    |    L    |
///       +---------+---------+======+
///       :     length [VarUInt]     :
///       +==========================+
///       :      representation      :
///       +==========================+
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
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct TypeDescriptor {
    format: usize,
    length: usize,
}

#[derive(Clone, Debug, PartialEq)]
struct TypedValue<'a> {
    format: u8,
    representation: Option<&'a [u8]>, // None for null
}

pub fn take_type_descriptor(input: &[u8]) -> IResult<&[u8], TypeDescriptor> {
    let (input, descriptor) = take(1usize)(input)?;
    let format: usize = (descriptor[0] >> 4) as usize;
    let length: usize = (descriptor[0] & 0b0000_1111) as usize;
    Ok((input, TypeDescriptor { format, length }))
}

/// ### 0: null
///
/// ```text
///             7       4 3       0
///            +---------+---------+
/// Null value |    0    |    15   |
///            +---------+---------+
/// Values of type null always have empty lengths and representations.
/// The only valid L value is 15, representing the only value of this type, null.null.
///
/// NOP Padding
///          7       4 3       0
///         +---------+---------+
/// NOP Pad |    0    |    L    |
///         +---------+---------+======+
///         :     length [VarUInt]     :
///         +--------------------------+
///         |      ignored octets      |
///         +--------------------------+
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
/// The following is a two byte NOP pad:
///
/// 0x01 0xFE
/// Note that the single byte of “payload” 0xFE is arbitrary and ignored by the parser.
///
/// The following is a 16 byte NOP pad:
///
/// 0x0E 0x8E 0x00 ... <12 arbitrary octets> ... 0x00
/// NOP padding is valid anywhere a value can be encoded, except for within an annotation wrapper.
/// NOP padding in struct requires additional encoding considerations.
/// ``
pub fn parse_null(input: &[u8], length: usize) -> IResult<&[u8], IonValue> {
    match length {
        0..=13 => Ok((&input[length..], IonValue::IonNull(IonNull::Pad))),
        14 => match take_usize_var_uint(input) {
            Ok((rest, length)) => Ok((&rest[length..], IonValue::IonNull(IonNull::Pad))),
            Err(e) => Err(e),
        },
        15 => Ok((input, IonValue::IonNull(IonNull::Null))),
        _ => Err(Err::Failure((input, ErrorKind::NoneOf))),
    }
}

/// ### 1: bool
///
/// ```text
///             7       4 3       0
///            +---------+---------+
/// Bool value |    1    |   rep   |
///            +---------+---------+
/// Values of type bool always have empty lengths, and their representation is stored in the typedesc
/// itself (rather than after the typedesc). A representation of 0 means false; a representation of 1
/// means true; and a representation of 15 means null.bool.
/// ```
pub fn parse_bool(input: &[u8], length: usize) -> IResult<&[u8], IonValue> {
    match length {
        0 => Ok((input, IonValue::IonBoolean(IonBoolean::False))),
        1 => Ok((input, IonValue::IonBoolean(IonBoolean::True))),
        15 => Ok((input, IonValue::IonBoolean(IonBoolean::Null))),
        _ => Err(Err::Failure((input, ErrorKind::NoneOf))),
    }
}

/// ### 2: positive int
///
/// ```text
/// Values of type int are stored using two type codes: 2 for positive values and 3 for negative values.
/// Both codes use a UInt subfield to store the magnitude.
///
///            7       4 3       0
///           +---------+---------+
/// Int value |  2 or 3 |    L    |
///           +---------+---------+======+
///           :     length [VarUInt]     :
///           +==========================+
///           :     magnitude [UInt]     :
///           +==========================+
/// Zero is always stored as positive; negative zero is illegal.
///
/// If the value is zero then T must be 2, L is zero, and there are no length or magnitude subfields.
/// As a result, when T is 3, both L and the magnitude subfield must be non-zero.
///
/// With either type code 2 or 3, if L is 15, then the value is null.int and the magnitude is empty.
/// Note that this implies there are two equivalent binary representations of null integer values.
/// ```
pub fn parse_positive_int(input: &[u8], length: usize) -> IResult<&[u8], IonValue> {
    match length {
        0..=13 => {
            let (rest, magnitude) = take_uint(length)(input).expect("Cannot take UInt");
            Ok((
                rest,
                IonValue::IonInteger(IonInteger::Integer {
                    value: magnitude.to_bigint().expect("Cannot convert to BigInt"),
                }),
            ))
        }
        14 => match take_usize_var_uint(input) {
            Ok((rest, length)) => {
                let (rest, magnitude) = take_uint(length)(rest).expect("Cannot take UInt");
                Ok((
                    rest,
                    IonValue::IonInteger(IonInteger::Integer {
                        value: magnitude.to_bigint().expect("Cannot convert to BigInt"),
                    }),
                ))
            }
            Err(e) => Err(e),
        },
        15 => Ok((input, IonValue::IonInteger(IonInteger::Null))),
        _ => Err(Err::Failure((input, ErrorKind::NoneOf))),
    }
}

/// ### 3: negative int
///
/// ```text
/// Values of type int are stored using two type codes: 2 for positive values and 3 for negative values.
/// Both codes use a UInt subfield to store the magnitude.
///
///            7       4 3       0
///           +---------+---------+
/// Int value |  2 or 3 |    L    |
///           +---------+---------+======+
///           :     length [VarUInt]     :
///           +==========================+
///           :     magnitude [UInt]     :
///           +==========================+
/// Zero is always stored as positive; negative zero is illegal.
///
/// If the value is zero then T must be 2, L is zero, and there are no length or magnitude subfields.
/// As a result, when T is 3, both L and the magnitude subfield must be non-zero.
///
/// With either type code 2 or 3, if L is 15, then the value is null.int and the magnitude is empty.
/// Note that this implies there are two equivalent binary representations of null integer values.
/// ```
pub fn parse_negative_int(input: &[u8], length: usize) -> IResult<&[u8], IonValue> {
    let (rest, value) = parse_positive_int(input, length).expect("cannot parse");
    match value {
        IonValue::IonInteger(value) => match value {
            IonInteger::Null => Ok((rest, IonValue::IonInteger(IonInteger::Null))),
            IonInteger::Integer { value } => Ok((
                rest,
                IonValue::IonInteger(IonInteger::Integer { value: -1 * value }),
            )),
        },
        _ => Err(Err::Failure((input, ErrorKind::IsNot))),
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
/// ```
pub fn parse_float(input: &[u8], length: usize) -> IResult<&[u8], IonValue> {
    match length {
        0 => Ok((input, IonValue::IonFloat(IonFloat::Float { value: 0e0 }))),
        4 => {
            let (rest, value) = float(input)?;
            Ok((
                rest,
                IonValue::IonFloat(IonFloat::Float {
                    value: f64::from(value),
                }),
            ))
        }
        8 => {
            let (rest, value) = double(input)?;
            Ok((rest, IonValue::IonFloat(IonFloat::Float { value })))
        }
        15 => Ok((input, IonValue::IonFloat(IonFloat::Null))),
        _ => Err(Err::Failure((input, ErrorKind::NoneOf))),
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
/// ```
pub fn parse_decimal(input: &[u8], length: usize) -> IResult<&[u8], IonValue> {
    match length {
        0 => Ok((
            input,
            IonValue::IonDecimal(IonDecimal::Decimal {
                coefficient: BigInt::zero(),
                exponent: BigInt::zero(),
            }),
        )),
        1..=13 => {
            let (rest, exponent) = take_var_int(input)?;
            let (rest, coefficient) = match length - (input.len() - rest.len()) {
                0 => (rest, BigInt::zero()),
                residual_length => take_int(residual_length)(rest)?,
            };
            Ok((
                rest,
                IonValue::IonDecimal(IonDecimal::Decimal {
                    exponent,
                    coefficient,
                }),
            ))
        }
        14 => match take_usize_var_uint(input) {
            Ok((after_length, length)) => {
                let (after_exponent, exponent) = take_var_int(after_length)?;
                let exponent_length = after_length.len() - after_exponent.len();
                if exponent_length > length {
                    return Err(Err::Failure((after_length, ErrorKind::TooLarge)));
                }
                let residual_length = length - exponent_length;
                let (rest, coefficient) = match residual_length {
                    0 => (after_exponent, BigInt::zero()),
                    len => take_int(len)(after_exponent)?,
                };
                Ok((
                    rest,
                    IonValue::IonDecimal(IonDecimal::Decimal {
                        exponent,
                        coefficient,
                    }),
                ))
            }
            Err(e) => Err(e),
        },
        15 => Ok((input, IonValue::IonDecimal(IonDecimal::Null))),
        _ => Err(Err::Failure((input, ErrorKind::NoneOf))),
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
/// ```
pub fn parse_timestamp(input: &[u8], length: usize) -> IResult<&[u8], IonValue> {
    // TODO: This is broadly pretty inefficient, particularly the many heap allocations done here
    // to handle the specification's use of infinite size integers for all fields.

    let (rest, length) = match length {
        0..=13 => (input, length),
        14 => take_usize_var_uint(input)?,
        15 => return Ok((input, IonValue::IonTimestamp(IonTimestamp::Null))),
        _ => return Err(Err::Failure((input, ErrorKind::NoneOf))),
    };

    let (rest, offset) = take_var_int(rest)?;
    let (rest, year) = take_var_uint(rest)?;

    // Parsing complete with precision of Year
    if input.len() - rest.len() == length {
        return Ok((
            rest,
            IonValue::IonTimestamp(IonTimestamp::Year { offset, year }),
        ));
    }

    let (rest, month) = take_var_uint(rest)?;

    // Parsing complete with precision of Month
    if input.len() - rest.len() == length {
        return Ok((
            rest,
            IonValue::IonTimestamp(IonTimestamp::Month {
                offset,
                year,
                month,
            }),
        ));
    }

    let (rest, day) = take_var_uint(rest)?;

    // Parsing complete with precision of Day
    if input.len() - rest.len() == length {
        return Ok((
            rest,
            IonValue::IonTimestamp(IonTimestamp::Day {
                offset,
                year,
                month,
                day,
            }),
        ));
    }

    let (rest, hour) = take_var_uint(rest)?;
    let (rest, minute) = take_var_uint(rest)?;

    // Parsing complete with precision of Minute
    if input.len() - rest.len() == length {
        return Ok((
            rest,
            IonValue::IonTimestamp(IonTimestamp::Minute {
                offset,
                year,
                month,
                day,
                hour,
                minute,
            }),
        ));
    }

    let (rest, second) = take_var_uint(rest)?;

    // Parsing complete with precision of Second
    if input.len() - rest.len() == length {
        return Ok((
            rest,
            IonValue::IonTimestamp(IonTimestamp::Second {
                offset,
                year,
                month,
                day,
                hour,
                minute,
                second,
            }),
        ));
    }

    let (rest, fraction_exponent) = take_var_int_as_i32(rest)?;
    let (rest, fraction_coefficient) = match input.len() - rest.len() {
        parsed_bytes if parsed_bytes > length => {
            return Err(Err::Failure((input, ErrorKind::TooLarge)))
        }
        // A missing coefficient defaults to zero.
        parsed_bytes if parsed_bytes == length => (rest, BigInt::zero()),
        parsed_bytes if parsed_bytes < length => {
            let remaining_bytes = length - (input.len() - rest.len());
            take_int(remaining_bytes)(rest)?
        }
        // (parsed_bytes > length || parsed_bytes == length || parsed_bytes < length) == true
        _ => unreachable!(),
    };

    // The fractional seconds’ value is (coefficient * 10 ^ exponent).
    // It must be greater than or equal to zero and (TODO: less than 1).
    let fraction_coefficient = match fraction_coefficient.to_biguint() {
        Some(value) => value,
        None => return Err(Err::Failure((input, ErrorKind::Verify))),
    };

    // Fractions whose coefficient is zero and exponent is greater than -1 are ignored.
    if fraction_coefficient.is_zero() && fraction_exponent > -1 {
        return Ok((
            rest,
            IonValue::IonTimestamp(IonTimestamp::Second {
                offset,
                year,
                month,
                day,
                hour,
                minute,
                second,
            }),
        ));
    }

    // Parsing complete with precision of FractionalSecond
    Ok((
        rest,
        IonValue::IonTimestamp(IonTimestamp::FractionalSecond {
            offset,
            year,
            month,
            day,
            hour,
            minute,
            second,
            fraction_coefficient,
            fraction_exponent,
        }),
    ))
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
/// In the binary encoding, all Ion symbols are stored as integer symbol IDs whose text values are
/// provided by a symbol table. If L is zero then the symbol ID is zero and the length and symbol ID
/// fields are omitted.
///
/// See Ion Symbols for more details about symbol representations and symbol tables.
/// ```
pub fn parse_symbol(input: &[u8], length: usize) -> IResult<&[u8], IonValue> {
    let (rest, length) = match length {
        0 => return Ok((input, IonValue::IonSymbol(IonSymbol::SidZero))),
        1..=13 => (input, length),
        14 => take_usize_var_uint(input)?,
        15 => return Ok((input, IonValue::IonSymbol(IonSymbol::Null))),
        _ => return Err(Err::Failure((input, ErrorKind::NoneOf))),
    };
    let (rest, symbol_id) = take_uint(length)(rest)?;
    // FIXME
    Ok((rest, IonValue::IonSymbol(IonSymbol::Null)))
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
/// These are always sequences of Unicode characters, encoded as a sequence of UTF-8 octets.
/// ```
pub fn parse_string(input: &[u8], length: usize) -> IResult<&[u8], IonValue> {
    let (rest, length) = match length {
        0..=13 => (input, length),
        14 => take_usize_var_uint(input)?,
        15 => return Ok((input, IonValue::IonString(IonString::Null))),
        _ => return Err(Err::Failure((input, ErrorKind::NoneOf))),
    };
    let (rest, representation) = take(length)(rest)?;
    let representation = match std::str::from_utf8(representation) {
        Ok(value) => value,
        Err(err) => return Err(Err::Failure((rest, ErrorKind::ParseTo))),
    };
    Ok((
        input,
        IonValue::IonString(IonString::String(String::from(representation))),
    ))
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
/// Values of type clob are encoded as a sequence of octets that should be interpreted as text with
/// an unknown encoding (and thus opaque to the application).
///
/// Zero-length clobs are legal, so L may be zero.
/// ```
pub fn parse_clob(input: &[u8], length: usize) -> IResult<&[u8], IonValue> {
    match length {
        0..=14 => unimplemented!(),
        15 => Ok((input, IonValue::IonClob(IonClob::Null))),
        _ => Err(Err::Failure((input, ErrorKind::NoneOf))),
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
/// This is a sequence of octets with no interpretation (and thus opaque to the application).
///
/// Zero-length blobs are legal, so L may be zero.
/// ```
pub fn parse_blob(input: &[u8], length: usize) -> IResult<&[u8], IonValue> {
    match length {
        0..=14 => unimplemented!(),
        15 => Ok((input, IonValue::IonBlob(IonBlob::Null))),
        _ => Err(Err::Failure((input, ErrorKind::NoneOf))),
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
///
/// The representation fields of a list value are simply nested Ion values.
///
/// When L is 15, the value is null.list and there’s no length or nested values. When L is 0,
/// the value is an empty list, and there’s no length or nested values.
///
/// Because values indicate their total lengths in octets, it is possible to locate the beginning of
/// each successive value in constant time.
/// ```
pub fn parse_list(input: &[u8], length: usize) -> IResult<&[u8], IonValue> {
    match length {
        0..=14 => unimplemented!(),
        15 => Ok((input, IonValue::IonList(IonList::Null))),
        _ => Err(Err::Failure((input, ErrorKind::NoneOf))),
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
///
/// Values of type sexp are encoded exactly as are list values, except with a different type code.
/// ```
pub fn parse_sexp(input: &[u8], length: usize) -> IResult<&[u8], IonValue> {
    match length {
        0..=14 => unimplemented!(),
        15 => Ok((
            input,
            IonValue::IonSymbolicExpression(IonSymbolicExpression::Null),
        )),
        _ => Err(Err::Failure((input, ErrorKind::NoneOf))),
    }
}

/// ### 13: struct
///
/// ```text
/// Structs are encoded as sequences of symbol/value pairs. Since all symbols are encoded as positive
/// integers, we can omit the typedesc on the field names and just encode the integer value.
///
///               7       4 3       0
///              +---------+---------+
/// Struct value |   13    |    L    |
///              +---------+---------+======+
///              :     length [VarUInt]     :
///              +======================+===+==================+
///              : field name [VarUInt] :        value         :
///              +======================+======================+
///                          ⋮                     ⋮
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
/// ```
pub fn parse_struct(input: &[u8], length: usize) -> IResult<&[u8], IonValue> {
    match length {
        0..=14 => unimplemented!(),
        15 => Ok((input, IonValue::IonStructure(IonStructure::Null))),
        _ => Err(Err::Failure((input, ErrorKind::NoneOf))),
    }
}

/// ### 14: Annotations
///
/// ```text
/// This special type code doesn’t map to an Ion value type,
/// but instead is a wrapper used to associate annotations with content.
///
/// Annotations are a special type that wrap content identified by the other type codes.
/// The annotations themselves are encoded as integer symbol ids.
///
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
///
/// The length field L field indicates the length from the beginning of the annot_length field to the
/// end of the enclosed value. Because at least one annotation and exactly one content field must exist,
/// L is at least 3 and is never 15.
///
/// The annot_length field contains the length of the (one or more) annot fields.
///
/// It is illegal for an annotation to wrap another annotation atomically, i.e.,
/// annotation(annotation(value)). However, it is legal to have an annotation on a container that
/// holds annotated values.
/// Note that it is possible to enforce the illegality of annotation(annotation(value))
/// directly in a grammar, but we have not chosen to do that in this document.
///
/// Furthermore, it is illegal for an annotation to wrap a NOP Pad since this encoding is not an
/// Ion value. Thus, the following sequence is malformed:
///
/// 0xE3 0x81 0x84 0x00
/// Note: Because L cannot be zero, the octet 0xE0 is not a valid type descriptor.
/// Instead, that octet signals the start of a binary version marker.
/// ```
pub fn parse_annotation(input: &[u8], length: usize) -> IResult<&[u8], IonValue> {
    match length {
        3..=14 => unimplemented!(),
        _ => Err(Err::Failure((input, ErrorKind::NoneOf))),
    }
}

/// ### 15: reserved
///
/// The remaining type code, 15, is reserved for future use and is not legal in Ion 1.0 data.

/// ## Illegal Type Descriptors
///
/// ```text
/// The preceding sections define valid type descriptor octets, composed of a type code (T) in the
/// upper four bits and a length field (L) in the lower four bits. As mentioned,
/// many possible combinations are illegal and must cause parsing errors.
///
/// The following table enumerates the illegal type descriptors in Ion 1.0 data.
///
/// T	L	                Reason
/// 1	[3-14]	            For bool values, L is used to encode the value,
///                         and may be 0 (false), 1 (true), or 15 (null.bool).
/// 3	[0]	                The int 0 is always stored with type code 2.
///                         Thus, type code 3 with L equal to zero is illegal.
/// 4	[1-3],[5-7],[9-14]	For float values, only 32-bit and 64-bit IEEE-754 values are supported.
///                         Additionally, 0e0 and null.float are represented with L equal to 0 and 15,
///                         respectively.
/// 14	[0]*,[1-2],[15]	    Annotation wrappers must have one annot_length field, at least one annot
///                         field, and exactly one value field. Null annotation wrappers are illegal.
///                         *Note: Since 0xE0 signals the start of the BVM, encountering this octet
///                         where a type descriptor is expected should only cause parsing errors when
///                         it is not followed by the rest of the BVM octet sequence.
/// 15	[0-15]	            The type code 15 is illegal in Ion 1.0 data.
/// ```

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;
    use crate::parser::parse::BVM_BYTES;

    #[test]
    fn test_take_type_descriptor() {
        // Constructed TypeDescriptor for null.null should contain {format: 0x0, length: 0xF}
        let null_null = include_bytes!("../../tests/ion-tests/iontestdata/good/null.10n");
        let null_val = parse::take_ion_version(null_null).unwrap();
        assert_eq!(
            take_type_descriptor(null_val.0),
            Ok((
                &null_null[(BVM_BYTES + TYPE_DESCRIPTOR_BYTES)..],
                TypeDescriptor {
                    format: 0x0,
                    length: 0xF,
                }
            ))
        );
        // Constructed TypeDescriptor for null.bool should contain {format: 0x1, length: 0xF}
        let null_bool = include_bytes!("../../tests/ion-tests/iontestdata/good/nullBool.10n");
        let bool_val = parse::take_ion_version(null_bool).unwrap();
        assert_eq!(
            take_type_descriptor(bool_val.0),
            Ok((
                &null_bool[(BVM_BYTES + TYPE_DESCRIPTOR_BYTES)..],
                TypeDescriptor {
                    format: 0x1,
                    length: 0xF,
                }
            ))
        );
    }

    #[test]
    fn binary_version_marker_test() {
        let data = include_bytes!("../../tests/ion-tests/iontestdata/good/null.10n");
        println!("bytes:\n{:?}", &data[0..4]);
        // assert_eq!(, );
    }

    #[test]
    fn parse_null_test() {
        let file_bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/null.10n");
        let value = parse::take_ion_version(file_bytes).unwrap();
        let descriptor = take_type_descriptor(value.0).unwrap();

        assert_eq!(
            parse_null(descriptor.0, descriptor.1.length),
            Ok((&[] as &[u8], IonValue::IonNull(IonNull::Null)))
        );
    }

    #[test]
    fn parse_nop_test_one_byte() {
        let file_bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/nopPadOneByte.10n");
        let value = parse::take_ion_version(file_bytes).unwrap();
        let descriptor = take_type_descriptor(value.0).unwrap();

        assert_eq!(
            parse_null(descriptor.0, descriptor.1.length),
            Ok((&[] as &[u8], IonValue::IonNull(IonNull::Pad)))
        );
    }

    #[test]
    fn parse_nop_test_16_bytes() {
        let file_bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/nopPad16Bytes.10n");
        let value = parse::take_ion_version(file_bytes).unwrap();
        let descriptor = take_type_descriptor(value.0).unwrap();
        let parse = take_var_uint(descriptor.0);

        assert_eq!(
            parse_null(descriptor.0, descriptor.1.length),
            Ok((&[] as &[u8], IonValue::IonNull(IonNull::Pad)))
        );
    }
}
