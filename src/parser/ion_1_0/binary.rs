use super::subfield::*;
use super::typed_value::*;
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
use num_traits::cast::FromPrimitive;
use num_traits::cast::ToPrimitive;
use num_traits::identities::Zero;
use num_traits::real::Real;
use num_traits::{One, Signed};

/// Documentation draws extensively on http://amzn.github.io/ion-docs/docs/binary.html.

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

/// Take a single IonValue from the head of an Ion byte stream
pub fn take_value(i: &[u8]) -> IResult<&[u8], IonValue> {
    let (rest, descriptor) = take_type_descriptor(i)?;
    let length = descriptor.length as usize;
    match descriptor.type_code {
        TypeCode::Null => take_null(rest, length),
        TypeCode::Bool => take_bool(rest, length),
        TypeCode::PosInt => take_positive_int(rest, length),
        TypeCode::NegInt => take_negative_int(rest, length),
        TypeCode::Float => take_float(rest, length),
        TypeCode::Decimal => take_decimal(rest, length),
        TypeCode::Timestamp => take_timestamp(rest, length),
        TypeCode::Symbol => take_symbol(rest, length),
        TypeCode::String => take_string(rest, length),
        TypeCode::Clob => take_clob(rest, length),
        TypeCode::Blob => take_blob(rest, length),
        TypeCode::List => take_list(rest, length),
        TypeCode::Sexp => take_sexp(rest, length),
        TypeCode::Struct => take_struct(rest, length),
        TypeCode::Annotation => take_annotation(rest, length),
        TypeCode::Reserved => Err(Err::Failure((i, ErrorKind::LengthValue))),
    }
}

/// Take a single IonValue from the head of an Ion byte stream
pub fn parse_value(i: &[u8]) -> IResult<&[u8], IonValue> {
    let (rest, typed_value) = take_typed_value(i)?;
    let (_, value) = parse_typed_value(typed_value)?;
    Ok((rest, value))
}

/// Parse a TypedValue containing bytes representing an Ion value into the IonValue data model form
pub fn parse_typed_value(value: TypedValue) -> IResult<&[u8], IonValue> {
    match value.type_code {
        TypeCode::Null => parse_null(value),
        TypeCode::Bool => parse_bool(value),
        TypeCode::PosInt => parse_positive_int(value),
        TypeCode::NegInt => parse_negative_int(value),
        TypeCode::Float => parse_float(value),
        TypeCode::Decimal => parse_decimal(value),
        TypeCode::Timestamp => parse_timestamp(value),
        TypeCode::Symbol => parse_symbol(value),
        TypeCode::String => parse_string(value),
        TypeCode::Clob => parse_clob(value),
        TypeCode::Blob => parse_blob(value),
        TypeCode::List => parse_list(value),
        TypeCode::Sexp => parse_sexp(value),
        TypeCode::Struct => parse_struct(value),
        TypeCode::Annotation => parse_annotation(value),
        TypeCode::Reserved => error_reserved(value),
    }
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
/// ```
fn take_null(input: &[u8], length: usize) -> IResult<&[u8], IonValue> {
    match length {
        0..=13 => Ok((&input[length..], IonValue::IonNull(IonNull::Pad))),
        14 => match take_usize_var_uint(input) {
            Ok((rest, length)) => Ok((&rest[length..], IonValue::IonNull(IonNull::Pad))),
            Err(e) => Err(e),
        },
        15 => Ok((input, IonValue::IonNull(IonNull::Null))),
        _ => Err(Err::Failure((input, ErrorKind::LengthValue))),
    }
}

fn parse_null(typed_value: TypedValue) -> IResult<&[u8], IonValue> {
    match typed_value.length_code {
        0..=14 => Ok((&[], IonValue::IonNull(IonNull::Pad))),
        15 => Ok((&[], IonValue::IonNull(IonNull::Null))),
        _ => Err(Err::Failure((typed_value.index, ErrorKind::LengthValue))),
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
pub fn take_bool(input: &[u8], length: usize) -> IResult<&[u8], IonValue> {
    match length {
        0 => Ok((input, IonValue::IonBoolean(IonBoolean::False))),
        1 => Ok((input, IonValue::IonBoolean(IonBoolean::True))),
        15 => Ok((input, IonValue::IonBoolean(IonBoolean::Null))),
        _ => Err(Err::Failure((input, ErrorKind::LengthValue))),
    }
}

fn parse_bool(typed_value: TypedValue) -> IResult<&[u8], IonValue> {
    match typed_value.length_code {
        0 => Ok((&[], IonValue::IonBoolean(IonBoolean::False))),
        1 => Ok((&[], IonValue::IonBoolean(IonBoolean::True))),
        15 => Ok((&[], IonValue::IonBoolean(IonBoolean::Null))),
        _ => Err(Err::Failure((typed_value.index, ErrorKind::LengthValue))),
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
pub fn take_positive_int(input: &[u8], length: usize) -> IResult<&[u8], IonValue> {
    let (rest, length) = match length {
        0..=13 => (input, length),
        14 => take_usize_var_uint(input)?,
        15 => return Ok((input, IonValue::IonInteger(IonInteger::Null))),
        _ => return Err(Err::Failure((input, ErrorKind::LengthValue))),
    };
    let (rest, magnitude) = take_uint(length)(rest).expect("Cannot take UInt");
    Ok((
        rest,
        IonValue::IonInteger(IonInteger::Integer {
            value: BigInt::from_biguint(Sign::Plus, magnitude),
        }),
    ))
}

fn parse_positive_int(typed_value: TypedValue) -> IResult<&[u8], IonValue> {
    match typed_value.length_code {
        0..=14 => {
            let magnitude = BigUint::from_bytes_be(typed_value.rep);
            Ok((
                &[],
                IonValue::IonInteger(IonInteger::Integer {
                    value: BigInt::from_biguint(Sign::Plus, magnitude),
                }),
            ))
        }
        15 => Ok((&[], IonValue::IonInteger(IonInteger::Null))),
        _ => Err(Err::Failure((&[], ErrorKind::LengthValue))),
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
pub fn take_negative_int(input: &[u8], length: usize) -> IResult<&[u8], IonValue> {
    let (rest, length) = match length {
        0..=13 => (input, length),
        14 => take_usize_var_uint(input)?,
        15 => return Ok((input, IonValue::IonInteger(IonInteger::Null))),
        _ => return Err(Err::Failure((input, ErrorKind::LengthValue))),
    };
    let (rest, magnitude) = take_uint(length)(rest).expect("Cannot take UInt");
    Ok((
        rest,
        IonValue::IonInteger(IonInteger::Integer {
            value: -1 * magnitude.to_bigint().expect("Cannot convert to BigInt"),
        }),
    ))
}

fn parse_negative_int(typed_value: TypedValue) -> IResult<&[u8], IonValue> {
    match typed_value.length_code {
        0..=14 => {
            let magnitude = BigUint::from_bytes_be(typed_value.rep);
            Ok((
                &[],
                IonValue::IonInteger(IonInteger::Integer {
                    value: BigInt::from_biguint(Sign::Minus, magnitude),
                }),
            ))
        }
        15 => Ok((&[], IonValue::IonInteger(IonInteger::Null))),
        _ => Err(Err::Failure((typed_value.index, ErrorKind::LengthValue))),
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
pub fn take_float(input: &[u8], length: usize) -> IResult<&[u8], IonValue> {
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
        _ => Err(Err::Failure((input, ErrorKind::LengthValue))),
    }
}

fn parse_float(typed_value: TypedValue) -> IResult<&[u8], IonValue> {
    match typed_value.length_code {
        0 => Ok((&[], IonValue::IonFloat(IonFloat::Float { value: 0e0 }))),
        4 => {
            let (_, value) = float(typed_value.rep)?;
            Ok((
                &[],
                IonValue::IonFloat(IonFloat::Float {
                    value: f64::from(value),
                }),
            ))
        }
        8 => {
            let (_, value) = double(typed_value.rep)?;
            Ok((&[], IonValue::IonFloat(IonFloat::Float { value })))
        }
        15 => Ok((&[], IonValue::IonFloat(IonFloat::Null))),
        _ => Err(Err::Failure((typed_value.index, ErrorKind::LengthValue))),
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
pub fn take_decimal(input: &[u8], length: usize) -> IResult<&[u8], IonValue> {
    let (exponent_index, length) = match length {
        0 => {
            return Ok((
                input,
                IonValue::IonDecimal(IonDecimal::Decimal {
                    coefficient: BigInt::zero(),
                    exponent: BigInt::zero(),
                }),
            ))
        }
        1..=13 => (input, length),
        14 => take_usize_var_uint(input)?,
        15 => return Ok((input, IonValue::IonDecimal(IonDecimal::Null))),
        _ => return Err(Err::Failure((input, ErrorKind::LengthValue))),
    };
    let (coefficient_index, exponent) = take_var_int(exponent_index)?;
    let remaining_bytes = length - (exponent_index.len() - coefficient_index.len());
    let (rest, coefficient) = match remaining_bytes {
        0 => (coefficient_index, BigInt::zero()),
        residual_length => take_int(residual_length)(coefficient_index)?,
    };
    Ok((
        rest,
        IonValue::IonDecimal(IonDecimal::Decimal {
            exponent,
            coefficient,
        }),
    ))
}

fn parse_decimal(typed_value: TypedValue) -> IResult<&[u8], IonValue> {
    match typed_value.length_code {
        0 => Ok((
            &[],
            IonValue::IonDecimal(IonDecimal::Decimal {
                coefficient: BigInt::zero(),
                exponent: BigInt::zero(),
            }),
        )),
        1..=14 => {
            let (coefficient_index, exponent) = take_var_int(typed_value.rep)?;
            let (_, coefficient) = match coefficient_index.len() {
                0 => (coefficient_index, BigInt::zero()),
                remaining_bytes => take_int(remaining_bytes)(coefficient_index)?,
            };
            Ok((
                &[],
                IonValue::IonDecimal(IonDecimal::Decimal {
                    exponent,
                    coefficient,
                }),
            ))
        }
        15 => Ok((&[], IonValue::IonDecimal(IonDecimal::Null))),
        _ => Err(Err::Failure((typed_value.index, ErrorKind::LengthValue))),
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
pub fn take_timestamp(input: &[u8], length: usize) -> IResult<&[u8], IonValue> {
    // TODO: This is broadly pretty inefficient, particularly the many heap allocations done here
    // to handle the specification's use of infinite size integers for all fields.

    let (rest, length) = match length {
        0..=13 => (input, length),
        14 => take_usize_var_uint(input)?,
        15 => return Ok((input, IonValue::IonTimestamp(IonTimestamp::Null))),
        _ => return Err(Err::Failure((input, ErrorKind::LengthValue))),
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
    // It must be greater than or equal to zero and (FIXME: less than 1).
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

fn parse_timestamp(typed_value: TypedValue) -> IResult<&[u8], IonValue> {
    match typed_value.length_code {
        0..=14 => {
            let (rest, offset) = take_var_int(typed_value.rep)?;
            let (rest, year) = take_var_uint(rest)?;

            // Parsing complete with precision of Year
            if rest.is_empty() {
                return Ok((
                    rest,
                    IonValue::IonTimestamp(IonTimestamp::Year { offset, year }),
                ));
            }

            let (rest, month) = take_var_uint(rest)?;

            // Parsing complete with precision of Month
            if rest.is_empty() {
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
            if rest.is_empty() {
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
            if rest.is_empty() {
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
            if rest.is_empty() {
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
                None => return Err(Err::Failure((typed_value.index, ErrorKind::Verify))),
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
        15 => Ok((&[], IonValue::IonTimestamp(IonTimestamp::Null))),
        _ => Err(Err::Failure((typed_value.index, ErrorKind::LengthValue))),
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
/// In the binary encoding, all Ion symbols are stored as integer symbol IDs whose text values are
/// provided by a symbol table. If L is zero then the symbol ID is zero and the length and symbol ID
/// fields are omitted.
///
/// See Ion Symbols for more details about symbol representations and symbol tables.
/// ```
pub fn take_symbol(input: &[u8], length: usize) -> IResult<&[u8], IonValue> {
    let (rest, length) = match length {
        0 => return Ok((input, IonValue::IonSymbol(IonSymbol::SidZero))),
        1..=13 => (input, length),
        14 => take_usize_var_uint(input)?,
        15 => return Ok((input, IonValue::IonSymbol(IonSymbol::Null))),
        _ => return Err(Err::Failure((input, ErrorKind::LengthValue))),
    };
    let (rest, symbol_id) = take_uint(length)(rest)?;
    // FIXME
    Ok((rest, IonValue::IonSymbol(IonSymbol::Null)))
}

fn parse_symbol(typed_value: TypedValue) -> IResult<&[u8], IonValue> {
    match typed_value.length_code {
        0 => Ok((&[], IonValue::IonSymbol(IonSymbol::SidZero))),
        1..=14 => {
            let symbol_id = parse_uint(typed_value.rep);
            // FIXME
            Ok((&[], IonValue::IonSymbol(IonSymbol::Null)))
        }
        15 => Ok((&[], IonValue::IonSymbol(IonSymbol::Null))),
        _ => Err(Err::Failure((typed_value.index, ErrorKind::LengthValue))),
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
/// These are always sequences of Unicode characters, encoded as a sequence of UTF-8 octets.
/// ```
pub fn take_string(input: &[u8], length: usize) -> IResult<&[u8], IonValue> {
    let (rest, length) = match length {
        0..=13 => (input, length),
        14 => take_usize_var_uint(input)?,
        15 => return Ok((input, IonValue::IonString(IonString::Null))),
        _ => return Err(Err::Failure((input, ErrorKind::LengthValue))),
    };
    let (rest, representation) = take(length)(rest)?;
    let representation = match std::str::from_utf8(representation) {
        Ok(value) => value,
        Err(err) => return Err(Err::Failure((rest, ErrorKind::ParseTo))),
    };
    Ok((
        rest,
        IonValue::IonString(IonString::String {
            value: String::from(representation),
        }),
    ))
}

fn parse_string(typed_value: TypedValue) -> IResult<&[u8], IonValue> {
    match typed_value.length_code {
        0..=14 => {
            let representation = match std::str::from_utf8(typed_value.rep) {
                Ok(value) => value,
                Err(err) => return Err(Err::Failure((&[], ErrorKind::ParseTo))),
            };
            Ok((
                &[],
                IonValue::IonString(IonString::String {
                    value: String::from(representation),
                }),
            ))
        }
        15 => Ok((&[], IonValue::IonString(IonString::Null))),
        _ => Err(Err::Failure((typed_value.index, ErrorKind::LengthValue))),
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
/// Values of type clob are encoded as a sequence of octets that should be interpreted as text with
/// an unknown encoding (and thus opaque to the application).
///
/// Zero-length clobs are legal, so L may be zero.
/// ```
pub fn take_clob(input: &[u8], length: usize) -> IResult<&[u8], IonValue> {
    let (rest, length) = match length {
        0..=13 => (input, length),
        14 => take_usize_var_uint(input)?,
        15 => return Ok((input, IonValue::IonClob(IonClob::Null))),
        _ => return Err(Err::Failure((input, ErrorKind::LengthValue))),
    };
    let (rest, data) = take(length)(rest)?;
    Ok((
        rest,
        IonValue::IonClob(IonClob::Clob {
            data: data.to_vec(),
        }),
    ))
}

fn parse_clob(typed_value: TypedValue) -> IResult<&[u8], IonValue> {
    match typed_value.length_code {
        0..=14 => Ok((
            &[],
            IonValue::IonClob(IonClob::Clob {
                data: typed_value.rep.to_vec(),
            }),
        )),
        15 => Ok((&[], IonValue::IonClob(IonClob::Null))),
        _ => Err(Err::Failure((typed_value.index, ErrorKind::LengthValue))),
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
pub fn take_blob(input: &[u8], length: usize) -> IResult<&[u8], IonValue> {
    let (rest, length) = match length {
        0..=13 => (input, length),
        14 => take_usize_var_uint(input)?,
        15 => return Ok((input, IonValue::IonBlob(IonBlob::Null))),
        _ => return Err(Err::Failure((input, ErrorKind::LengthValue))),
    };
    let (rest, data) = take(length)(rest)?;
    Ok((
        rest,
        IonValue::IonBlob(IonBlob::Blob {
            data: data.to_vec(),
        }),
    ))
}

fn parse_blob(typed_value: TypedValue) -> IResult<&[u8], IonValue> {
    match typed_value.length_code {
        0..=14 => Ok((
            &[],
            IonValue::IonBlob(IonBlob::Blob {
                data: typed_value.rep.to_vec(),
            }),
        )),
        15 => Ok((&[], IonValue::IonBlob(IonBlob::Null))),
        _ => Err(Err::Failure((typed_value.index, ErrorKind::LengthValue))),
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
pub fn take_list(input: &[u8], length: usize) -> IResult<&[u8], IonValue> {
    match length {
        0..=14 => unimplemented!(),
        15 => Ok((input, IonValue::IonList(IonList::Null))),
        _ => Err(Err::Failure((input, ErrorKind::LengthValue))),
    }
}

fn parse_list(typed_value: TypedValue) -> IResult<&[u8], IonValue> {
    match typed_value.length_code {
        0..=14 => unimplemented!(),
        15 => Ok((&[], IonValue::IonList(IonList::Null))),
        _ => Err(Err::Failure((typed_value.index, ErrorKind::LengthValue))),
    }
}

struct BinaryListIterator<'a> {
    cursor: u8,
    list_bytes: &'a [u8],
}

impl<'a> BinaryListIterator<'a> {
    fn new(list: &[u8]) -> BinaryListIterator {
        BinaryListIterator {
            cursor: 0,
            list_bytes: list,
        }
    }
}

// Implement `Iterator` for `BinaryListIterator`.
impl<'a> Iterator for BinaryListIterator<'a> {
    type Item = IonValue;

    // The return type is `Option<T>`:
    //     * When the `Iterator` is finished, `None` is returned.
    //     * Otherwise, the next value is wrapped in `Some` and returned.
    fn next(&mut self) -> Option<IonValue> {
        let cursor = self.cursor;
        let list_bytes = self.list_bytes;

        Some(IonValue::IonNull(IonNull::Null))
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
pub fn take_sexp(input: &[u8], length: usize) -> IResult<&[u8], IonValue> {
    match length {
        0..=14 => unimplemented!(),
        15 => Ok((
            input,
            IonValue::IonSymbolicExpression(IonSymbolicExpression::Null),
        )),
        _ => Err(Err::Failure((input, ErrorKind::LengthValue))),
    }
}

fn parse_sexp(typed_value: TypedValue) -> IResult<&[u8], IonValue> {
    match typed_value.length_code {
        0..=14 => unimplemented!(),
        15 => Ok((
            &[],
            IonValue::IonSymbolicExpression(IonSymbolicExpression::Null),
        )),
        _ => Err(Err::Failure((typed_value.index, ErrorKind::LengthValue))),
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
pub fn take_struct(input: &[u8], length: usize) -> IResult<&[u8], IonValue> {
    match length {
        0..=14 => unimplemented!(),
        15 => Ok((input, IonValue::IonStructure(IonStructure::Null))),
        _ => Err(Err::Failure((input, ErrorKind::LengthValue))),
    }
}

fn parse_struct(typed_value: TypedValue) -> IResult<&[u8], IonValue> {
    match typed_value.length_code {
        0..=14 => unimplemented!(),
        15 => Ok((&[], IonValue::IonStructure(IonStructure::Null))),
        _ => Err(Err::Failure((typed_value.index, ErrorKind::LengthValue))),
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
pub fn take_annotation(input: &[u8], length: usize) -> IResult<&[u8], IonValue> {
    match length {
        3..=14 => unimplemented!(),
        _ => Err(Err::Failure((input, ErrorKind::LengthValue))),
    }
}

fn parse_annotation(typed_value: TypedValue) -> IResult<&[u8], IonValue> {
    match typed_value.length_code {
        0..=14 => unimplemented!(),
        15 => Ok((&[], IonValue::IonStructure(IonStructure::Null))),
        _ => Err(Err::Failure((typed_value.index, ErrorKind::LengthValue))),
    }
}

/// ### 15: reserved
///
/// The remaining type code, 15, is reserved for future use and is not legal in Ion 1.0 data.

pub fn error_reserved(typed_value: TypedValue) -> IResult<&[u8], IonValue> {
    Err(Err::Failure((typed_value.index, ErrorKind::LengthValue)))
}
