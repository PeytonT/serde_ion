use super::subfield::*;
use nom::{bytes::complete::take, error::ErrorKind, Err, IResult};
use num_traits::cast::FromPrimitive;

const TYPE_DESCRIPTOR_BYTES: usize = 1;

/// Documentation draws extensively on http://amzn.github.io/ion-docs/docs/binary.html.

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
///
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

/// A partially-processed value from an Ion stream
///
/// Contains information required to complete processing or to skip over the value.
#[derive(Clone, Debug, PartialEq)]
pub struct TypedValue<'a> {
    /// The type code T from the type descriptor
    pub type_code: TypeCode,
    /// The four-bit length L from the type descriptor.
    /// Not to be confused with the length of the representation.
    pub length_code: u8,
    /// Slice of the Ion stream from the starting index of the value
    pub index: &'a [u8],
    /// The sequence of octets comprising the binary value.
    pub value: &'a [u8],
    /// The sequence of octets comprising the representation.
    pub rep: &'a [u8],
}

/// The possible values of the type code of a Typed Value
///
/// # Panics
///
/// While TypeCode itself obviously does not have any mechanism to originate a panic, other code
/// depends via the FromPrimitive derivation on the fact that there are 16 variants of this enum.
#[derive(Clone, Debug, PartialEq, FromPrimitive)]
pub enum TypeCode {
    Null,
    Bool,
    PosInt,
    NegInt,
    Float,
    Decimal,
    Timestamp,
    Symbol,
    String,
    Clob,
    Blob,
    List,
    Sexp,
    Struct,
    Annotation,
    Reserved,
}

pub fn take_typed_value(input: &[u8]) -> IResult<&[u8], TypedValue> {
    let (rest, descriptor_byte) = take(1usize)(input)?;
    let type_code: TypeCode = TypeCode::from_u8(descriptor_byte[0] >> 4).unwrap();
    let length_code: u8 = descriptor_byte[0] & 0b0000_1111;
    let length: usize = length_code as usize;
    match type_code {
        TypeCode::Null => match length_code {
            // Special cases: 1-byte NOP pad (L == 0) and null.null (L == 15) both have empty representation
            0 | 15 => Ok((
                rest,
                TypedValue {
                    type_code,
                    length_code,
                    index: input,
                    value: descriptor_byte,
                    rep: &[], // null.null has only a descriptor octet
                },
            )),
            x => {
                let (rest, length) = take_representation_length(rest, x)?;
                let (end, rep) = take(length)(rest)?;
                // TODO: remove unnecessary checks
                let (end, value) = take(input.len() - end.len())(input)?;
                Ok((
                    end,
                    TypedValue {
                        type_code,
                        length_code,
                        index: input,
                        value,
                        rep,
                    },
                ))
            }
        },
        TypeCode::Bool => Ok((
            rest,
            TypedValue {
                type_code,
                length_code,
                index: input,
                value: descriptor_byte,
                rep: &[], // bools have only a descriptor octet
            },
        )),
        TypeCode::Float => match length_code {
            // Special cases: 0e0 (L == 0) and null.float (L == 15) both have empty representation
            0 | 15 => Ok((
                rest,
                TypedValue {
                    type_code,
                    length_code,
                    index: input,
                    value: descriptor_byte,
                    rep: &[],
                },
            )),
            4 => {
                let (end, rep) = take(4usize)(rest)?;
                // TODO: remove unnecessary checks
                // we know these 5 bytes exist from the previous line
                let (end, value) = take(5usize)(input)?;
                Ok((
                    end,
                    TypedValue {
                        type_code,
                        length_code,
                        index: input,
                        value,
                        rep,
                    },
                ))
            }
            8 => {
                let (end, rep) = take(8usize)(rest)?;
                // TODO: remove unnecessary checks
                // we know these 9 bytes exist from the previous line
                let (end, value) = take(9usize)(input)?;
                Ok((
                    end,
                    TypedValue {
                        type_code,
                        length_code,
                        index: input,
                        value,
                        rep,
                    },
                ))
            }
            _ => Err(Err::Failure((rest, ErrorKind::LengthValue))),
        },
        TypeCode::Reserved => Err(Err::Failure((rest, ErrorKind::LengthValue))),
        // All remaining type codes behave in a standard way
        _ => {
            let (rest, length) = take_representation_length(rest, length_code)?;
            let (end, rep) = take(length)(rest)?;
            let (end, value) = take(input.len() - end.len())(input)?;
            Ok((
                end,
                TypedValue {
                    type_code,
                    length_code,
                    index: input,
                    value,
                    rep,
                },
            ))
        }
    }
}

fn take_representation_length(input: &[u8], length_code: u8) -> IResult<&[u8], usize> {
    let length: usize = length_code as usize;
    let (rest, length) = match length {
        0..=13 => (input, length),
        14 => take_usize_var_uint(input)?,
        _ => (input, 0),
    };
    Ok((rest, length))
}

#[allow(non_snake_case)]
#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn type_code_has_16_variants() {
        let fifteen: u8 = 0b0000_1111;
        let type_code: TypeCode = TypeCode::from_u8(fifteen).unwrap();
        assert_eq!(type_code, TypeCode::Reserved);
    }

    #[test]
    #[should_panic]
    fn type_code_has_no_17th_variant() {
        let sixteen: u8 = 0b0001_0000;
        let type_code: TypeCode = TypeCode::from_u8(sixteen).unwrap();
    }
}
