use super::subfield::*;
use crate::error::{BinaryFormatError, FormatError};
use crate::parser::parse_error::{IonError, IonResult};
use nom::{bytes::complete::take, Err};
use num_traits::cast::FromPrimitive;

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
    pub length_code: LengthCode,
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
#[derive(Clone, Debug, PartialEq, FromPrimitive, Copy)]
pub enum TypeCode {
    Null = 0,
    Bool = 1,
    PosInt = 2,
    NegInt = 3,
    Float = 4,
    Decimal = 5,
    Timestamp = 6,
    Symbol = 7,
    String = 8,
    Clob = 9,
    Blob = 10,
    List = 11,
    Sexp = 12,
    Struct = 13,
    Annotation = 14,
    Reserved = 15,
}

/// The possible values of the length field of a Typed Value
///
/// # Panics
///
/// While LengthCode itself obviously does not have any mechanism to originate a panic, other code
/// depends via the FromPrimitive derivation on the fact that there are 16 variants of this enum.
#[derive(Clone, Debug, PartialEq, FromPrimitive, Copy)]
pub enum LengthCode {
    L0 = 0,
    L1 = 1,
    L2 = 2,
    L3 = 3,
    L4 = 4,
    L5 = 5,
    L6 = 6,
    L7 = 7,
    L8 = 8,
    L9 = 9,
    L10 = 10,
    L11 = 11,
    L12 = 12,
    L13 = 13,
    L14 = 14,
    L15 = 15,
}

/// Exists to support skip-searching over a value stream.
pub fn take_typed_value(index: &[u8]) -> IonResult<&[u8], TypedValue> {
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

    #[test]
    fn length_code_has_16_variants() {
        let fifteen: u8 = 0b0000_1111;
        let length_code: LengthCode = LengthCode::from_u8(fifteen).unwrap();
        assert_eq!(length_code, LengthCode::L15);
    }

    #[test]
    #[should_panic]
    fn length_code_has_no_17th_variant() {
        let sixteen: u8 = 0b0001_0000;
        let length_code: LengthCode = LengthCode::from_u8(sixteen).unwrap();
    }
}
