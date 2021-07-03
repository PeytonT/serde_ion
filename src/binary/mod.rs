//! Special values and types related to the binary Ion format.

/// ## Binary Version Markers
///
/// In the binary format, an Ion value stream starts with a four-octet binary version marker (BVM)
/// that specifies the Ion version used to encode the data that follows, followed by zero or more
/// values which contain the actual data. These values are generally referred to as  
/// “top-level values”.
///
/// ```text
///                        7    0 7     0 7     0 7    0
///                       +------+-------+-------+------+
/// binary version marker | 0xE0 | major | minor | 0xEA |
///                       +------+-------+-------+------+
/// ```
/// The first octet is 0xE0 and the fourth octet is 0xEA.
/// The second and third octets contain major and minor version numbers.
/// The only currently valid BVM, identifying Ion 1.0, is 0xE0 0x01 0x00 0xEA.
///
/// ```text
///               31                      0
///              +-------------------------+
/// value stream |  binary version marker  |
///              +-------------------------+
///              :          value          :
///              +=========================+
///                           ⋮
///              +=========================+
///              :  binary version marker  :
///              +=========================+
///              :          value          :
///              +=========================+
///                           ⋮
/// ```
/// A value stream can contain other BVMs interspersed with the top-level values.
/// Each BVM resets the deserializer to the initial state for the given Ion version.
pub(crate) const BVM_1_0: [u8; 4] = [
    BVM_START,
    BVM_1_0_MAJOR_VERSION,
    BVM_1_0_MINOR_VERSION,
    BVM_END,
];
const BVM_START: u8 = 0xE0;
const BVM_END: u8 = 0xEA;
const BVM_1_0_MAJOR_VERSION: u8 = 0x01;
const BVM_1_0_MINOR_VERSION: u8 = 0x00;

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

/// The possible values of the type code of a Typed Value
///
/// # Panics
///
/// While TypeCode itself obviously does not have any mechanism to originate a panic, other code
/// depends via the FromPrimitive derivation on the fact that there are 16 variants of this enum.
#[derive(Clone, Debug, PartialEq, FromPrimitive, Copy)]
pub(crate) enum TypeCode {
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

impl TypeCode {
    pub(crate) const fn to_byte(self) -> u8 {
        (self as u8) << 4
    }
}

/// The possible values of the length field of a Typed Value
///
/// # Panics
///
/// While LengthCode itself obviously does not have any mechanism to originate a panic, other code
/// depends via the FromPrimitive derivation on the fact that there are 16 variants of this enum.
#[derive(Clone, Debug, PartialEq, FromPrimitive, Copy)]
pub(crate) enum LengthCode {
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

pub(crate) const fn type_descriptor(t: TypeCode, l: LengthCode) -> u8 {
    t.to_byte() + l as u8
}

/// ## UInt Fields
///
/// UInt fields represent fixed-length unsigned integer values.
/// These field formats are always used in some context that clearly indicates the
/// number of octets in the field.
///
/// UInts are sequences of octets, interpreted as big-endian.
///
/// ```text
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
/// ```
pub(crate) struct UInt(Vec<u8>);

impl UInt {
    pub fn new(bytes: Vec<u8>) -> UInt {
        UInt(bytes)
    }

    pub fn into_bytes(self) -> Vec<u8> {
        self.0
    }
}

/// ## Int Fields
///
/// Int fields represent fixed-length signed integer values.
/// These field formats are always used in some context that clearly indicates the
/// number of octets in the field.
///
/// Ints are sequences of octets, interpreted as sign-and-magnitude big endian integers (with the sign
/// on the highest-order bit of the first octet). This means that the representations of
/// 123456 and -123456 should only differ in their sign bit.
///
/// ```text
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
/// ```
pub(crate) struct Int(Vec<u8>);

impl Int {
    pub fn new(bytes: Vec<u8>) -> Int {
        Int(bytes)
    }

    pub fn into_bytes(self) -> Vec<u8> {
        self.0
    }
}

/// ## VarUInt Fields
///
/// VarUInt fields represent self-delimiting, variable-length unsigned integer values.
/// These field formats are always used in a context that does not indicate the number of octets
/// in the field; the last octet (and only the last octet) has its high-order bit set to
/// terminate the field.
///
/// VarUInts are a sequence of octets. The high-order bit of the last octet is one,
/// indicating the end of the sequence. All other high-order bits must be zero.
///
/// ```text
///                 7  6                   0       n+7 n+6                 n
///               +===+=====================+     +---+---------------------+
/// VarUInt field : 0 :         bits        :  …  | 1 |         bits        |
///               +===+=====================+     +---+---------------------+
/// ```
pub(crate) struct VarUInt(Vec<u8>);

impl VarUInt {
    pub fn new(bytes: Vec<u8>) -> VarUInt {
        VarUInt(bytes)
    }

    pub fn into_bytes(self) -> Vec<u8> {
        self.0
    }
}

/// ## VarInt Fields
///
/// VarInt fields represent self-delimiting, variable-length signed integer values.
/// These field formats are always used in a context that does not indicate the number of octets
/// in the field; the last octet (and only the last octet) has its high-order bit set to
/// terminate the field.
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
/// ```text
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
/// ```
/// ```text
///                             7   6  5           0
///                           +---+---+-------------+
/// single octet VarInt field | 1 |   |  magnitude  |
///                           +---+---+-------------+
///                                 ^
///                                 |
///                                 +--sign
/// ```
pub(crate) struct VarInt(Vec<u8>);

impl VarInt {
    pub fn new(bytes: Vec<u8>) -> VarInt {
        VarInt(bytes)
    }

    pub fn into_bytes(self) -> Vec<u8> {
        self.0
    }
}

#[derive(PartialEq, PartialOrd, Eq, Ord, Copy, Clone, Debug, Hash)]
pub(crate) enum Sign {
    Minus,
    Plus,
}

#[allow(non_snake_case)]
#[cfg(test)]
mod tests {
    use num_traits::cast::FromPrimitive;
    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn type_code_has_16_variants() {
        let fifteen: u8 = 0b0000_1111;
        let type_code: TypeCode = TypeCode::from_u8(fifteen).unwrap();
        assert_eq!(type_code, TypeCode::Reserved);
    }

    #[test]
    fn type_code_has_no_17th_variant() {
        let sixteen: u8 = 0b0001_0000;
        let type_code: Option<TypeCode> = TypeCode::from_u8(sixteen);
        assert_eq!(type_code, None);
    }

    #[test]
    fn length_code_has_16_variants() {
        let fifteen: u8 = 0b0000_1111;
        let length_code: LengthCode = LengthCode::from_u8(fifteen).unwrap();
        assert_eq!(length_code, LengthCode::L15);
    }

    #[test]
    fn length_code_has_no_17th_variant() {
        let sixteen: u8 = 0b0001_0000;
        let length_code: Option<LengthCode> = LengthCode::from_u8(sixteen);
        assert_eq!(length_code, None);
    }
}
