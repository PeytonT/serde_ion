use crate::binary::{self, Int, UInt, VarInt, VarUInt};
use bit_vec::BitVec;
use num_bigint::{BigInt, BigUint, Sign};

impl From<&BigInt> for Int {
    fn from(int: &BigInt) -> Self {
        let (sign, mut bytes) = int.to_bytes_be();
        let first_byte = match bytes.first_mut() {
            None => return Int::new(vec![0u8]),
            Some(first_byte) => first_byte,
        };
        match sign {
            Sign::Minus => {
                if *first_byte > 0b0111_1111 {
                    bytes.insert(0, 0b1000_0000);
                } else {
                    *first_byte |= 0b1000_0000;
                }
                Int::new(bytes)
            }
            Sign::Plus => {
                if *first_byte > 0b0111_1111 {
                    bytes.insert(0, 0b0000_0000);
                }
                Int::new(bytes)
            }
            Sign::NoSign => Int::new(vec![0u8]),
        }
    }
}

// TODO: Is BigUInt -> positive sign Int really a reasonable default?
impl From<&BigUint> for Int {
    fn from(int: &BigUint) -> Self {
        let mut bytes = int.to_bytes_be();
        let first_byte = match bytes.first_mut() {
            None => return Int::new(vec![0u8]),
            Some(first_byte) => first_byte,
        };
        if *first_byte > 0b0111_1111 {
            bytes.insert(0, 0b0000_0000);
        }
        Int::new(bytes)
    }
}

impl From<&BigUint> for UInt {
    fn from(int: &BigUint) -> Self {
        // Conveniently, BigUint::to_bytes_be produces the UInt byte representation.
        UInt::new(int.to_bytes_be())
    }
}

impl From<&BigInt> for VarInt {
    fn from(int: &BigInt) -> Self {
        match int.sign() {
            Sign::Minus => VarInt::new(serialize_var_int_parts(
                binary::Sign::Minus,
                int.magnitude(),
            )),
            Sign::NoSign | Sign::Plus => {
                VarInt::new(serialize_var_int_parts(binary::Sign::Plus, int.magnitude()))
            }
        }
    }
}

pub(crate) fn serialize_var_int_parts(sign: binary::Sign, magnitude: &BigUint) -> Vec<u8> {
    // bits needed to represent the magnitude of the VarInt
    let num_bits = magnitude.bits() as usize;

    // plus one more for the sign bit gives the total number of payload bits needed
    let payload_bits = num_bits + 1;

    // round up to the nearest multiple of 7 to get the number of bytes that will be needed
    let total_bytes = (payload_bits + 6) / 7;

    // the number of bits that will be available to store payload_bits
    let available_bits = total_bytes * 7;

    // compute the number of leading zeros that will be needed
    let leading_zeros = available_bits - payload_bits;

    // get the bytes from the BigUint
    let bytes = magnitude.to_bytes_be();

    // get the offset into the BitInt bytes where the num_bits meaningful bits begin
    let offset = bytes.len() * 8 - num_bits;

    let input_bits = BitVec::from_bytes(bytes.as_slice());
    let mut output_bits = BitVec::with_capacity(total_bytes * 8);

    // the first byte is special because it has a sign bit and may have leading zeros
    output_bits.push(total_bytes == 1);
    match sign {
        binary::Sign::Minus => output_bits.push(true),
        binary::Sign::Plus => output_bits.push(false),
    }
    // push leading zeros
    for _ in 0..leading_zeros {
        output_bits.push(false);
    }
    // fill the rest of the first byte
    let first_byte_bit_count = 6 - leading_zeros;
    for i in 0..first_byte_bit_count {
        output_bits.push(input_bits[offset + i]);
    }

    // fill the remaining bytes
    for i in 0..total_bytes - 1 {
        let index = first_byte_bit_count + offset + i * 7;
        // push 'true' if this is the last byte, else false
        output_bits.push(i == total_bytes - 2);

        // push the next 7 bits of payload, this will exactly reach the end of the payload bits
        output_bits.push(input_bits[index]);
        output_bits.push(input_bits[index + 1]);
        output_bits.push(input_bits[index + 2]);
        output_bits.push(input_bits[index + 3]);
        output_bits.push(input_bits[index + 4]);
        output_bits.push(input_bits[index + 5]);
        output_bits.push(input_bits[index + 6]);
    }

    output_bits.to_bytes()
}

impl From<&BigUint> for VarUInt {
    fn from(int: &BigUint) -> Self {
        // bits needed to represent the VarUint
        let payload_bits = int.bits() as usize;

        // round up to the nearest multiple of 7 to get the number of bytes that will be needed
        let total_bytes = (payload_bits + 6) / 7;

        // the number of bits that will be available to store payload_bits
        let available_bits = total_bytes * 7;

        // compute the number of leading zeros that will be needed
        let leading_zeros = available_bits - payload_bits;

        // get the bytes from the BigUint
        let bytes = int.to_bytes_be();

        // get the offset into the BigUint bytes where the payload_bits meaningful bits begin
        let offset = bytes.len() * 8 - payload_bits;

        let input_bits = BitVec::from_bytes(bytes.as_slice());
        let mut output_bits = BitVec::with_capacity(total_bytes * 8);

        // the first byte is special because it may have leading zeros
        output_bits.push(total_bytes == 1);
        // push leading zeros
        for _ in 0..leading_zeros {
            output_bits.push(false);
        }
        // fill the rest of the first byte
        let first_byte_bit_count = 7 - leading_zeros;
        for i in 0..first_byte_bit_count {
            output_bits.push(input_bits[offset + i]);
        }

        // fill the remaining bytes
        for i in 0..total_bytes - 1 {
            let index = first_byte_bit_count + offset + i * 7;
            // push 'true' if this is the last byte, else false
            output_bits.push(i == total_bytes - 2);

            // push the next 7 bits of payload, this will exactly reach the end of the payload bits
            output_bits.push(input_bits[index]);
            output_bits.push(input_bits[index + 1]);
            output_bits.push(input_bits[index + 2]);
            output_bits.push(input_bits[index + 3]);
            output_bits.push(input_bits[index + 4]);
            output_bits.push(input_bits[index + 5]);
            output_bits.push(input_bits[index + 6]);
        }

        VarUInt::new(output_bits.to_bytes())
    }
}

impl From<usize> for VarUInt {
    fn from(int: usize) -> Self {
        // A VarUint from usize is unlikely to need more than 4 bytes.
        let mut vec: Vec<u8> = Vec::with_capacity(4);
        append_var_uint_usize(&mut vec, int);
        VarUInt::new(vec)
    }
}

pub(crate) fn append_var_uint_u8(bytes: &mut Vec<u8>, value: u8) {
    // Serialization of VarUInts with reasonable sizes can be unrolled.
    match value {
        ONE_BYTE_VARUINT_RANGE_LOWER_U8..=ONE_BYTE_VARUINT_RANGE_UPPER_U8 => {
            bytes.push(value | VAR_UINT_END);
        }
        _ => {
            bytes.push((value >> 7) & VAR_UINT_CONTINUE);
            bytes.push(value | VAR_UINT_END);
        }
    }
}

pub(crate) fn append_var_uint_u16(bytes: &mut Vec<u8>, value: u16) {
    // Serialization of VarUInts with reasonable sizes can be unrolled.
    match value {
        ONE_BYTE_VARUINT_RANGE_LOWER_U16..=ONE_BYTE_VARUINT_RANGE_UPPER_U16 => {
            bytes.push(value as u8 | VAR_UINT_END);
        }
        TWO_BYTE_VARUINT_RANGE_LOWER_U16..=TWO_BYTE_VARUINT_RANGE_UPPER_U16 => {
            bytes.push((value >> 7) as u8 & VAR_UINT_CONTINUE);
            bytes.push(value as u8 | VAR_UINT_END);
        }
        _ => {
            bytes.push((value >> 14) as u8 & VAR_UINT_CONTINUE);
            bytes.push((value >> 7) as u8 & VAR_UINT_CONTINUE);
            bytes.push(value as u8 | VAR_UINT_END);
        }
    }
}

pub(crate) fn append_var_uint_usize(bytes: &mut Vec<u8>, value: usize) {
    // Serialization of VarUInts with reasonable sizes can be unrolled.
    match value {
        ONE_BYTE_VARUINT_RANGE_LOWER_USIZE..=ONE_BYTE_VARUINT_RANGE_UPPER_USIZE => {
            bytes.extend_from_slice(&serialize_1_byte_var_uint(value));
        }
        TWO_BYTE_VARUINT_RANGE_LOWER_USIZE..=TWO_BYTE_VARUINT_RANGE_UPPER_USIZE => {
            bytes.extend_from_slice(&serialize_2_byte_var_uint(value));
        }
        THREE_BYTE_VARUINT_RANGE_LOWER_USIZE..=THREE_BYTE_VARUINT_RANGE_UPPER_USIZE => {
            bytes.extend_from_slice(&serialize_3_byte_var_uint(value));
        }
        FOUR_BYTE_VARUINT_RANGE_LOWER_USIZE..=FOUR_BYTE_VARUINT_RANGE_UPPER_USIZE => {
            bytes.extend_from_slice(&serialize_4_byte_var_uint(value));
        }
        // Hypothetical 268MB+ Ion values don't merit special handling.
        _ => {
            bytes.append(&mut VarUInt::from(&BigUint::from(value)).into_bytes());
        }
    }
}

const VAR_UINT_CONTINUE: u8 = 0b0111_1111;
const VAR_UINT_END: u8 = 0b1000_0000;

// Ints that have 1 to 7 relevant bits, requiring 1 VarUInt byte.
pub(crate) const ONE_BYTE_VARUINT_RANGE_LOWER_U8: u8 = 0;
pub(crate) const ONE_BYTE_VARUINT_RANGE_UPPER_U8: u8 = 127;
pub(crate) const ONE_BYTE_VARUINT_RANGE_LOWER_U16: u16 = 0;
pub(crate) const ONE_BYTE_VARUINT_RANGE_UPPER_U16: u16 = 127;
pub(crate) const ONE_BYTE_VARUINT_RANGE_LOWER_USIZE: usize = 0;
pub(crate) const ONE_BYTE_VARUINT_RANGE_UPPER_USIZE: usize = 127;

// Applicable to ints that fit into 7 bits (size < 128).
fn serialize_1_byte_var_uint(int: usize) -> [u8; 1] {
    debug_assert!(
        int <= 127,
        "Cannot convert int >= 128 to VarUInt of 1 byte."
    );
    [int as u8 | VAR_UINT_END]
}

// Ints that have 8 to 14 relevant bits, requiring 2 VarUInt bytes.
pub(crate) const TWO_BYTE_VARUINT_RANGE_LOWER_U16: u16 = 128;
pub(crate) const TWO_BYTE_VARUINT_RANGE_UPPER_U16: u16 = 16_383;
pub(crate) const TWO_BYTE_VARUINT_RANGE_LOWER_USIZE: usize = 128;
pub(crate) const TWO_BYTE_VARUINT_RANGE_UPPER_USIZE: usize = 16_383;

// Applicable to ints that fit into 14 bits (size < 16384).
fn serialize_2_byte_var_uint(int: usize) -> [u8; 2] {
    debug_assert!(
        int <= TWO_BYTE_VARUINT_RANGE_UPPER_USIZE,
        "Cannot convert int>= 16384 to VarUInt of 2 bytes."
    );
    [
        (int >> 7) as u8 & VAR_UINT_CONTINUE,
        int as u8 | VAR_UINT_END,
    ]
}

// Ints that have 15 to 21 relevant bits, requiring 3 VarUInt bytes.
pub(crate) const THREE_BYTE_VARUINT_RANGE_LOWER_USIZE: usize = 16_384;
pub(crate) const THREE_BYTE_VARUINT_RANGE_UPPER_USIZE: usize = 2_097_151;

// Applicable to ints that fit into 21 bits (size < 2097152).
fn serialize_3_byte_var_uint(int: usize) -> [u8; 3] {
    debug_assert!(
        int <= THREE_BYTE_VARUINT_RANGE_UPPER_USIZE,
        "Cannot convert int>= 2097152 to VarUInt of 3 bytes."
    );
    [
        (int >> 14) as u8 & VAR_UINT_CONTINUE,
        (int >> 7) as u8 & VAR_UINT_CONTINUE,
        int as u8 | VAR_UINT_END,
    ]
}

// Ints that have 22 to 28 relevant bits, requiring 4 VarUInt bytes.
pub(crate) const FOUR_BYTE_VARUINT_RANGE_LOWER_USIZE: usize = 2_097_152;
pub(crate) const FOUR_BYTE_VARUINT_RANGE_UPPER_USIZE: usize = 268_435_455;

// Applicable to ints that fit into 28 bits (size < 268435456).
fn serialize_4_byte_var_uint(int: usize) -> [u8; 4] {
    debug_assert!(
        int <= FOUR_BYTE_VARUINT_RANGE_UPPER_USIZE,
        "Cannot convert int >= 268435456 to VarUInt of 4 bytes."
    );
    [
        (int >> 21) as u8 & VAR_UINT_CONTINUE,
        (int >> 14) as u8 & VAR_UINT_CONTINUE,
        (int >> 7) as u8 & VAR_UINT_CONTINUE,
        int as u8 | VAR_UINT_END,
    ]
}

#[allow(non_snake_case)]
#[cfg(test)]
mod tests {
    use hex::decode;
    use pretty_assertions::assert_eq;

    use super::*;

    /// Examples from tests/ion-tests/iontestdata/good/subfieldInt.ion

    #[test]
    fn test_subfieldInt_0x7f() {
        // 1 byte
        // 7 bits
        // hex: 0x7f
        // dec: 127
        let written_bytes = Int::from(&BigInt::from(127)).into_bytes();
        let bytes: &[u8] = &decode("7f").unwrap();
        assert_eq!(bytes, written_bytes.as_slice());
    }

    #[test]
    fn test_subfieldInt_0x7fff() {
        // 2 bytes
        // 15 bits
        // hex: 0x7fff
        // dec: 32767
        let written_bytes = Int::from(&BigInt::from(32767)).into_bytes();
        let bytes: &[u8] = &decode("7fff").unwrap();
        assert_eq!(bytes, written_bytes.as_slice());
    }

    #[test]
    fn test_subfieldInt_0x7fffff() {
        // 3 bytes
        // 23 bits
        // hex: 0x7fffff
        // dec: 8388607
        let written_bytes = Int::from(&BigInt::from(8_388_607)).into_bytes();
        let bytes: &[u8] = &decode("7fffff").unwrap();
        assert_eq!(bytes, written_bytes.as_slice());
    }

    #[test]
    fn test_subfieldInt_0x7fffffff() {
        // 4 bytes
        // 31 bits
        // hex: 0x7fffffff
        // dec: 2147483647
        let written_bytes = Int::from(&BigInt::from(2_147_483_647)).into_bytes();
        let bytes: &[u8] = &decode("7fffffff").unwrap();
        assert_eq!(bytes, written_bytes.as_slice());
    }

    #[test]
    fn test_subfieldInt_0x7fffffffff() {
        // 5 bytes
        // 39 bits
        // hex: 0x7fffffffff
        // dec: 549755813887
        let written_bytes = Int::from(&BigInt::from(549_755_813_887i64)).into_bytes();
        let bytes: &[u8] = &decode("7fffffffff").unwrap();
        assert_eq!(bytes, written_bytes.as_slice());
    }

    #[test]
    fn test_subfieldInt_0x7fffffffffff() {
        // 6 bytes
        // 47 bits
        // hex: 0x7fffffffffff
        // dec: 140737488355327
        let written_bytes = Int::from(&BigInt::from(140_737_488_355_327i64)).into_bytes();
        let bytes: &[u8] = &decode("7fffffffffff").unwrap();
        assert_eq!(bytes, written_bytes.as_slice());
    }

    #[test]
    fn test_subfieldInt_0x7fffffffffffff() {
        // 7 bytes
        // 55 bits
        // hex: 0x7fffffffffffff
        // dec: 36028797018963967
        let written_bytes = Int::from(&BigInt::from(36_028_797_018_963_967i64)).into_bytes();
        let bytes: &[u8] = &decode("7fffffffffffff").unwrap();
        assert_eq!(bytes, written_bytes.as_slice());
    }

    #[test]
    fn test_subfieldInt_0x7fffffffffffffff() {
        // 8 bytes
        // 63 bits
        // hex: 0x7fffffffffffffff
        // dec: 9223372036854775807
        let written_bytes = Int::from(&BigInt::from(9_223_372_036_854_775_807i64)).into_bytes();
        let bytes: &[u8] = &decode("7fffffffffffffff").unwrap();
        assert_eq!(bytes, written_bytes.as_slice());
    }

    #[test]
    fn test_subfieldInt_0x00ffffffffffffffff() {
        // 9 bytes
        // 64 bits
        // hex: 0x00ffffffffffffffff
        // dec: 18446744073709551615
        let written_bytes = Int::from(&BigInt::from(18_446_744_073_709_551_615i128)).into_bytes();
        let bytes: &[u8] = &decode("00ffffffffffffffff").unwrap();
        assert_eq!(bytes, written_bytes.as_slice());
    }

    #[test]
    fn test_subfieldInt_0xfffffffffffffff() {
        // 9 bytes
        // 71 bits
        // hex: 0xfffffffffffffff
        // dec: 2361183241434822606847
        let written_bytes =
            Int::from(&BigInt::from(2_361_183_241_434_822_606_847i128)).into_bytes();
        let bytes: &[u8] = &decode("7fffffffffffffffff").unwrap();
        assert_eq!(bytes, written_bytes.as_slice());
    }

    #[test]
    fn test_subfieldInt_0x7fffffffffffffffffff() {
        // 10 bytes
        // 79 bits used
        // hex: 0x7fffffffffffffffffff
        // dec: 604462909807314587353087
        let written_bytes =
            Int::from(&BigInt::from(604_462_909_807_314_587_353_087i128)).into_bytes();
        let bytes: &[u8] = &decode("7fffffffffffffffffff").unwrap();
        assert_eq!(bytes, written_bytes.as_slice());
    }

    /// Examples from tests/ion-tests/iontestdata/good/subfieldUint.ion

    #[test]
    fn test_subfieldUint_0xff() {
        // 1 byte
        // 8 bits
        // hex: 0xff
        // dec: 255
        let written_bytes = UInt::from(&BigUint::from(255u32)).into_bytes();
        let bytes: &[u8] = &decode("ff").unwrap();
        assert_eq!(bytes, written_bytes.as_slice());
    }

    #[test]
    fn test_subfieldUint_0xffff() {
        // 2 bytes
        // 16 bits
        // hex: 0xffff
        // dec: 65535
        let written_bytes = UInt::from(&BigUint::from(65535u32)).into_bytes();
        let bytes: &[u8] = &decode("ffff").unwrap();
        assert_eq!(bytes, written_bytes.as_slice());
    }

    #[test]
    fn test_subfieldUint_0xffffff() {
        // 3 bytes
        // 24 bits
        // hex: 0xffffff
        // dec: 16777215
        let written_bytes = UInt::from(&BigUint::from(16_777_215u32)).into_bytes();
        let bytes: &[u8] = &decode("ffffff").unwrap();
        assert_eq!(bytes, written_bytes.as_slice());
    }

    #[test]
    fn test_subfieldUint_0x7fffffff() {
        // 4 bytes
        // 31 bits
        // hex: 0x7fffffff
        // dec: 2147483647
        let written_bytes = UInt::from(&BigUint::from(2_147_483_647u32)).into_bytes();
        let bytes: &[u8] = &decode("7fffffff").unwrap();
        assert_eq!(bytes, written_bytes.as_slice());
    }

    #[test]
    fn test_subfieldUint_0xffffffff() {
        // 4 bytes
        // 32 bits
        // hex: 0xffffffff
        // dec: 4294967295
        let written_bytes = UInt::from(&BigUint::from(4_294_967_295u32)).into_bytes();
        let bytes: &[u8] = &decode("ffffffff").unwrap();
        assert_eq!(bytes, written_bytes.as_slice());
    }

    #[test]
    fn test_subfieldUint_0xffffffffff() {
        // 5 bytes
        // 40 bits
        // hex: 0xffffffffff
        // dec: 1099511627775
        let written_bytes = UInt::from(&BigUint::from(1_099_511_627_775u64)).into_bytes();
        let bytes: &[u8] = &decode("ffffffffff").unwrap();
        assert_eq!(bytes, written_bytes.as_slice());
    }

    #[test]
    fn test_subfieldUint_0xffffffffffff() {
        // 6 bytes
        // 48 bits
        // hex: 0xffffffffffff
        // dec: 281474976710655
        let written_bytes = UInt::from(&BigUint::from(281_474_976_710_655u64)).into_bytes();
        let bytes: &[u8] = &decode("ffffffffffff").unwrap();
        assert_eq!(bytes, written_bytes.as_slice());
    }

    #[test]
    fn test_subfieldUint_0xffffffffffffff() {
        // 7 bytes
        // 56 bits
        // hex: 0xffffffffffffff
        // dec: 72057594037927935
        let written_bytes = UInt::from(&BigUint::from(72_057_594_037_927_935u64)).into_bytes();
        let bytes: &[u8] = &decode("ffffffffffffff").unwrap();
        assert_eq!(bytes, written_bytes.as_slice());
    }

    #[test]
    fn test_subfieldUint_0x7fffffffffffffff() {
        // 8 bytes
        // 63 bits
        // hex: 0x7fffffffffffffff
        // dec: 9223372036854775807
        let written_bytes = UInt::from(&BigUint::from(9_223_372_036_854_775_807u64)).into_bytes();
        let bytes: &[u8] = &decode("7fffffffffffffff").unwrap();
        assert_eq!(bytes, written_bytes.as_slice());
    }

    #[test]
    fn test_subfieldUint_0xffffffffffffffff() {
        // 8 bytes
        // 64 bits
        // hex: 0xffffffffffffffff
        // dec: 18446744073709551615
        let written_bytes = UInt::from(&BigUint::from(18_446_744_073_709_551_615u64)).into_bytes();
        let bytes: &[u8] = &decode("ffffffffffffffff").unwrap();
        assert_eq!(bytes, written_bytes.as_slice());
    }

    #[test]
    fn test_subfieldUint_0xffffffffffffffffff() {
        // 9 bytes
        // 72 bits
        // hex: 0xffffffffffffffffff
        // dec: 4722366482869645213695
        let written_bytes =
            UInt::from(&BigUint::from(4_722_366_482_869_645_213_695u128)).into_bytes();
        let bytes: &[u8] = &decode("ffffffffffffffffff").unwrap();
        assert_eq!(bytes, written_bytes.as_slice());
    }

    #[test]
    fn test_subfieldUint_0xffffffffffffffffffff() {
        // 10 bytes
        // 80 bits
        // hex: 0xffffffffffffffffffff
        // dec: 1208925819614629174706175
        let written_bytes =
            UInt::from(&BigUint::from(1_208_925_819_614_629_174_706_175u128)).into_bytes();
        let bytes: &[u8] = &decode("ffffffffffffffffffff").unwrap();
        assert_eq!(bytes, written_bytes.as_slice());
    }

    /// Examples from tests/ion-tests/iontestdata/good/subfieldVarInt.ion

    #[test]
    fn test_subfieldVarInt_0xbf() {
        // 1 byte
        // 6 bits
        // hex: 0xbf
        // dec: 63
        let written_bytes = VarInt::from(&BigInt::from(63)).into_bytes();
        let bytes: &[u8] = &decode("bf").unwrap();
        assert_eq!(bytes, written_bytes.as_slice());
    }

    #[test]
    fn test_subfieldVarInt_0x3fff() {
        // 2 bytes
        // 13 bits
        // hex: 0x3fff
        // dec: 8191
        let written_bytes = VarInt::from(&BigInt::from(8191)).into_bytes();
        let bytes: &[u8] = &decode("3fff").unwrap();
        assert_eq!(bytes, written_bytes.as_slice());
    }

    #[test]
    fn test_subfieldVarInt_0x3f7fff() {
        // 3 bytes
        // 20 bits
        // hex: 0x3f7fff
        // dec: 1048575
        let written_bytes = VarInt::from(&BigInt::from(1_048_575)).into_bytes();
        let bytes: &[u8] = &decode("3f7fff").unwrap();
        assert_eq!(bytes, written_bytes.as_slice());
    }

    #[test]
    fn test_subfieldVarInt_0x3f7f7fff() {
        // 4 bytes
        // 27 bits
        // hex: 0x3f7f7fff
        // dec: 134217727
        let written_bytes = VarInt::from(&BigInt::from(134_217_727)).into_bytes();
        let bytes: &[u8] = &decode("3f7f7fff").unwrap();
        assert_eq!(bytes, written_bytes.as_slice());
    }

    #[test]
    fn test_subfieldVarInt_0x077f7f7fff() {
        // 5 bytes
        // 31 bits
        // hex: 0x077f7f7fff
        // dec: 2147483647
        let written_bytes = VarInt::from(&BigInt::from(2_147_483_647)).into_bytes();
        let bytes: &[u8] = &decode("077f7f7fff").unwrap();
        assert_eq!(bytes, written_bytes.as_slice());
    }

    #[test]
    fn test_subfieldVarInt_0xa0() {
        // 1 byte
        // hex: 0xa0
        // dec: 32
        let written_bytes = VarInt::from(&BigInt::from(32)).into_bytes();
        let bytes: &[u8] = &decode("a0").unwrap();
        assert_eq!(bytes, written_bytes.as_slice());
    }

    #[test]
    fn test_subfieldVarInt_0x2080() {
        // 2 bytes
        // hex: 0x2080
        // dec: 4096
        let written_bytes = VarInt::from(&BigInt::from(4096)).into_bytes();
        let bytes: &[u8] = &decode("2080").unwrap();
        assert_eq!(bytes, written_bytes.as_slice());
    }

    /// Examples from
    /// tests/ion-tests/iontestdata/good/subfieldVarUint.ion
    /// tests/ion-tests/iontestdata/good/subfieldVarUint15bit.ion
    /// tests/ion-tests/iontestdata/good/subfieldVarUint16bit.ion
    /// tests/ion-tests/iontestdata/good/subfieldVarUint32bit.ion

    #[test]
    fn test_subfieldVarUint_0xff() {
        // 1 byte
        // 7 bits
        // hex: 0xff
        // dec: 127
        let written_bytes = VarUInt::from(&BigUint::from(127u32)).into_bytes();
        let bytes: &[u8] = &decode("ff").unwrap();
        assert_eq!(bytes, written_bytes.as_slice());
        assert_eq!(bytes, serialize_1_byte_var_uint(127));

        let mut byte_stream: Vec<u8> = vec![];
        append_var_uint_usize(&mut byte_stream, 127);
        assert_eq!(bytes, byte_stream.as_slice());
    }

    #[test]
    fn test_subfieldVarUint_0x7fff() {
        // 2 bytes
        // 14 bits
        // hex: 0x7fff
        // dec: 16383
        let written_bytes = VarUInt::from(&BigUint::from(16383u32)).into_bytes();
        let bytes: &[u8] = &decode("7fff").unwrap();
        assert_eq!(bytes, written_bytes.as_slice());
        assert_eq!(bytes, serialize_2_byte_var_uint(16383));

        let mut byte_stream: Vec<u8> = vec![];
        append_var_uint_usize(&mut byte_stream, 16383);
        assert_eq!(bytes, byte_stream.as_slice());
    }

    #[test]
    fn test_subfieldVarUint_0x017fff() {
        // 3 bytes
        // 15 bits
        // hex: 0x017fff
        // dec: 32767
        let written_bytes = VarUInt::from(&BigUint::from(32767u32)).into_bytes();
        let bytes: &[u8] = &decode("017fff").unwrap();
        assert_eq!(bytes, written_bytes.as_slice());
        assert_eq!(bytes, serialize_3_byte_var_uint(32767));

        let mut byte_stream: Vec<u8> = vec![];
        append_var_uint_usize(&mut byte_stream, 32767);
        assert_eq!(bytes, byte_stream.as_slice());
    }

    #[test]
    fn test_subfieldVarUint_0x037fff() {
        // 3 bytes
        // 16 bits
        // hex: 0x037fff
        // dec: 65535
        let written_bytes = VarUInt::from(&BigUint::from(65535u32)).into_bytes();
        let bytes: &[u8] = &decode("037fff").unwrap();
        assert_eq!(bytes, written_bytes.as_slice());
        assert_eq!(bytes, serialize_3_byte_var_uint(65535));

        let mut byte_stream: Vec<u8> = vec![];
        append_var_uint_usize(&mut byte_stream, 65535);
        assert_eq!(bytes, byte_stream.as_slice());
    }

    #[test]
    fn test_subfieldVarUint_0x7f7fff() {
        // 3 bytes
        // 21 bits
        // hex: 0x7f7fff
        // dec: 2097151
        let written_bytes = VarUInt::from(&BigUint::from(2_097_151u32)).into_bytes();
        let bytes: &[u8] = &decode("7f7fff").unwrap();
        assert_eq!(bytes, written_bytes.as_slice());
        assert_eq!(bytes, serialize_3_byte_var_uint(2_097_151));

        let mut byte_stream: Vec<u8> = vec![];
        append_var_uint_usize(&mut byte_stream, 2_097_151);
        assert_eq!(bytes, byte_stream.as_slice());
    }
    #[test]
    fn test_subfieldVarUint_0x7f7f7fff() {
        // 4 bytes
        // 28 bits
        // hex: 0x7f7f7fff
        // dec: 268435455
        let written_bytes = VarUInt::from(&BigUint::from(268_435_455u32)).into_bytes();
        let bytes: &[u8] = &decode("7f7f7fff").unwrap();
        assert_eq!(bytes, written_bytes.as_slice());
        assert_eq!(bytes, serialize_4_byte_var_uint(268_435_455));

        let mut byte_stream: Vec<u8> = vec![];
        append_var_uint_usize(&mut byte_stream, 268_435_455);
        assert_eq!(bytes, byte_stream.as_slice());
    }

    #[test]
    fn test_subfieldVarUint_0x077f7f7fff() {
        // 5 bytes
        // 31 bits
        // hex: 0x077f7f7fff
        // dec: 2147483647
        let written_bytes = VarUInt::from(&BigUint::from(2_147_483_647u32)).into_bytes();
        let bytes: &[u8] = &decode("077f7f7fff").unwrap();
        assert_eq!(bytes, written_bytes.as_slice());

        let mut byte_stream: Vec<u8> = vec![];
        append_var_uint_usize(&mut byte_stream, 2_147_483_647);
        assert_eq!(bytes, byte_stream.as_slice());
    }
}
