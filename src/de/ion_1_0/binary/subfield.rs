use std::iter;

use crate::binary;
use bit_vec::BitVec;
use nom::{
    bytes::complete::{take, take_while},
    error::{ErrorKind, ParseError},
    Err,
};
use num_bigint::{BigInt, BigUint, Sign};
use num_traits::{cast::ToPrimitive, identities::Zero};

use crate::error::{IonError, IonResult};

/// ## Basic Field Formats
///
/// Binary-encoded Ion values are comprised of one or more fields, and the fields use a small number
/// of basic formats (separate from the Ion types visible to users).

/// ### UInt and Int Fields
///
/// UInt and Int fields represent fixed-length unsigned and signed integer values.
/// These field formats are always used in some context that clearly indicates the
/// number of octets in the field.
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
///
/// UInts are sequences of octets, interpreted as big-endian.
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
///
/// Ints are sequences of octets, interpreted as sign-and-magnitude big endian integers (with the sign
/// on the highest-order bit of the first octet). This means that the representations of
/// 123456 and -123456 should only differ in their sign bit.
///
/// ## VarUInt and VarInt Fields
///
/// VarUInt and VarInt fields represent self-delimiting, variable-length unsigned and signed integer
/// values. These field formats are always used in a context that does not indicate the number of octets
/// in the field; the last octet (and only the last octet) has its high-order bit set to
/// terminate the field.
///
/// ```text
///                 7  6                   0       n+7 n+6                 n
///               +===+=====================+     +---+---------------------+
/// VarUInt field : 0 :         bits        :  …  | 1 |         bits        |
///               +===+=====================+     +---+---------------------+
/// ```
///
/// VarUInts are a sequence of octets. The high-order bit of the last octet is one,
/// indicating the end of the sequence. All other high-order bits must be zero.
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
///                             7   6  5           0
///                           +---+---+-------------+
/// single octet VarInt field | 1 |   |  magnitude  |
///                           +---+---+-------------+
///                                 ^
///                                 |
///                                 +--sign
/// ```

pub(crate) fn take_int(length: usize) -> impl Fn(&[u8]) -> IonResult<&[u8], num_bigint::BigInt> {
    move |i: &[u8]| {
        let (rest, bytes) = take(length)(i)?;
        Ok((rest, parse_int(bytes)))
    }
}

pub(crate) fn parse_int(bytes: &[u8]) -> num_bigint::BigInt {
    let sign = match bytes.first() {
        Some(v) if *v > 0b0111_1111 => Sign::Minus,
        Some(_) => Sign::Plus,
        None => return BigInt::zero(),
    };

    if sign == Sign::Minus {
        // TODO: Performance
        let mut bytes = Vec::from(bytes);
        bytes[0] ^= 0b1000_0000; // clear the high bit to get the magnitude
        BigInt::from_biguint(sign, BigUint::from_bytes_be(&*bytes))
    } else {
        BigInt::from_biguint(sign, BigUint::from_bytes_be(bytes))
    }
}

pub(crate) fn take_uint(length: usize) -> impl Fn(&[u8]) -> IonResult<&[u8], num_bigint::BigUint> {
    move |i: &[u8]| {
        let (input, bytes) = take(length)(i)?;
        Ok((input, BigUint::from_bytes_be(bytes)))
    }
}

pub(crate) fn parse_uint(bytes: &[u8]) -> num_bigint::BigUint {
    BigUint::from_bytes_be(bytes)
}

pub(crate) fn take_var_int(i: &[u8]) -> IonResult<&[u8], (binary::Sign, num_bigint::BigUint)> {
    let (input, sequence) = take_while(high_bit_unset)(i)?;
    let (input, terminator) = take(1usize)(input)?;
    Ok((input, parse_var_int(sequence, terminator[0])))
}

pub(crate) fn take_var_int_as_num_bigint(i: &[u8]) -> IonResult<&[u8], num_bigint::BigInt> {
    let (input, sequence) = take_while(high_bit_unset)(i)?;
    let (input, terminator) = take(1usize)(input)?;
    Ok((input, parse_var_int_as_num_bigint(sequence, terminator[0])))
}

// There are scenarios (ex. timestamp exponent values) where allowing a VarInt that cannot fit in
// i32 is unreasonable. This should not pose an issue for non-pathological use cases.
// TODO: obvious room for performance improvement
pub(crate) fn take_var_int_as_i32(i: &[u8]) -> IonResult<&[u8], i32> {
    let (rest, sequence) = take_while(high_bit_unset)(i)?;
    let (rest, terminator) = take(1usize)(rest)?;
    let value = parse_var_int_as_num_bigint(sequence, terminator[0]);
    match value.to_i32() {
        Some(value) => Ok((rest, value)),
        None => Err(Err::Failure(IonError::from_error_kind(
            rest,
            ErrorKind::TooLarge,
        ))),
    }
}

// num_bigint::BigInt doesn't preserve the distinction between positive and negative zero,
// but this distinction exists in VarInts and is relevant when parsing Timestamps.
pub(crate) fn parse_var_int(sequence: &[u8], terminator: u8) -> (binary::Sign, BigUint) {
    let sign = match sequence.first() {
        Some(byte) => {
            // we know that no byte in the sequence has the high bit set
            if *byte > 0b0011_1111 {
                binary::Sign::Minus
            } else {
                binary::Sign::Plus
            }
        }
        None => {
            // we know that the terminator byte has the high bit set
            if terminator > 0b1011_1111 {
                binary::Sign::Minus
            } else {
                binary::Sign::Plus
            }
        }
    };

    // total number of payload bits in the VarInt
    let payload_bits = 7 * (sequence.len() + 1);

    // round payload_bits up to the nearest multiple of 8
    let bit_capacity = (payload_bits + 8 - 1) & (usize::MAX << 3);

    let mut bits = BitVec::with_capacity(bit_capacity);

    let leading_bits = bit_capacity - payload_bits;

    // zero the extra leading bits
    for _ in 0..leading_bits {
        bits.push(false);
    }

    // insert all payload bits
    for byte in sequence.iter().chain(iter::once(&terminator)) {
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

    (sign, BigUint::from_bytes_be(&*bits.to_bytes()))
}

pub(crate) fn parse_var_int_as_num_bigint(sequence: &[u8], terminator: u8) -> num_bigint::BigInt {
    let (sign, biguint): (binary::Sign, BigUint) = parse_var_int(sequence, terminator);
    BigInt::from_biguint(
        match sign {
            binary::Sign::Minus => num_bigint::Sign::Minus,
            binary::Sign::Plus => num_bigint::Sign::Plus,
        },
        biguint,
    )
}

pub(crate) fn take_var_uint(i: &[u8]) -> IonResult<&[u8], num_bigint::BigUint> {
    let (input, sequence) = take_while(high_bit_unset)(i)?;
    let (input, terminator) = take(1usize)(input)?;
    Ok((input, parse_var_uint(sequence, terminator[0])))
}

// There are scenarios (ex. byte-length tags) where a VarUint that cannot fit in usize is unreasonable.
// TODO: obvious room for performance improvement
pub(crate) fn take_var_uint_as_usize(i: &[u8]) -> IonResult<&[u8], usize> {
    let (rest, sequence) = take_while(high_bit_unset)(i)?;
    let (rest, terminator) = take(1usize)(rest)?;
    let value = parse_var_uint(sequence, terminator[0]);
    match value.to_usize() {
        Some(value) => Ok((rest, value)),
        None => Err(Err::Failure(IonError::from_error_kind(
            rest,
            ErrorKind::TooLarge,
        ))),
    }
}

pub(crate) fn parse_var_uint(sequence: &[u8], terminator: u8) -> num_bigint::BigUint {
    // total number of payload bits in the VarUInt
    let payload_bits = 7 * (sequence.len() + 1);

    // round payload_bits up to the nearest multiple of 8
    let bit_capacity = (payload_bits + 8 - 1) & (usize::MAX << 3);

    let mut bits = BitVec::with_capacity(bit_capacity);

    let leading_bits = bit_capacity - payload_bits;

    // zero the extra leading bits
    for _ in 0..leading_bits {
        bits.push(false);
    }

    // insert all payload bits
    for byte in sequence.iter().chain(iter::once(&terminator)) {
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

// TODO: Profile and see if #[inline] directives are needed here and elsewhere.
fn high_bit_unset(byte: u8) -> bool {
    byte < 0b1000_0000
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
        let bytes: &[u8] = &decode("7f").unwrap();
        let int = parse_int(bytes);
        assert_eq!(int, BigInt::from(127));
    }

    #[test]
    fn test_subfieldInt_0x7fff() {
        // 2 bytes
        // 15 bits
        // hex: 0x7fff
        // dec: 32767
        let bytes: &[u8] = &decode("7fff").unwrap();
        let int = parse_int(bytes);
        assert_eq!(int, BigInt::from(32767));
    }

    #[test]
    fn test_subfieldInt_0x7fffff() {
        // 3 bytes
        // 23 bits
        // hex: 0x7fffff
        // dec: 8388607
        let bytes: &[u8] = &decode("7fffff").unwrap();
        let int = parse_int(bytes);
        assert_eq!(int, BigInt::from(8_388_607));
    }

    #[test]
    fn test_subfieldInt_0x7fffffff() {
        // 4 bytes
        // 31 bits
        // hex: 0x7fffffff
        // dec: 2147483647
        let bytes: &[u8] = &decode("7fffffff").unwrap();
        let int = parse_int(bytes);
        assert_eq!(int, BigInt::from(2_147_483_647));
    }

    #[test]
    fn test_subfieldInt_0x7fffffffff() {
        // 5 bytes
        // 39 bits
        // hex: 0x7fffffffff
        // dec: 549755813887
        let bytes: &[u8] = &decode("7fffffffff").unwrap();
        let int = parse_int(bytes);
        assert_eq!(int, BigInt::from(549_755_813_887i64));
    }

    #[test]
    fn test_subfieldInt_0x7fffffffffff() {
        // 6 bytes
        // 47 bits
        // hex: 0x7fffffffffff
        // dec: 140737488355327
        let bytes: &[u8] = &decode("7fffffffffff").unwrap();
        let int = parse_int(bytes);
        assert_eq!(int, BigInt::from(140_737_488_355_327i64));
    }

    #[test]
    fn test_subfieldInt_0x7fffffffffffff() {
        // 7 bytes
        // 55 bits
        // hex: 0x7fffffffffffff
        // dec: 36028797018963967
        let bytes: &[u8] = &decode("7fffffffffffff").unwrap();
        let int = parse_int(bytes);
        assert_eq!(int, BigInt::from(36_028_797_018_963_967i64));
    }

    #[test]
    fn test_subfieldInt_0x7fffffffffffffff() {
        // 8 bytes
        // 63 bits
        // hex: 0x7fffffffffffffff
        // dec: 9223372036854775807
        let bytes: &[u8] = &decode("7fffffffffffffff").unwrap();
        let int = parse_int(bytes);
        assert_eq!(int, BigInt::from(9_223_372_036_854_775_807i64));
    }

    #[test]
    fn test_subfieldInt_0x00ffffffffffffffff() {
        // 9 bytes
        // 64 bits
        // hex: 0x00ffffffffffffffff
        // dec: 18446744073709551615
        let bytes: &[u8] = &decode("00ffffffffffffffff").unwrap();
        let int = parse_int(bytes);
        assert_eq!(int, BigInt::from(18_446_744_073_709_551_615i128));
    }

    #[test]
    fn test_subfieldInt_0xfffffffffffffff() {
        // 9 bytes
        // 71 bits
        // hex: 0xfffffffffffffff
        // dec: 2361183241434822606847
        let bytes: &[u8] = &decode("7fffffffffffffffff").unwrap();
        let int = parse_int(bytes);
        assert_eq!(int, BigInt::from(2_361_183_241_434_822_606_847i128));
    }

    #[test]
    fn test_subfieldInt_0x7fffffffffffffffffff() {
        // 10 bytes
        // 79 bits used
        // hex: 0x7fffffffffffffffffff
        // dec: 604462909807314587353087
        let bytes: &[u8] = &decode("7fffffffffffffffffff").unwrap();
        let int = parse_int(bytes);
        assert_eq!(int, BigInt::from(604_462_909_807_314_587_353_087i128));
    }

    /// Examples from tests/ion-tests/iontestdata/good/subfieldUint.ion

    #[test]
    fn test_subfieldUint_0xff() {
        // 1 byte
        // 8 bits
        // hex: 0xff
        // dec: 255
        let bytes: &[u8] = &decode("ff").unwrap();
        let uint = parse_uint(bytes);
        assert_eq!(uint, BigUint::from(255u32));
    }

    #[test]
    fn test_subfieldUint_0xffff() {
        // 2 bytes
        // 16 bits
        // hex: 0xffff
        // dec: 65535
        let bytes: &[u8] = &decode("ffff").unwrap();
        let uint = parse_uint(bytes);
        assert_eq!(uint, BigUint::from(65535u32));
    }

    #[test]
    fn test_subfieldUint_0xffffff() {
        // 3 bytes
        // 24 bits
        // hex: 0xffffff
        // dec: 16777215
        let bytes: &[u8] = &decode("ffffff").unwrap();
        let uint = parse_uint(bytes);
        assert_eq!(uint, BigUint::from(16_777_215u32));
    }

    #[test]
    fn test_subfieldUint_0x7fffffff() {
        // 4 bytes
        // 31 bits
        // hex: 0x7fffffff
        // dec: 2147483647
        let bytes: &[u8] = &decode("7fffffff").unwrap();
        let uint = parse_uint(bytes);
        assert_eq!(uint, BigUint::from(2_147_483_647u32));
    }

    #[test]
    fn test_subfieldUint_0xffffffff() {
        // 4 bytes
        // 32 bits
        // hex: 0xffffffff
        // dec: 4294967295
        let bytes: &[u8] = &decode("ffffffff").unwrap();
        let uint = parse_uint(bytes);
        assert_eq!(uint, BigUint::from(4_294_967_295u32));
    }

    #[test]
    fn test_subfieldUint_0xffffffffff() {
        // 5 bytes
        // 40 bits
        // hex: 0xffffffffff
        // dec: 1099511627775
        let bytes: &[u8] = &decode("ffffffffff").unwrap();
        let uint = parse_uint(bytes);
        assert_eq!(uint, BigUint::from(1_099_511_627_775u64));
    }

    #[test]
    fn test_subfieldUint_0xffffffffffff() {
        // 6 bytes
        // 48 bits
        // hex: 0xffffffffffff
        // dec: 281474976710655
        let bytes: &[u8] = &decode("ffffffffffff").unwrap();
        let uint = parse_uint(bytes);
        assert_eq!(uint, BigUint::from(281_474_976_710_655u64));
    }

    #[test]
    fn test_subfieldUint_0xffffffffffffff() {
        // 7 bytes
        // 56 bits
        // hex: 0xffffffffffffff
        // dec: 72057594037927935
        let bytes: &[u8] = &decode("ffffffffffffff").unwrap();
        let uint = parse_uint(bytes);
        assert_eq!(uint, BigUint::from(72_057_594_037_927_935u64));
    }

    #[test]
    fn test_subfieldUint_0x7fffffffffffffff() {
        // 8 bytes
        // 63 bits
        // hex: 0x7fffffffffffffff
        // dec: 9223372036854775807
        let bytes: &[u8] = &decode("7fffffffffffffff").unwrap();
        let uint = parse_uint(bytes);
        assert_eq!(uint, BigUint::from(9_223_372_036_854_775_807u64));
    }

    #[test]
    fn test_subfieldUint_0xffffffffffffffff() {
        // 8 bytes
        // 64 bits
        // hex: 0xffffffffffffffff
        // dec: 18446744073709551615
        let bytes: &[u8] = &decode("ffffffffffffffff").unwrap();
        let uint = parse_uint(bytes);
        assert_eq!(uint, BigUint::from(18_446_744_073_709_551_615u64));
    }

    #[test]
    fn test_subfieldUint_0xffffffffffffffffff() {
        // 9 bytes
        // 72 bits
        // hex: 0xffffffffffffffffff
        // dec: 4722366482869645213695
        let bytes: &[u8] = &decode("ffffffffffffffffff").unwrap();
        let uint = parse_uint(bytes);
        assert_eq!(uint, BigUint::from(4_722_366_482_869_645_213_695u128));
    }

    #[test]
    fn test_subfieldUint_0xffffffffffffffffffff() {
        // 10 bytes
        // 80 bits
        // hex: 0xffffffffffffffffffff
        // dec: 1208925819614629174706175
        let bytes: &[u8] = &decode("ffffffffffffffffffff").unwrap();
        let uint = parse_uint(bytes);
        assert_eq!(uint, BigUint::from(1_208_925_819_614_629_174_706_175u128));
    }

    /// Examples from tests/ion-tests/iontestdata/good/subfieldVarInt.ion

    #[test]
    fn test_subfieldVarInt_0xbf() {
        // 1 byte
        // 6 bits
        // hex: 0xbf
        // dec: 63
        let bytes: &[u8] = &decode("bf").unwrap();
        let sequence = &bytes[..bytes.len() - 1];
        let terminator = bytes[bytes.len() - 1];
        let varint = parse_var_int_as_num_bigint(sequence, terminator);
        assert_eq!(varint, BigInt::from(63));
    }

    #[test]
    fn test_subfieldVarInt_0x3fff() {
        // 2 bytes
        // 13 bits
        // hex: 0x3fff
        // dec: 8191
        let bytes: &[u8] = &decode("3fff").unwrap();
        let sequence = &bytes[..bytes.len() - 1];
        let terminator = bytes[bytes.len() - 1];
        let varint = parse_var_int_as_num_bigint(sequence, terminator);
        assert_eq!(varint, BigInt::from(8191));
    }

    #[test]
    fn test_subfieldVarInt_0x3f7fff() {
        // 3 bytes
        // 20 bits
        // hex: 0x3f7fff
        // dec: 1048575
        let bytes: &[u8] = &decode("3f7fff").unwrap();
        let sequence = &bytes[..bytes.len() - 1];
        let terminator = bytes[bytes.len() - 1];
        let varint = parse_var_int_as_num_bigint(sequence, terminator);
        assert_eq!(varint, BigInt::from(1_048_575));
    }

    #[test]
    fn test_subfieldVarInt_0x3f7f7fff() {
        // 4 bytes
        // 27 bits
        // hex: 0x3f7f7fff
        // dec: 134217727
        let bytes: &[u8] = &decode("3f7f7fff").unwrap();
        let sequence = &bytes[..bytes.len() - 1];
        let terminator = bytes[bytes.len() - 1];
        let varint = parse_var_int_as_num_bigint(sequence, terminator);
        assert_eq!(varint, BigInt::from(134_217_727));
    }

    #[test]
    fn test_subfieldVarInt_0x077f7f7fff() {
        // 5 bytes
        // 31 bits
        // hex: 0x077f7f7fff
        // dec: 2147483647
        let bytes: &[u8] = &decode("077f7f7fff").unwrap();
        let sequence = &bytes[..bytes.len() - 1];
        let terminator = bytes[bytes.len() - 1];
        let varint = parse_var_int_as_num_bigint(sequence, terminator);
        assert_eq!(varint, BigInt::from(2_147_483_647));
    }

    #[test]
    fn test_subfieldVarInt_0xa0() {
        // 1 byte
        // hex: 0xa0
        // dec: 32
        let bytes: &[u8] = &decode("a0").unwrap();
        let sequence = &bytes[..bytes.len() - 1];
        let terminator = bytes[bytes.len() - 1];
        let varint = parse_var_int_as_num_bigint(sequence, terminator);
        assert_eq!(varint, BigInt::from(32));
    }

    #[test]
    fn test_subfieldVarInt_0x2080() {
        // 2 bytes
        // hex: 0x2080
        // dec: 4096
        let bytes: &[u8] = &decode("2080").unwrap();
        let sequence = &bytes[..bytes.len() - 1];
        let terminator = bytes[bytes.len() - 1];
        let varint = parse_var_int_as_num_bigint(sequence, terminator);
        assert_eq!(varint, BigInt::from(4096));
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
        let bytes: &[u8] = &decode("ff").unwrap();
        let sequence = &bytes[..bytes.len() - 1];
        let terminator = bytes[bytes.len() - 1];
        let varuint = parse_var_uint(sequence, terminator);
        assert_eq!(varuint, BigUint::from(127u32));
    }

    #[test]
    fn test_subfieldVarUint_0x7fff() {
        // 2 bytes
        // 14 bits
        // hex: 0x7fff
        // dec: 16383
        let bytes: &[u8] = &decode("7fff").unwrap();
        let sequence = &bytes[..bytes.len() - 1];
        let terminator = bytes[bytes.len() - 1];
        let varuint = parse_var_uint(sequence, terminator);
        assert_eq!(varuint, BigUint::from(16383u32));
    }

    #[test]
    fn test_subfieldVarUint_0x017fff() {
        // 3 bytes
        // 15 bits
        // hex: 0x017fff
        // dec: 32767
        let bytes: &[u8] = &decode("017fff").unwrap();
        let sequence = &bytes[..bytes.len() - 1];
        let terminator = bytes[bytes.len() - 1];
        let varuint = parse_var_uint(sequence, terminator);
        assert_eq!(varuint, BigUint::from(32767u32));
    }

    #[test]
    fn test_subfieldVarUint_0x037fff() {
        // 3 bytes
        // 16 bits
        // hex: 0x037fff
        // dec: 65535
        let bytes: &[u8] = &decode("037fff").unwrap();
        let sequence = &bytes[..bytes.len() - 1];
        let terminator = bytes[bytes.len() - 1];
        let varuint = parse_var_uint(sequence, terminator);
        assert_eq!(varuint, BigUint::from(65535u32));
    }

    #[test]
    fn test_subfieldVarUint_0x7f7fff() {
        // 3 bytes
        // 21 bits
        // hex: 0x7f7fff
        // dec: 2097151
        let bytes: &[u8] = &decode("7f7fff").unwrap();
        let sequence = &bytes[..bytes.len() - 1];
        let terminator = bytes[bytes.len() - 1];
        let varuint = parse_var_uint(sequence, terminator);
        assert_eq!(varuint, BigUint::from(2_097_151u32));
    }
    #[test]
    fn test_subfieldVarUint_0x7f7f7fff() {
        // 4 bytes
        // 28 bits
        // hex: 0x7f7f7fff
        // dec: 268435455
        let bytes: &[u8] = &decode("7f7f7fff").unwrap();
        let sequence = &bytes[..bytes.len() - 1];
        let terminator = bytes[bytes.len() - 1];
        let varuint = parse_var_uint(sequence, terminator);
        assert_eq!(varuint, BigUint::from(268_435_455u32));
    }

    #[test]
    fn test_subfieldVarUint_0x077f7f7fff() {
        // 5 bytes
        // 31 bits
        // hex: 0x077f7f7fff
        // dec: 2147483647
        let bytes: &[u8] = &decode("077f7f7fff").unwrap();
        let sequence = &bytes[..bytes.len() - 1];
        let terminator = bytes[bytes.len() - 1];
        let varuint = parse_var_uint(sequence, terminator);
        assert_eq!(varuint, BigUint::from(2_147_483_647u32));
    }
}
