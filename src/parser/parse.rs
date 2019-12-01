use super::version_1_0::take_value;
use crate::ion_types::{
    IonBlob, IonBoolean, IonClob, IonDecimal, IonFloat, IonInteger, IonList, IonNull, IonString,
    IonStructure, IonSymbol, IonSymbolicExpression, IonTimestamp, IonValue,
};
use nom::{
    branch::alt,
    bytes::complete::{tag, take},
    error::ErrorKind,
    multi::many0,
    sequence::{pair, preceded, tuple},
    Err, IResult,
};

// Binary Ion streams begin with a four-octet Binary Version Marker
// BVM_START MAJOR_VERSION MINOR_VERSION BVM_END
// For version 1.0, this is 0xE0 0x01 0x00 0xEA
pub const BVM_BYTES: usize = 4;
pub const BVM_START_BYTE: u8 = 0xE0;
pub const BVM_END_BYTE: u8 = 0xEA;
pub const BVM_START: [u8; 1] = [BVM_START_BYTE];
pub const BVM_END: [u8; 1] = [BVM_END_BYTE];
pub const VERSION_1_0: IonVersion = IonVersion {
    major: 0x01,
    minor: 0x00,
};
pub const BVM_1_0: [u8; 4] = ion_bvm(VERSION_1_0.major, VERSION_1_0.minor).as_bytes();

#[derive(Clone, Debug, PartialEq)]
pub struct IonVersion {
    major: u8,
    minor: u8,
}

#[derive(Clone, Debug, PartialEq)]
struct BinaryVersionMarker {
    start: u8,
    major: u8,
    minor: u8,
    end: u8,
}

const fn ion_bvm(major_version: u8, minor_version: u8) -> BinaryVersionMarker {
    BinaryVersionMarker {
        start: BVM_START_BYTE,
        major: major_version,
        minor: minor_version,
        end: BVM_END_BYTE,
    }
}

impl BinaryVersionMarker {
    const fn as_bytes(&self) -> [u8; 4] {
        [self.start, self.major, self.minor, self.end]
    }
}

pub fn take_ion_version(input: &[u8]) -> IResult<&[u8], IonVersion> {
    let (input, (start, major, minor, end)) =
        tuple((tag(BVM_START), take(1usize), take(1usize), tag(BVM_END)))(input)?;
    Ok((
        input,
        IonVersion {
            major: major[0],
            minor: minor[0],
        },
    ))
}

pub fn parse(input: &[u8]) -> IResult<&[u8], Vec<IonValue>> {
    alt((
        preceded(tag(BVM_1_0), many0(super::version_1_0::parse_value)),
        version_placeholder,
    ))(input)
}

// Ion currently has only one version, so this exists to provide alternative in nom::branch::alt. Immediately errors if reached.
fn version_placeholder(input: &[u8]) -> IResult<&[u8], Vec<IonValue>> {
    Err(Err::Error((input, ErrorKind::NoneOf)))
}

#[allow(non_snake_case)]
#[cfg(test)]
mod tests {
    // Note this useful idiom: importing names from outer (for mod tests) scope.
    use super::*;
    use num_bigint::{BigInt, BigUint, Sign};
    use num_traits::identities::Zero;
    use num_traits::Num;
    use std::str::FromStr;

    #[test]
    fn binary_version_marker_test() {
        let data = include_bytes!("../../tests/ion-tests/iontestdata/good/null.10n");
        assert_eq!(&BVM_1_0, &data[0..4]);
    }

    #[test]
    fn take_binary_version_marker_test() {
        let null_null = include_bytes!("../../tests/ion-tests/iontestdata/good/null.10n");
        assert_eq!(
            take_ion_version(null_null),
            Ok((&null_null[4..], VERSION_1_0))
        );
    }

    // Parse null tests

    #[test]
    fn test_parse_value_null() {
        let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/null.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(value, vec![IonValue::IonNull(IonNull::Null)]);
    }

    #[test]
    fn test_parse_value_nopPadOneByte() {
        let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/nopPadOneByte.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(value, vec![IonValue::IonNull(IonNull::Pad)]);
    }

    #[test]
    fn test_parse_value_emptyThreeByteNopPad() {
        let bytes =
            include_bytes!("../../tests/ion-tests/iontestdata/good/emptyThreeByteNopPad.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(value, vec![IonValue::IonNull(IonNull::Pad)]);
    }

    #[test]
    fn test_parse_value_nopPad16Bytes() {
        let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/nopPad16Bytes.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(value, vec![IonValue::IonNull(IonNull::Pad)]);
    }

    // Parse bool tests

    #[test]
    fn test_parse_value_nullBool() {
        let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/nullBool.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(value, vec![IonValue::IonBoolean(IonBoolean::Null)]);
    }

    // Parse int tests

    #[test]
    fn test_parse_value_nullInt2() {
        let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/nullInt2.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(value, vec![IonValue::IonInteger(IonInteger::Null)]);
    }

    #[test]
    fn test_parse_value_nullInt3() {
        let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/nullInt3.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(value, vec![IonValue::IonInteger(IonInteger::Null)]);
    }

    #[test]
    fn test_parse_intBigSize13() {
        let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/intBigSize13.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(value.len(), 1usize);
        match value[0].clone() {
            IonValue::IonInteger(IonInteger::Integer { value: x }) => {}
            _ => panic!("expected IonInteger"),
        }
    }

    #[test]
    fn test_parse_intBigSize14() {
        let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/intBigSize14.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(value.len(), 1usize);
        match value[0].clone() {
            IonValue::IonInteger(IonInteger::Integer { value: x }) => {}
            _ => panic!("expected IonInteger"),
        }
    }

    #[test]
    fn test_parse_intBigSize16() {
        let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/intBigSize16.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(value.len(), 1usize);
        match value[0].clone() {
            IonValue::IonInteger(IonInteger::Integer { value: x }) => {}
            _ => panic!("expected IonInteger"),
        }
    }

    #[test]
    fn test_parse_intBigSize256() {
        let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/intBigSize256.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(value.len(), 1usize);
        match value[0].clone() {
            IonValue::IonInteger(IonInteger::Integer { value: x }) => {}
            _ => panic!("expected IonInteger"),
        }
    }

    #[test]
    fn test_parse_intBigSize1201() {
        let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/intBigSize1201.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(value.len(), 1usize);
        match value[0].clone() {
            IonValue::IonInteger(IonInteger::Integer { value: x }) => {}
            _ => panic!("expected IonInteger"),
        }
    }

    #[test]
    fn test_parse_intLongMaxValuePlusOne() {
        let bytes =
            include_bytes!("../../tests/ion-tests/iontestdata/good/intLongMaxValuePlusOne.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(
            value,
            vec![IonValue::IonInteger(IonInteger::Integer {
                value: BigInt::from_str("9223372036854775808").unwrap()
            })]
        );
    }

    #[test]
    fn test_parse_intLongMinValue() {
        let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/intLongMinValue.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(
            value,
            vec![IonValue::IonInteger(IonInteger::Integer {
                value: BigInt::from_str("-9223372036854775808").unwrap()
            })]
        );
    }

    // Parse float tests

    #[test]
    fn test_parse_nullFloat() {
        let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/nullFloat.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(value, vec![IonValue::IonFloat(IonFloat::Null)]);
    }

    // Parse decimal tests

    #[test]
    fn test_parse_nullDecimal() {
        let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/nullDecimal.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(value, vec![IonValue::IonDecimal(IonDecimal::Null)]);
    }

    #[test]
    fn test_parse_decimalNegativeOneDotZero() {
        let bytes =
            include_bytes!("../../tests/ion-tests/iontestdata/good/decimalNegativeOneDotZero.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(
            value,
            vec![IonValue::IonDecimal(IonDecimal::Decimal {
                coefficient: BigInt::from_str_radix("-10", 10).unwrap(),
                exponent: BigInt::from_str_radix("-1", 10).unwrap(),
            })]
        );
    }

    #[test]
    fn test_parse_decimalNegativeZeroDot() {
        let bytes =
            include_bytes!("../../tests/ion-tests/iontestdata/good/decimalNegativeZeroDot.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(
            value,
            vec![IonValue::IonDecimal(IonDecimal::Decimal {
                coefficient: BigInt::zero(),
                exponent: BigInt::zero(),
            })]
        );
    }

    #[test]
    fn test_parse_decimalNegativeZeroDotZero() {
        let bytes =
            include_bytes!("../../tests/ion-tests/iontestdata/good/decimalNegativeZeroDotZero.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(
            value,
            vec![IonValue::IonDecimal(IonDecimal::Decimal {
                coefficient: BigInt::zero(),
                exponent: BigInt::from_str_radix("-1", 10).unwrap(),
            })]
        );
    }

    #[test]
    fn test_parse_decimalOneDotZero() {
        let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/decimalOneDotZero.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(
            value,
            vec![IonValue::IonDecimal(IonDecimal::Decimal {
                coefficient: BigInt::from_str_radix("10", 10).unwrap(),
                exponent: BigInt::from_str_radix("-1", 10).unwrap(),
            })]
        );
    }

    #[test]
    fn test_parse_decimalZeroDot() {
        let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/decimalZeroDot.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(
            value,
            vec![IonValue::IonDecimal(IonDecimal::Decimal {
                coefficient: BigInt::zero(),
                exponent: BigInt::zero(),
            })]
        );
    }

    // Parse timestamp tests

    #[test]
    fn test_parse_nullTimestamp() {
        let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/nullTimestamp.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(value, vec![IonValue::IonTimestamp(IonTimestamp::Null)]);
    }

    #[test]
    fn test_parse_timestamp2011() {
        let bytes =
            include_bytes!("../../tests/ion-tests/iontestdata/good/timestamp/timestamp2011.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(
            value,
            vec![IonValue::IonTimestamp(IonTimestamp::Year {
                offset: BigInt::zero(),
                year: BigUint::from(2011u32)
            })]
        );
    }

    #[test]
    fn test_parse_timestamp2011_02() {
        let bytes =
            include_bytes!("../../tests/ion-tests/iontestdata/good/timestamp/timestamp2011-02.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(
            value,
            vec![IonValue::IonTimestamp(IonTimestamp::Month {
                offset: BigInt::zero(),
                year: BigUint::from(2011u32),
                month: BigUint::from(2u32)
            })]
        );
    }

    #[test]
    fn test_parse_timestamp2011_02_20() {
        let bytes = include_bytes!(
            "../../tests/ion-tests/iontestdata/good/timestamp/timestamp2011-02-20.10n"
        );
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(
            value,
            vec![IonValue::IonTimestamp(IonTimestamp::Day {
                offset: BigInt::zero(),
                year: BigUint::from(2011u32),
                month: BigUint::from(2u32),
                day: BigUint::from(20u32)
            })]
        );
    }

    #[test]
    fn test_parse_timestamp2011_02_20T19_30_59_100_8_00() {
        let bytes = include_bytes!(
            "../../tests/ion-tests/iontestdata/good/timestamp/timestamp2011-02-20T19_30_59_100-08_00.10n"
        );
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(
            value,
            vec![IonValue::IonTimestamp(IonTimestamp::FractionalSecond {
                offset: BigInt::from(-480i32),
                year: BigUint::from(2011u32),
                month: BigUint::from(2u32),
                day: BigUint::from(20u32),
                hour: BigUint::from(19u32),
                minute: BigUint::from(30u32),
                second: BigUint::from(59u32),
                fraction_coefficient: BigUint::from(100u32),
                fraction_exponent: -3,
            })]
        );
    }

    // Parse symbol tests

    #[test]
    fn test_parse_nullSymbol() {
        let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/nullSymbol.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(value, vec![IonValue::IonSymbol(IonSymbol::Null)]);
    }

    #[ignore] // FIXME: re-enable on completion of symbol implementation
    #[test]
    fn test_parse_symbolExplicitZero() {
        let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/symbolExplicitZero.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(value, vec![IonValue::IonSymbol(IonSymbol::SidZero)]);
    }

    #[test]
    fn test_parse_symbolImplicitZero() {
        let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/symbolImplicitZero.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(value, vec![IonValue::IonSymbol(IonSymbol::SidZero)]);
    }

    // Parse string tests

    #[test]
    fn test_parse_nullString() {
        let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/nullString.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(value, vec![IonValue::IonString(IonString::Null)]);
    }

    // Parse clob tests

    #[test]
    fn test_parse_nullClob() {
        let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/nullClob.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(value, vec![IonValue::IonClob(IonClob::Null)]);
    }

    #[test]
    fn test_parse_clobWithDel() {
        let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/clobWithDel.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(
            value,
            vec![IonValue::IonClob(IonClob::Clob { data: vec![127u8] })]
        );
    }

    #[test]
    fn test_parse_clobWithNonAsciiCharacter() {
        let bytes =
            include_bytes!("../../tests/ion-tests/iontestdata/good/clobWithNonAsciiCharacter.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(
            value,
            vec![IonValue::IonClob(IonClob::Clob { data: vec![128u8] })]
        );
    }

    #[test]
    fn test_parse_clobWithNullCharacter() {
        let bytes =
            include_bytes!("../../tests/ion-tests/iontestdata/good/clobWithNullCharacter.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(
            value,
            vec![IonValue::IonClob(IonClob::Clob { data: vec![0u8] })]
        );
    }

    // Parse blob tests

    #[test]
    fn test_parse_nullBlob() {
        let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/nullBlob.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(value, vec![IonValue::IonBlob(IonBlob::Null)]);
    }

    // Parse list tests

    #[test]
    fn test_parse_nullList() {
        let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/nullList.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(value, vec![IonValue::IonList(IonList::Null)]);
    }
}
