use super::combinators::{all_consuming, many0, map, preceded};
use super::ion_1_0;
use crate::error::IonResult;
use crate::ion_types::IonValue;
use crate::symbols::SymbolTable;
use nom::{
    bytes::complete::{tag, take},
    sequence::tuple,
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

pub fn take_ion_version(input: &[u8]) -> IonResult<&[u8], IonVersion> {
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

pub fn parse(input: &[u8]) -> IonResult<&[u8], Vec<IonValue>> {
    all_consuming(map(many0(preceded(tag(BVM_1_0), _parse_1_0())), |x| {
        x.into_iter().flatten().collect()
    }))(input)
}

fn _parse_1_0() -> impl FnMut(&[u8]) -> IonResult<&[u8], Vec<IonValue>> {
    move |i: &[u8]| {
        let symbol_table = SymbolTable::SystemV1;
        many0(ion_1_0::binary::parse(symbol_table))(i)
    }
}

#[allow(non_snake_case)]
#[cfg(test)]
mod tests {
    // Note this useful idiom: importing names from outer (for mod tests) scope.
    use super::*;
    use crate::error::{BinaryFormatError, FormatError, IonError};
    use crate::{
        ion_types::{
            IonBlob, IonBool, IonClob, IonData, IonDecimal, IonFloat, IonInt, IonList, IonNull,
            IonSexp, IonString, IonStruct, IonSymbol, IonTimestamp, IonValue,
        },
        symbols::SymbolToken,
    };
    use nom::error::ParseError;
    use nom::{AsBytes, Err};
    use num_bigint::{BigInt, BigUint};
    use num_traits::identities::Zero;
    use num_traits::Num;
    use pretty_assertions::assert_eq;
    use std::str::FromStr;

    pub fn strip_bvm(input: &[u8]) -> &[u8] {
        assert_eq!(BVM_1_0, &input[..4]);
        &input[4..]
    }

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
    mod null {
        use self::assert_eq;
        use super::*;

        #[test]
        fn test_parse_value_null() {
            let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/null.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![IonValue {
                    content: IonData::Null(IonNull::Null),
                    annotations: None,
                }]
            );
        }

        #[test]
        fn test_parse_value_nopPadOneByte() {
            let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/nopPadOneByte.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![IonValue {
                    content: IonData::Null(IonNull::Pad),
                    annotations: None,
                }]
            );
        }

        #[test]
        fn test_parse_value_emptyThreeByteNopPad() {
            let bytes =
                include_bytes!("../../tests/ion-tests/iontestdata/good/emptyThreeByteNopPad.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![IonValue {
                    content: IonData::Null(IonNull::Pad),
                    annotations: None,
                }]
            );
        }

        #[test]
        fn test_parse_value_nopPad16Bytes() {
            let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/nopPad16Bytes.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![IonValue {
                    content: IonData::Null(IonNull::Pad),
                    annotations: None,
                }]
            );
        }
    }

    // Parse bool tests
    mod bool {
        use self::assert_eq;
        use super::*;

        #[test]
        fn test_parse_value_nullBool() {
            let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/nullBool.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![IonValue {
                    content: IonData::Bool(IonBool::Null),
                    annotations: None,
                }]
            );
        }
    }

    // Parse int tests
    mod int {
        use self::assert_eq;
        use super::*;

        #[test]
        fn test_parse_value_nullInt2() {
            let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/nullInt2.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![IonValue {
                    content: IonData::Int(IonInt::Null),
                    annotations: None,
                }]
            );
        }

        #[test]
        fn test_parse_value_nullInt3() {
            let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/nullInt3.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![IonValue {
                    content: IonData::Int(IonInt::Null),
                    annotations: None,
                }]
            );
        }

        #[test]
        fn test_parse_intBigSize13() {
            let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/intBigSize13.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(value.len(), 1usize);
            match value[0].clone() {
                IonValue {
                    content: IonData::Int(IonInt::Integer { value: x }),
                    annotations: None,
                } => {}
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
                IonValue {
                    content: IonData::Int(IonInt::Integer { value: x }),
                    annotations: None,
                } => {}
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
                IonValue {
                    content: IonData::Int(IonInt::Integer { value: x }),
                    annotations: None,
                } => {}
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
                IonValue {
                    content: IonData::Int(IonInt::Integer { value: x }),
                    annotations: None,
                } => {}
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
                IonValue {
                    content: IonData::Int(IonInt::Integer { value: x }),
                    annotations: None,
                } => {}
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
                vec![IonValue {
                    content: IonData::Int(IonInt::Integer {
                        value: BigInt::from_str("9223372036854775808").unwrap()
                    }),
                    annotations: None,
                }]
            );
        }

        #[test]
        fn test_parse_intLongMinValue() {
            let bytes =
                include_bytes!("../../tests/ion-tests/iontestdata/good/intLongMinValue.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![IonValue {
                    content: IonData::Int(IonInt::Integer {
                        value: BigInt::from_str("-9223372036854775808").unwrap()
                    }),
                    annotations: None,
                }]
            );
        }
    }

    // Parse float tests
    mod float {
        use self::assert_eq;
        use super::*;

        #[test]
        fn test_parse_nullFloat() {
            let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/nullFloat.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![IonValue {
                    content: IonData::Float(IonFloat::Null),
                    annotations: None,
                }]
            );
        }
    }

    // Parse decimal tests
    mod decimal {
        use self::assert_eq;
        use super::*;

        #[test]
        fn test_parse_nullDecimal() {
            let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/nullDecimal.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![IonValue {
                    content: IonData::Decimal(IonDecimal::Null),
                    annotations: None,
                }]
            );
        }

        #[test]
        fn test_parse_decimalNegativeOneDotZero() {
            let bytes = include_bytes!(
                "../../tests/ion-tests/iontestdata/good/decimalNegativeOneDotZero.10n"
            );
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![IonValue {
                    content: IonData::Decimal(IonDecimal::Decimal {
                        coefficient: BigInt::from_str_radix("-10", 10).unwrap(),
                        exponent: BigInt::from_str_radix("-1", 10).unwrap(),
                    }),
                    annotations: None,
                }]
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
                vec![IonValue {
                    content: IonData::Decimal(IonDecimal::Decimal {
                        coefficient: BigInt::zero(),
                        exponent: BigInt::zero(),
                    }),
                    annotations: None,
                }]
            );
        }

        #[test]
        fn test_parse_decimalNegativeZeroDotZero() {
            let bytes = include_bytes!(
                "../../tests/ion-tests/iontestdata/good/decimalNegativeZeroDotZero.10n"
            );
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![IonValue {
                    content: IonData::Decimal(IonDecimal::Decimal {
                        coefficient: BigInt::zero(),
                        exponent: BigInt::from_str_radix("-1", 10).unwrap(),
                    }),
                    annotations: None,
                }]
            );
        }

        #[test]
        fn test_parse_decimalOneDotZero() {
            let bytes =
                include_bytes!("../../tests/ion-tests/iontestdata/good/decimalOneDotZero.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![IonValue {
                    content: IonData::Decimal(IonDecimal::Decimal {
                        coefficient: BigInt::from_str_radix("10", 10).unwrap(),
                        exponent: BigInt::from_str_radix("-1", 10).unwrap(),
                    }),
                    annotations: None,
                }]
            );
        }

        #[test]
        fn test_parse_decimalZeroDot() {
            let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/decimalZeroDot.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![IonValue {
                    content: IonData::Decimal(IonDecimal::Decimal {
                        coefficient: BigInt::zero(),
                        exponent: BigInt::zero(),
                    }),
                    annotations: None,
                }]
            );
        }
    }

    // Parse timestamp tests
    mod timestamp {
        use self::assert_eq;
        use super::*;

        #[test]
        fn test_parse_nullTimestamp() {
            let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/nullTimestamp.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![IonValue {
                    content: IonData::Timestamp(IonTimestamp::Null),
                    annotations: None,
                }]
            );
        }

        #[test]
        fn test_parse_timestamp2011() {
            let bytes = include_bytes!(
                "../../tests/ion-tests/iontestdata/good/timestamp/timestamp2011.10n"
            );
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![IonValue {
                    content: IonData::Timestamp(IonTimestamp::Year {
                        offset: BigInt::zero(),
                        year: BigUint::from(2011u32)
                    }),
                    annotations: None,
                }]
            );
        }

        #[test]
        fn test_parse_timestamp2011_02() {
            let bytes = include_bytes!(
                "../../tests/ion-tests/iontestdata/good/timestamp/timestamp2011-02.10n"
            );
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![IonValue {
                    content: IonData::Timestamp(IonTimestamp::Month {
                        offset: BigInt::zero(),
                        year: BigUint::from(2011u32),
                        month: BigUint::from(2u32)
                    }),
                    annotations: None,
                }]
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
                vec![IonValue {
                    content: IonData::Timestamp(IonTimestamp::Day {
                        offset: BigInt::zero(),
                        year: BigUint::from(2011u32),
                        month: BigUint::from(2u32),
                        day: BigUint::from(20u32)
                    }),
                    annotations: None,
                }]
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
                vec![IonValue {
                    content: IonData::Timestamp(IonTimestamp::FractionalSecond {
                        offset: BigInt::from(-480i32),
                        year: BigUint::from(2011u32),
                        month: BigUint::from(2u32),
                        day: BigUint::from(20u32),
                        hour: BigUint::from(19u32),
                        minute: BigUint::from(30u32),
                        second: BigUint::from(59u32),
                        fraction_coefficient: BigUint::from(100u32),
                        fraction_exponent: -3,
                    }),
                    annotations: None,
                }]
            );
        }
    }

    // Parse symbol tests
    mod symbol {
        use self::assert_eq;
        use super::*;

        #[test]
        fn test_parse_nullSymbol() {
            let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/nullSymbol.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![IonValue {
                    content: IonData::Symbol(IonSymbol::Null),
                    annotations: None,
                }]
            );
        }

        #[test]
        fn test_parse_symbolExplicitZero() {
            let bytes =
                include_bytes!("../../tests/ion-tests/iontestdata/good/symbolExplicitZero.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![IonValue {
                    content: IonData::Symbol(IonSymbol::Symbol {
                        token: SymbolToken::Zero
                    }),
                    annotations: None,
                }]
            );
        }

        #[test]
        fn test_parse_symbolImplicitZero() {
            let bytes =
                include_bytes!("../../tests/ion-tests/iontestdata/good/symbolImplicitZero.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![IonValue {
                    content: IonData::Symbol(IonSymbol::Symbol {
                        token: SymbolToken::Zero
                    }),
                    annotations: None,
                }]
            );
        }
    }

    // Parse string tests
    mod string {
        use self::assert_eq;
        use super::*;

        #[test]
        fn test_parse_nullString() {
            let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/nullString.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![IonValue {
                    content: IonData::String(IonString::Null),
                    annotations: None,
                }]
            );
        }
    }

    // Parse clob tests
    mod clob {
        use self::assert_eq;
        use super::*;

        #[test]
        fn test_parse_nullClob() {
            let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/nullClob.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![IonValue {
                    content: IonData::Clob(IonClob::Null),
                    annotations: None,
                }]
            );
        }

        #[test]
        fn test_parse_clobWithDel() {
            let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/clobWithDel.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![IonValue {
                    content: IonData::Clob(IonClob::Clob { data: vec![127u8] }),
                    annotations: None,
                }]
            );
        }

        #[test]
        fn test_parse_clobWithNonAsciiCharacter() {
            let bytes = include_bytes!(
                "../../tests/ion-tests/iontestdata/good/clobWithNonAsciiCharacter.10n"
            );
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![IonValue {
                    content: IonData::Clob(IonClob::Clob { data: vec![128u8] }),
                    annotations: None,
                }]
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
                vec![IonValue {
                    content: IonData::Clob(IonClob::Clob { data: vec![0u8] }),
                    annotations: None,
                }]
            );
        }
    }

    // Parse blob tests
    mod blob {
        use self::assert_eq;
        use super::*;

        #[test]
        fn test_parse_nullBlob() {
            let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/nullBlob.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![IonValue {
                    content: IonData::Blob(IonBlob::Null),
                    annotations: None,
                }]
            );
        }
    }

    // Parse list tests
    mod list {
        use self::assert_eq;
        use super::*;
        use nom::error::ErrorKind;

        // Good

        #[test]
        fn test_parse_nullList() {
            let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/nullList.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![IonValue {
                    content: IonData::List(IonList::Null),
                    annotations: None,
                }]
            );
        }

        // Bad

        #[test]
        fn test_parse_listWithValueLargerThanSize() {
            let bytes = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/listWithValueLargerThanSize.10n"
            );
            let index_of_error = strip_bvm(bytes.as_bytes());
            let err = parse(bytes).err().unwrap();
            assert_eq!(
                dbg!(err),
                Err::Error(IonError::from_error_kind(index_of_error, ErrorKind::Eof))
            );
        }
    }

    // Parse sexp tests
    mod sexp {
        use self::assert_eq;
        use super::*;

        #[test]
        fn test_parse_nullSexp() {
            let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/nullSexp.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![IonValue {
                    content: IonData::Sexp(IonSexp::Null),
                    annotations: None,
                }]
            );
        }
    }

    // Parse struct tests
    mod r#struct {
        use self::assert_eq;
        use super::*;

        #[test]
        fn test_parse_nullStructure() {
            let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/nullStruct.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![IonValue {
                    content: IonData::Struct(IonStruct::Null),
                    annotations: None,
                }]
            );
        }
    }

    // Parse annotation tests
    mod annotation {
        use super::*;

        //    annotationLengthTooLongScalar.10n
        //---------------------------------
        //    Contains an Annotation wrapper whose declared length is too long for its
        //    subfields (including its wrapped scalar value).
        #[ignore]
        #[test]
        fn test_parse_annotationLengthTooLongScalar() {
            let bytes = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/annotationLengthTooLongScalar.10n"
            );
            let err = parse(bytes).err().unwrap();
            dbg!(err);
        }

        //    annotationLengthTooLongContainer.10n
        //---------------------------------
        //    Contains an Annotation wrapper whose declared length is too long for its
        //    subfields (including its wrapped container value).
        #[ignore]
        #[test]
        fn test_parse_annotationLengthTooLongContainer() {
            let bytes = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/annotationLengthTooLongContainer.10n"
            );
            let err = parse(bytes).err().unwrap();
            dbg!(err);
        }

        //    annotationLengthTooShortScalar.10n
        //---------------------------------
        //    Contains an Annotation wrapper whose declared length is too short for its
        //    subfields (including its wrapped scalar value).
        #[ignore]
        #[test]
        fn test_parse_annotationLengthTooShortScalar() {
            let bytes = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/annotationLengthTooShortScalar.10n"
            );
            let err = parse(bytes).err().unwrap();
            dbg!(err);
        }

        //    annotationLengthTooShortContainer.10n
        //---------------------------------
        //    Contains an Annotation wrapper whose declared length is too short for its
        //    subfields (including its wrapped container value).
        #[ignore]
        #[test]
        fn test_parse_annotationLengthTooShortContainer() {
            let bytes = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/annotationLengthTooShortContainer.10n"
            );
            let err = parse(bytes).err().unwrap();
            dbg!(err);
        }

        //    annotationNested.10n
        //--------------------
        //    Contains an Annotation wrapper which contains another annotation wrapper as
        //    its value.
        #[ignore]
        #[test]
        fn test_parse_annotationNested() {
            let bytes =
                include_bytes!("../../tests/ion-tests/iontestdata/bad/annotationNested.10n");
            let err = parse(bytes).err().unwrap();
            dbg!(err);
        }

        //    annotationWithNoValue.10n
        //-------------------------
        //    Contains an Annotation wrapper with no value.
        #[ignore]
        #[test]
        fn test_parse_annotationWithNoValue() {
            let bytes =
                include_bytes!("../../tests/ion-tests/iontestdata/bad/annotationWithNoValue.10n");
            let err = parse(bytes).err().unwrap();
            dbg!(err);
        }
    }

    /// Verify errors when parsing streams containing invalid type descriptors
    /// Tests in this module are obvious candidates for parameterized testing, if I can find a
    /// satisfactory parameterized testing harness.
    mod invalid_typecodes {
        use self::assert_eq;
        use super::*;

        #[test]
        fn test_parse_invalid_type_descriptor_T1L2() {
            let T1L2 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_1_length_2.10n"
            );
            let index_of_error = strip_bvm(T1L2.as_bytes());
            let err = parse(T1L2).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::BoolValue(2))
                ))
            );
        }

        #[test]
        fn test_parse_invalid_type_descriptor_T1L3() {
            let T1L3 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_1_length_3.10n"
            );
            let index_of_error = strip_bvm(T1L3.as_bytes());
            let err = parse(T1L3).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::BoolValue(3))
                ))
            );
        }

        #[test]
        fn test_parse_invalid_type_descriptor_T1L4() {
            let T1L4 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_1_length_4.10n"
            );
            let index_of_error = strip_bvm(T1L4.as_bytes());
            let err = parse(T1L4).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::BoolValue(4))
                ))
            );
        }

        #[test]
        fn test_parse_invalid_type_descriptor_T1L5() {
            let T1L5 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_1_length_5.10n"
            );
            let index_of_error = strip_bvm(T1L5.as_bytes());
            let err = parse(T1L5).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::BoolValue(5))
                ))
            );
        }

        #[test]
        fn test_parse_invalid_type_descriptor_T1L6() {
            let T1L6 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_1_length_6.10n"
            );
            let index_of_error = strip_bvm(T1L6.as_bytes());
            let err = parse(T1L6).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::BoolValue(6))
                ))
            );
        }

        #[test]
        fn test_parse_invalid_type_descriptor_T1L7() {
            let T1L7 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_1_length_7.10n"
            );
            let index_of_error = strip_bvm(T1L7.as_bytes());
            let err = parse(T1L7).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::BoolValue(7))
                ))
            );
        }

        #[test]
        fn test_parse_invalid_type_descriptor_T1L8() {
            let T1L8 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_1_length_8.10n"
            );
            let index_of_error = strip_bvm(T1L8.as_bytes());
            let err = parse(T1L8).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::BoolValue(8))
                ))
            );
        }

        #[test]
        fn test_parse_invalid_type_descriptor_T1L9() {
            let T1L9 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_1_length_9.10n"
            );
            let index_of_error = strip_bvm(T1L9.as_bytes());
            let err = parse(T1L9).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::BoolValue(9))
                ))
            );
        }

        #[test]
        fn test_parse_invalid_type_descriptor_T1L10() {
            let T1L10 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_1_length_10.10n"
            );
            let index_of_error = strip_bvm(T1L10.as_bytes());
            let err = parse(T1L10).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::BoolValue(10))
                ))
            );
        }

        #[test]
        fn test_parse_invalid_type_descriptor_T1L11() {
            let T1L11 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_1_length_11.10n"
            );
            let index_of_error = strip_bvm(T1L11.as_bytes());
            let err = parse(T1L11).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::BoolValue(11))
                ))
            );
        }

        #[test]
        fn test_parse_invalid_type_descriptor_T1L12() {
            let T1L12 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_1_length_12.10n"
            );
            let index_of_error = strip_bvm(T1L12.as_bytes());
            let err = parse(T1L12).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::BoolValue(12))
                ))
            );
        }

        #[test]
        fn test_parse_invalid_type_descriptor_T1L13() {
            let T1L13 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_1_length_13.10n"
            );
            let index_of_error = strip_bvm(T1L13.as_bytes());
            let err = parse(T1L13).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::BoolValue(13))
                ))
            );
        }

        #[test]
        fn test_parse_invalid_type_descriptor_T1L14() {
            let T1L14 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_1_length_14.10n"
            );
            let index_of_error = strip_bvm(T1L14.as_bytes());
            let err = parse(T1L14).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::BoolValue(14))
                ))
            );
        }

        #[test]
        fn test_parse_invalid_type_descriptor_T3L0() {
            let T3L0 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_3_length_0.10n"
            );
            let index_of_error = strip_bvm(T3L0.as_bytes());
            let err = parse(T3L0).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::NegativeZero)
                ))
            );
        }

        #[test]
        fn test_parse_invalid_type_descriptor_T4L1() {
            let T4L1 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_4_length_1.10n"
            );
            let index_of_error = strip_bvm(T4L1.as_bytes());
            let err = parse(T4L1).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::FloatLength(1))
                ))
            );
        }

        #[test]
        fn test_parse_invalid_type_descriptor_T4L2() {
            let T4L2 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_4_length_2.10n"
            );
            let index_of_error = strip_bvm(T4L2.as_bytes());
            let err = parse(T4L2).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::FloatLength(2))
                ))
            );
        }

        #[test]
        fn test_parse_invalid_type_descriptor_T4L3() {
            let T4L3 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_4_length_3.10n"
            );
            let index_of_error = strip_bvm(T4L3.as_bytes());
            let err = parse(T4L3).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::FloatLength(3))
                ))
            );
        }

        #[test]
        fn test_parse_invalid_type_descriptor_T4L5() {
            let T4L5 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_4_length_5.10n"
            );
            let index_of_error = strip_bvm(T4L5.as_bytes());
            let err = parse(T4L5).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::FloatLength(5))
                ))
            );
        }

        #[test]
        fn test_parse_invalid_type_descriptor_T4L6() {
            let T4L6 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_4_length_6.10n"
            );
            let index_of_error = strip_bvm(T4L6.as_bytes());
            let err = parse(T4L6).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::FloatLength(6))
                ))
            );
        }

        #[test]
        fn test_parse_invalid_type_descriptor_T4L7() {
            let T4L7 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_4_length_7.10n"
            );
            let index_of_error = strip_bvm(T4L7.as_bytes());
            let err = parse(T4L7).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::FloatLength(7))
                ))
            );
        }

        #[test]
        fn test_parse_invalid_type_descriptor_T4L9() {
            let T4L9 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_4_length_9.10n"
            );
            let index_of_error = strip_bvm(T4L9.as_bytes());
            let err = parse(T4L9).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::FloatLength(9))
                ))
            );
        }

        #[test]
        fn test_parse_invalid_type_descriptor_T4L10() {
            let T4L10 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_4_length_10.10n"
            );
            let index_of_error = strip_bvm(T4L10.as_bytes());
            let err = parse(T4L10).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::FloatLength(10))
                ))
            );
        }

        #[test]
        fn test_parse_invalid_type_descriptor_T4L11() {
            let T4L11 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_4_length_11.10n"
            );
            let index_of_error = strip_bvm(T4L11.as_bytes());
            let err = parse(T4L11).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::FloatLength(11))
                ))
            );
        }

        #[test]
        fn test_parse_invalid_type_descriptor_T4L12() {
            let T4L12 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_4_length_12.10n"
            );
            let index_of_error = strip_bvm(T4L12.as_bytes());
            let err = parse(T4L12).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::FloatLength(12))
                ))
            );
        }

        #[test]
        fn test_parse_invalid_type_descriptor_T4L13() {
            let T4L13 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_4_length_13.10n"
            );
            let index_of_error = strip_bvm(T4L13.as_bytes());
            let err = parse(T4L13).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::FloatLength(13))
                ))
            );
        }

        #[test]
        fn test_parse_invalid_type_descriptor_T4L14() {
            let T4L14 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_4_length_14.10n"
            );
            let index_of_error = strip_bvm(T4L14.as_bytes());
            let err = parse(T4L14).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::FloatLength(14))
                ))
            );
        }

        #[test]
        fn test_parse_invalid_type_descriptor_T6L0() {
            let T6L0 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_6_length_0.10n"
            );
            let index_of_error = strip_bvm(T6L0.as_bytes());
            let err = parse(T6L0).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::TimestampLength(0))
                ))
            );
        }

        #[test]
        fn test_parse_invalid_type_descriptor_T6L1() {
            let T6L1 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_6_length_1.10n"
            );
            let index_of_error = strip_bvm(T6L1.as_bytes());
            let err = parse(T6L1).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::TimestampLength(1))
                ))
            );
        }

        #[test]
        fn test_parse_invalid_type_descriptor_T14L1() {
            let T14L1 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_14_length_1.10n"
            );
            let index_of_error = strip_bvm(T14L1.as_bytes());
            let err = parse(T14L1).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::AnnotationLength(1))
                ))
            );
        }

        #[test]
        fn test_parse_invalid_type_descriptor_T14L2() {
            let T14L2 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_14_length_2.10n"
            );
            let index_of_error = strip_bvm(T14L2.as_bytes());
            let err = parse(T14L2).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::AnnotationLength(2))
                ))
            );
        }

        #[test]
        fn test_parse_invalid_type_descriptor_T14L15() {
            let T14L15 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_14_length_15.10n"
            );
            let index_of_error = strip_bvm(T14L15.as_bytes());
            let err = parse(T14L15).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::AnnotationLength(15))
                ))
            );
        }

        #[test]
        fn test_parse_invalid_type_descriptor_T15L0() {
            let T15L0 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_15_length_0.10n"
            );
            let index_of_error = strip_bvm(T15L0.as_bytes());
            let err = parse(T15L0).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::ReservedTypeCode)
                ))
            );
        }

        #[test]
        fn test_parse_invalid_type_descriptor_T15L1() {
            let T15L1 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_15_length_1.10n"
            );
            let index_of_error = strip_bvm(T15L1.as_bytes());
            let err = parse(T15L1).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::ReservedTypeCode)
                ))
            );
        }

        #[test]
        fn test_parse_invalid_type_descriptor_T15L2() {
            let T15L2 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_15_length_2.10n"
            );
            let index_of_error = strip_bvm(T15L2.as_bytes());
            let err = parse(T15L2).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::ReservedTypeCode)
                ))
            );
        }

        #[test]
        fn test_parse_invalid_type_descriptor_T15L3() {
            let T15L3 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_15_length_3.10n"
            );
            let index_of_error = strip_bvm(T15L3.as_bytes());
            let err = parse(T15L3).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::ReservedTypeCode)
                ))
            );
        }

        #[test]
        fn test_parse_invalid_type_descriptor_T15L4() {
            let T15L4 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_15_length_4.10n"
            );
            let index_of_error = strip_bvm(T15L4.as_bytes());
            let err = parse(T15L4).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::ReservedTypeCode)
                ))
            );
        }

        #[test]
        fn test_parse_invalid_type_descriptor_T15L5() {
            let T15L5 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_15_length_5.10n"
            );
            let index_of_error = strip_bvm(T15L5.as_bytes());
            let err = parse(T15L5).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::ReservedTypeCode)
                ))
            );
        }

        #[test]
        fn test_parse_invalid_type_descriptor_T15L6() {
            let T15L6 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_15_length_6.10n"
            );
            let index_of_error = strip_bvm(T15L6.as_bytes());
            let err = parse(T15L6).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::ReservedTypeCode)
                ))
            );
        }

        #[test]
        fn test_parse_invalid_type_descriptor_T15L7() {
            let T15L7 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_15_length_7.10n"
            );
            let index_of_error = strip_bvm(T15L7.as_bytes());
            let err = parse(T15L7).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::ReservedTypeCode)
                ))
            );
        }

        #[test]
        fn test_parse_invalid_type_descriptor_T15L8() {
            let T15L8 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_15_length_8.10n"
            );
            let index_of_error = strip_bvm(T15L8.as_bytes());
            let err = parse(T15L8).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::ReservedTypeCode)
                ))
            );
        }

        #[test]
        fn test_parse_invalid_type_descriptor_T15L9() {
            let T15L9 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_15_length_9.10n"
            );
            let index_of_error = strip_bvm(T15L9.as_bytes());
            let err = parse(T15L9).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::ReservedTypeCode)
                ))
            );
        }

        #[test]
        fn test_parse_invalid_type_descriptor_T15L10() {
            let T15L10 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_15_length_10.10n"
            );
            let index_of_error = strip_bvm(T15L10.as_bytes());
            let err = parse(T15L10).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::ReservedTypeCode)
                ))
            );
        }

        #[test]
        fn test_parse_invalid_type_descriptor_T15L11() {
            let T15L11 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_15_length_11.10n"
            );
            let index_of_error = strip_bvm(T15L11.as_bytes());
            let err = parse(T15L11).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::ReservedTypeCode)
                ))
            );
        }

        #[test]
        fn test_parse_invalid_type_descriptor_T15L12() {
            let T15L12 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_15_length_12.10n"
            );
            let index_of_error = strip_bvm(T15L12.as_bytes());
            let err = parse(T15L12).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::ReservedTypeCode)
                ))
            );
        }

        #[test]
        fn test_parse_invalid_type_descriptor_T15L13() {
            let T15L13 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_15_length_13.10n"
            );
            let index_of_error = strip_bvm(T15L13.as_bytes());
            let err = parse(T15L13).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::ReservedTypeCode)
                ))
            );
        }

        #[test]
        fn test_parse_invalid_type_descriptor_T15L14() {
            let T15L14 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_15_length_14.10n"
            );
            let index_of_error = strip_bvm(T15L14.as_bytes());
            let err = parse(T15L14).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::ReservedTypeCode)
                ))
            );
        }

        #[test]
        fn test_parse_invalid_type_descriptor_T15L15() {
            let T15L15 = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/typecodes/type_15_length_15.10n"
            );
            let index_of_error = strip_bvm(T15L15.as_bytes());
            let err = parse(T15L15).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::ReservedTypeCode)
                ))
            );
        }
    }
}
