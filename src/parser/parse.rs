use super::ion_1_0;
use crate::{
    parser::{
        ion_1_0::{current_symbol_table::CurrentSymbolTable, text::ValueIterator},
        parse_error::{IonError, IonResult},
    },
    value::Value,
};
use nom::{
    bytes::complete::tag,
    combinator::{all_consuming, map},
    multi::many0,
    sequence::preceded,
    Err,
};

const BVM_START_BYTE: u8 = 0xE0;
const BVM_END_BYTE: u8 = 0xEA;

#[derive(Clone, Debug, PartialEq)]
struct IonVersion {
    major: u8,
    minor: u8,
}
const ION_VERSION_1_0: IonVersion = IonVersion {
    major: 0x01,
    minor: 0x00,
};

// Binary Ion streams begin with a four-octet Binary Version Marker
// BVM_START MAJOR_VERSION MINOR_VERSION BVM_END
// For version 1.0, this is 0xE0 0x01 0x00 0xEA
pub(crate) const BVM_1_0: [u8; 4] = [
    BVM_START_BYTE,
    ION_VERSION_1_0.major,
    ION_VERSION_1_0.minor,
    BVM_END_BYTE,
];

pub fn parse(input: &[u8]) -> IonResult<&[u8], Vec<Value>> {
    all_consuming(map(many0(preceded(tag(BVM_1_0), parse_ion_1_0())), |x| {
        x.into_iter().flatten().flatten().collect()
    }))(input)
}

fn parse_ion_1_0() -> impl FnMut(&[u8]) -> IonResult<&[u8], Vec<Option<Value>>> {
    move |i: &[u8]| many0(ion_1_0::binary::parse(CurrentSymbolTable::SystemV1))(i)
}

/// Ion text streams consist of zero or more top level values (TLVs).
///
/// It is assumed that each one starts with the Ion Version Marker (or IVM) if not otherwise
/// marked. The IVM can also be used to reset the symbol table if later encountered as a TLV.
pub fn parse_ion_text_1_0(input: &str) -> IonResult<&str, Vec<Value>> {
    let values: Result<Vec<Value>, Err<IonError<&str>>> = ValueIterator::new(input)
        .map(|result| match result {
            Ok((_, v)) => Ok(v),
            Err(e) => Err(e),
        })
        .collect();

    values.map(|v| (&input[input.len().saturating_sub(1)..], v))
}

#[allow(non_snake_case)]
#[cfg(test)]
mod tests {
    // Note this useful idiom: importing names from outer (for mod tests) scope.
    use super::*;
    use crate::error::{BinaryFormatError, FormatError};
    use crate::parser::parse_error::IonError;
    use crate::{
        symbols::SymbolToken,
        value::{Clob, Data, Decimal, Struct, Timestamp, Value},
    };
    use nom::error::{ErrorKind, ParseError};
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

    // Parse null tests
    mod null {
        use self::assert_eq;
        use super::*;

        #[test]
        fn test_parse_null() {
            let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/null.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(value, vec![Data::Null.into()]);
        }

        #[test]
        fn test_parse_nopPadOneByte() {
            let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/nopPadOneByte.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(value, vec![]);
        }

        #[test]
        fn test_parse_emptyThreeByteNopPad() {
            let bytes =
                include_bytes!("../../tests/ion-tests/iontestdata/good/emptyThreeByteNopPad.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(value, vec![]);
        }

        #[test]
        fn test_parse_nopPad16Bytes() {
            let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/nopPad16Bytes.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(value, vec![]);
        }

        #[test]
        fn test_parse_nopPadTooShort() {
            let bytes = include_bytes!("../../tests/ion-tests/iontestdata/bad/nopPadTooShort.10n");
            let index_of_error = strip_bvm(bytes.as_bytes());
            let err = parse(bytes).err().unwrap();
            assert_eq!(
                err,
                Err::Error(IonError::from_error_kind(index_of_error, ErrorKind::Eof))
            );
        }

        #[test]
        fn test_parse_nopPadWithAnnotations() {
            let bytes =
                include_bytes!("../../tests/ion-tests/iontestdata/bad/nopPadWithAnnotations.10n");
            let index_of_error = strip_bvm(bytes.as_bytes());
            let err = parse(bytes).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::AnnotatedPadding)
                ))
            );
        }
    }

    // Parse bool tests
    mod bool {
        use self::assert_eq;
        use super::*;

        #[test]
        fn test_parse_nullBool() {
            let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/nullBool.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(value, vec![Data::Bool(None).into()]);
        }

        //        boolWithInvalidLength_1.10n
        //    ---------------------------
        //        Contains a Bool whose _L_ value is `3`.
        //
        // Redundant, already covered by invalid typecode tests
        #[test]
        fn test_parse_boolWithInvalidLength_1() {
            let bytes =
                include_bytes!("../../tests/ion-tests/iontestdata/bad/boolWithInvalidLength_1.10n");
            let index_of_error = strip_bvm(bytes.as_bytes());
            let err = parse(bytes).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::BoolValue(3))
                ))
            );
        }

        //        boolWithInvalidLength_2.10n
        //    ---------------------------
        //        Contains a Bool whose _L_ value is `14`.
        //
        // Redundant, already covered by invalid typecode tests
        #[test]
        fn test_parse_boolWithInvalidLength_2() {
            let bytes =
                include_bytes!("../../tests/ion-tests/iontestdata/bad/boolWithInvalidLength_2.10n");
            let index_of_error = strip_bvm(bytes.as_bytes());
            let err = parse(bytes).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::BoolValue(14))
                ))
            );
        }
    }

    // Parse int tests
    mod int {
        use self::assert_eq;
        use super::*;

        #[test]
        fn test_parse_nullInt2() {
            let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/nullInt2.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(value, vec![Data::Int(None).into()]);
        }

        #[test]
        fn test_parse_nullInt3() {
            let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/nullInt3.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(value, vec![Data::Int(None).into()]);
        }

        #[test]
        fn test_parse_intBigSize13() {
            let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/intBigSize13.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![Value {
                    value: Data::Int(Some(
                        BigInt::from_str("11336061668709416277435181419700").unwrap()
                    )),
                    annotations: vec![],
                }]
            );
        }

        #[test]
        fn test_parse_intBigSize14() {
            let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/intBigSize14.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![Value {
                    value: Data::Int(Some(
                        BigInt::from_str("2773783639172303802999334644566508").unwrap()
                    )),
                    annotations: vec![],
                }]
            );
        }

        #[test]
        fn test_parse_intBigSize16() {
            let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/intBigSize16.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![Value {
                    value: Data::Int(Some(
                        BigInt::from_str("340272423131748694355562029545669544747").unwrap()
                    )),
                    annotations: vec![],
                }]
            );
        }

        #[test]
        fn test_parse_intBigSize256() {
            let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/intBigSize256.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![Value {
                    value: Data::Int(Some(BigInt::from_str("18173238162219679736857031944447898744767430095109316084451026048678348094928854458274167288816962557611640075817315237016025726423548207924331642028847993938530524659112028449811515920726159569583847554301932799584192974700038250645135419704389244690214111003505621818033044965879076306690914532152840279256440975668846810694285470204245958782248405612488959069641454132691581386219910938587286910894148564397155066367399697230287047229035630842240888106685623631032505806388903066971508775182055551847210338095961815021030725796281642316166745051164958432783938535334657296749823645911331793861360616240344479015948").unwrap())),
                    annotations: vec![],
                }]
            );
        }

        #[test]
        fn test_parse_intBigSize1201() {
            let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/intBigSize1201.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![Value {
                    value: Data::Int(Some(BigInt::from_str("-1209128330559208931735157079714115327865467964544370434819273077281224253405897103253290060918328914269463548631899645071812308993986982899415853489556522914554197390898738467539955638781239721353861037542922228152511825826456912479378000673148777515993780024795965930386155831186851955289881533965149418914029839383181842590611487224038227950198273128401548693883348401713634393145568121375459993071647448334348553273120825005547585005621905073557843647189389560417506785579189625794989355856149183678322368222017018053568223785843929618593862499036492881073539980186415837244215975091572070942626974486564192256958870281322433040920315629734420987799562134859339432681020864249825609107947443531203425338244143718177298760335807180501940870979035190848164558685567858444672451312593269633227097423878836115900713114887858077919450332372875880559910336249647028506892661241039085896754501554098250949667498984527106411008473035598074453738033716735238857637883477584250988319600774704456547850029372678707016484445435837022560093034271501880951998098283635724938896074423676575855970580069558260838440776035309445202541369670343925535423267260321422417815886413965353793233899700897139073058066763460217508771586525779901054710589009898000736916250763054841961990521459898743088196176584920012484449003850415078047222253686138267781078154495739671375572620663611552052114631900097945731921931095439124887671316064979896919603928988752398415764834722916930019335653630848969128050805919315243078165985152015169877839178130387274766520583849671414220244002835502730720905678296803140521721085874378275602166101879729520776205229110852860553922542875230905159577338887649938908711476126858731074810957223191985517430020061634257423447429047325734932149959639471077024578340172259498546287924773415569833549397495640648614921761740168603455621891605149029592116913444302003920541200298752613589073483704610085840643889518256883148865787569858833891480831853856235551962128752809483345650727329021797514398073382036743665491673472479062011149095552433898966603748025801917890013472824997063123013419469749078424510044454623139056612794416244953389598213366235639642844382108732370684721779546784123677197282416405412362675255768942931047761176059716707799470526871320177566835321501036030176667992290586158695325659258671221148046949617049967874189736192735806203766537313325095316665417565973046061927398904976505377574210036630257905963635310360480288366554574794982632248367478895180644304690231616206223328659325815377826124200213287802738293522676316584277245261826068265818652402000647997210503653388421722858961196639376695063759358340090199462437811680941857986339665514464814889259040298676717688682171451172594386941873269946865868540098809537684236649629809401295385789364940663064010519934464670550298619167439135479484913009111720326277206469486950548710197421832465756835597047652974").unwrap())),
                    annotations: vec![],
                }]
            );
        }

        #[test]
        fn test_parse_intLongMaxValuePlusOne() {
            let bytes =
                include_bytes!("../../tests/ion-tests/iontestdata/good/intLongMaxValuePlusOne.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![Data::Int(Some(BigInt::from_str("9223372036854775808").unwrap())).into()]
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
                vec![Data::Int(Some(BigInt::from_str("-9223372036854775808").unwrap())).into()]
            );
        }

        //        minLongWithLenTooLarge.10n
        //    --------------------------
        //        Contains an Int whose length is specified as 9 byte, but only 8 bytes of data
        //        are available.
        #[test]
        fn test_parse_minLongWithLenTooLarge() {
            let bytes =
                include_bytes!("../../tests/ion-tests/iontestdata/bad/minLongWithLenTooLarge.10n");
            let index_of_error = strip_bvm(bytes.as_bytes());
            let err = parse(bytes).err().unwrap();
            assert_eq!(
                err,
                Err::Error(IonError::from_error_kind(index_of_error, ErrorKind::Eof,))
            );
        }

        //        minLongWithLenTooSmall.10n
        //    --------------------------
        //        Contains an Int whose length is specified as 7 bytes, but contains 8 bytes of
        //        data. The trailing byte is `0x01` (a Null with an invalid _L_ value).
        #[test]
        fn test_parse_minLongWithLenTooSmall() {
            let bytes =
                include_bytes!("../../tests/ion-tests/iontestdata/bad/minLongWithLenTooSmall.10n");
            let index_of_error = &strip_bvm(bytes.as_bytes())[8..];
            let err = parse(bytes).err().unwrap();
            assert_eq!(
                err,
                Err::Error(IonError::from_error_kind(index_of_error, ErrorKind::Eof))
            );
        }

        //        negativeIntZero
        //        -----------------
        //        Contains a negative integer with length of 1 and value of zero (hex: `31 00`).
        #[test]
        fn test_parse_negativeIntZero() {
            let bytes = include_bytes!("../../tests/ion-tests/iontestdata/bad/negativeIntZero.10n");
            let index_of_error = strip_bvm(bytes.as_bytes());
            let err = parse(bytes).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::NegativeZero)
                ))
            );
        }

        //        negativeIntZero
        //        ---------------
        //        Contains a negative integer with length zero (hex: `30`).
        #[test]
        fn test_parse_negativeIntZeroLn() {
            let bytes =
                include_bytes!("../../tests/ion-tests/iontestdata/bad/negativeIntZeroLn.10n");
            let index_of_error = strip_bvm(bytes.as_bytes());
            let err = parse(bytes).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::NegativeZero)
                ))
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
            assert_eq!(value, vec![Data::Float(None).into()]);
        }

        //        floatLenTooLarge.10n
        //    --------------------
        //        Contains a Float whose length is specified as 8 bytes, but only 7 bytes of data
        //        are available.
        #[test]
        fn test_parse_floatLenTooLarge() {
            let bytes =
                include_bytes!("../../tests/ion-tests/iontestdata/bad/floatLenTooLarge.10n");
            let index_of_error = strip_bvm(bytes.as_bytes());
            let err = parse(bytes).err().unwrap();
            assert_eq!(
                err,
                Err::Error(IonError::from_error_kind(index_of_error, ErrorKind::Eof))
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
            assert_eq!(value, vec![Data::Decimal(None).into()]);
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
                vec![Data::Decimal(Some(Decimal {
                    coefficient: BigInt::from_str_radix("-10", 10).unwrap(),
                    exponent: BigInt::from_str_radix("-1", 10).unwrap(),
                }))
                .into()]
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
                vec![Data::Decimal(Some(Decimal {
                    coefficient: BigInt::zero(),
                    exponent: BigInt::zero(),
                }))
                .into()]
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
                vec![Data::Decimal(Some(Decimal {
                    coefficient: BigInt::zero(),
                    exponent: BigInt::from_str_radix("-1", 10).unwrap(),
                }))
                .into()]
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
                vec![Data::Decimal(Some(Decimal {
                    coefficient: BigInt::from_str_radix("10", 10).unwrap(),
                    exponent: BigInt::from_str_radix("-1", 10).unwrap(),
                }))
                .into()]
            );
        }

        #[test]
        fn test_parse_decimalZeroDot() {
            let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/decimalZeroDot.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![Data::Decimal(Some(Decimal {
                    coefficient: BigInt::zero(),
                    exponent: BigInt::zero(),
                }))
                .into()]
            );
        }

        // decimalExpTooLarge.10n
        // ----------------------
        // This file contains a Decimal who's exponent exceeds the length defined by the decimal container's length.
        //
        // The file contents are:
        // E0 01 00 EA 58 FC 59 59 59 EA 59 00 59 59 59 59 59 59 59 59 3A 44 3A 30 59 59 59 60 59 59 59 59 59 59 59 00 80 59 3A 44 3A 30 30
        // ^.......... ^..1..2..3..4..5..6..7..8. ^..1..2..3..4..5..6..7..8..9.
        // BVM         8 byte decimal (valid)     9 byte decimal (invalid)
        #[test]
        fn test_parse_decimalExpTooLarge() {
            let bytes =
                include_bytes!("../../tests/ion-tests/iontestdata/bad/decimalExpTooLarge.10n");
            let index_of_error = &strip_bvm(bytes.as_bytes())[9..];
            let err = parse(bytes).err().unwrap();
            assert_eq!(
                err,
                Err::Error(IonError::from_error_kind(index_of_error, ErrorKind::Eof))
            );
        }

        // decimalLenCauses64BitOverflow.10n
        // ---------------------------------
        // This file contains a Decimal who's total length is 2^64-1, larger than the
        // datagram size, and when combined with a buffer offset, is likely to cause an
        // overflow when calculating the end index of the value.
        #[ignore]
        #[test]
        fn test_parse_decimalLenCauses64BitOverflow() {
            let bytes = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/decimalLenCauses64BitOverflow.10n"
            );
            let index_of_error = strip_bvm(bytes.as_bytes());
            let err = parse(bytes).err().unwrap();
            assert_eq!(
                err,
                Err::Error(IonError::from_error_kind(index_of_error, ErrorKind::Eof))
            );
        }

        // decimalLenTooLarge.10n
        // ----------------------
        // Contains a Decimal whose length is specified as 34 bytes, but only 24 bytes of data are available.
        #[test]
        fn test_parse_decimalLenTooLarge() {
            let bytes =
                include_bytes!("../../tests/ion-tests/iontestdata/bad/decimalLenTooLarge.10n");
            let index_of_error = strip_bvm(bytes.as_bytes());
            let err = parse(bytes).err().unwrap();
            assert_eq!(
                err,
                Err::Error(IonError::from_error_kind(index_of_error, ErrorKind::Eof))
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
            assert_eq!(value, vec![Data::Timestamp(None).into()]);
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
                vec![Timestamp::Year {
                    offset: 0,
                    year: 2011
                }
                .into()]
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
                vec![Timestamp::Month {
                    offset: 0,
                    year: 2011,
                    month: 2
                }
                .into()]
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
                vec![Timestamp::Day {
                    offset: 0,
                    year: 2011,
                    month: 2,
                    day: 20
                }
                .into()]
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
                vec![Timestamp::FractionalSecond {
                    offset: -480,
                    year: 2011,
                    month: 2,
                    day: 20,
                    hour: 19,
                    minute: 30,
                    second: 59,
                    fraction_coefficient: BigUint::from(100u32),
                    fraction_exponent: -3,
                }
                .into()]
            );
        }
    }

    // Parse symbol tests
    mod symbol {
        use self::assert_eq;
        use super::*;
        use crate::error::SymbolError;

        #[test]
        fn test_parse_nullSymbol() {
            let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/nullSymbol.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(value, vec![Data::Symbol(None).into()]);
        }

        #[test]
        fn test_parse_symbolExplicitZero() {
            let bytes =
                include_bytes!("../../tests/ion-tests/iontestdata/good/symbolExplicitZero.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(value, vec![Data::Symbol(Some(SymbolToken::Zero)).into()]);
        }

        #[test]
        fn test_parse_symbolImplicitZero() {
            let bytes =
                include_bytes!("../../tests/ion-tests/iontestdata/good/symbolImplicitZero.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(value, vec![Data::Symbol(Some(SymbolToken::Zero)).into()]);
        }

        // The symbol ID does not contain a mapping in the current symbol table context.
        #[test]
        fn test_parse_symbolIDUnmapped() {
            let bytes =
                include_bytes!("../../tests/ion-tests/iontestdata/bad/symbolIDUnmapped.10n");
            let index_of_error = strip_bvm(bytes.as_bytes());
            let err = parse(bytes).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_symbol_error(
                    index_of_error,
                    SymbolError::AboveMaxId {
                        max_id: 9,
                        symbol_id: 10
                    }
                ))
            );
        }

        // The field name is out of range of the local symbol table.
        #[test]
        fn test_parse_fieldNameSymbolIDUnmapped() {
            let bytes = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/fieldNameSymbolIDUnmapped.10n"
            );
            let index_of_error = strip_bvm(bytes.as_bytes());
            let err = parse(bytes).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_symbol_error(
                    index_of_error,
                    SymbolError::AboveMaxId {
                        max_id: 9,
                        symbol_id: 10
                    }
                ))
            );
        }

        //        symbolLenTooLarge.10n
        //    ---------------------
        //        Contains a Symbol whose length is specified as 2 bytes, but only 1 byte of data
        //        is available.
        #[test]
        fn test_parse_symbolLenTooLarge() {
            let bytes =
                include_bytes!("../../tests/ion-tests/iontestdata/bad/symbolLenTooLarge.10n");
            let index_of_error = strip_bvm(bytes.as_bytes());
            let err = parse(bytes).err().unwrap();
            assert_eq!(
                err,
                Err::Error(IonError::from_error_kind(index_of_error, ErrorKind::Eof))
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
            assert_eq!(value, vec![Data::String(None).into()]);
        }

        //        stringLenTooLarge.10n
        //    ---------------------
        //        Contains a String whose length is specified as 44 bytes, but only 38 bytes of
        //        data are available.
        #[test]
        fn test_parse_stringLenTooLarge() {
            let bytes =
                include_bytes!("../../tests/ion-tests/iontestdata/bad/stringLenTooLarge.10n");
            let index_of_error = strip_bvm(bytes.as_bytes());
            let err = parse(bytes).err().unwrap();
            assert_eq!(
                err,
                Err::Error(IonError::from_error_kind(index_of_error, ErrorKind::Eof))
            );
        }

        //        stringWithLatinEncoding.10n
        //    ---------------------------
        //        Contains a String with several valid Latin-1 (ISO-8859-1) characters which do
        //        not produce valid UTF-8 code points.
        #[test]
        fn test_parse_stringWithLatinEncoding() {
            let bytes =
                include_bytes!("../../tests/ion-tests/iontestdata/bad/stringWithLatinEncoding.10n");
            let index_of_error = strip_bvm(bytes.as_bytes());
            let err = parse(bytes).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::StringEncoding)
                ))
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
            assert_eq!(value, vec![Data::Clob(None).into()]);
        }

        #[test]
        fn test_parse_clobWithDel() {
            let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/clobWithDel.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![Data::Clob(Some(Clob { data: vec![127u8] })).into()]
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
                vec![Data::Clob(Some(Clob { data: vec![128u8] })).into()]
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
                vec![Data::Clob(Some(Clob { data: vec![0u8] })).into()]
            );
        }

        //        clobLenTooLarge.10n
        //    -------------------
        //        Contains a Clob whose length is specified as 5,400 bytes, but only 16 bytes of
        //        data are available.
        #[test]
        fn test_parse_clobLenTooLarge() {
            let bytes = include_bytes!("../../tests/ion-tests/iontestdata/bad/clobLenTooLarge.10n");
            let index_of_error = strip_bvm(bytes.as_bytes());
            let err = parse(bytes).err().unwrap();
            assert_eq!(
                err,
                Err::Error(IonError::from_error_kind(index_of_error, ErrorKind::Eof))
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
            assert_eq!(value, vec![Data::Blob(None).into()]);
        }

        //        blobLenTooLarge.10n
        //    -------------------
        //        Contains a Blob whose length is specified as 15 bytes, but only 14 bytes of
        //        data are available.
        #[test]
        fn test_parse_blobLenTooLarge() {
            let bytes = include_bytes!("../../tests/ion-tests/iontestdata/bad/blobLenTooLarge.10n");
            let index_of_error = strip_bvm(bytes.as_bytes());
            let err = parse(bytes).err().unwrap();
            assert_eq!(
                err,
                Err::Error(IonError::from_error_kind(index_of_error, ErrorKind::Eof))
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
            assert_eq!(value, vec![Data::List(None).into()]);
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
                err,
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
            assert_eq!(value, vec![Data::Sexp(None).into()]);
        }
    }

    // Parse struct tests
    mod r#struct {
        use self::assert_eq;
        use super::*;
        use std::string::String;

        // Good

        #[test]
        fn test_parse_nullStruct() {
            let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/nullStruct.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(value, vec![Data::Struct(None).into()]);
        }

        #[test]
        fn test_parse_nopPadInsideEmptyStructNonZeroSymbolId() {
            let bytes = include_bytes!(
                "../../tests/ion-tests/iontestdata/good/nopPadInsideEmptyStructNonZeroSymbolId.10n"
            );
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![Data::Struct(Some(Struct { fields: vec![] })).into()]
            );
        }

        #[test]
        fn test_parse_nopPadInsideEmptyStructZeroSymbolId() {
            let bytes = include_bytes!(
                "../../tests/ion-tests/iontestdata/good/nopPadInsideEmptyStructZeroSymbolId.10n"
            );
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![Data::Struct(Some(Struct { fields: vec![] })).into()]
            );
        }

        #[test]
        fn test_parse_nopPadInsideStructWithNopPadThenValueZeroSymbolId() {
            let bytes = include_bytes!(
                "../../tests/ion-tests/iontestdata/good/nopPadInsideStructWithNopPadThenValueZeroSymbolId.10n"
            );
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![Data::Struct(Some(Struct {
                    fields: vec![(
                        SymbolToken::Known {
                            text: String::from("name")
                        },
                        Data::Bool(Some(true)).into(),
                    )]
                }))
                .into()]
            );
        }

        #[test]
        fn test_parse_nopPadInsideStructWithValueThenNopPad() {
            let bytes = include_bytes!(
                "../../tests/ion-tests/iontestdata/good/nopPadInsideStructWithValueThenNopPad.10n"
            );
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![Data::Struct(Some(Struct {
                    fields: vec![(
                        SymbolToken::Known {
                            text: String::from("name")
                        },
                        Data::Bool(Some(true)).into(),
                    )]
                }))
                .into()]
            );
        }

        #[test]
        fn test_parse_structAnnotatedEmpty() {
            let bytes =
                include_bytes!("../../tests/ion-tests/iontestdata/good/structAnnotatedEmpty.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![Value {
                    value: Data::Struct(Some(Struct { fields: vec![] })),
                    annotations: vec![SymbolToken::Known {
                        text: String::from("max_id")
                    }]
                }]
            );
        }

        #[test]
        fn test_parse_structEmpty() {
            let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/structEmpty.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![Data::Struct(Some(Struct { fields: vec![] })).into()]
            );
        }

        #[test]
        fn test_parse_structLen13() {
            let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/structLen13.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![Data::Struct(Some(Struct {
                    fields: vec![(
                        SymbolToken::Known {
                            text: String::from("name")
                        },
                        Data::String(Some(String::from("123456789AB"))).into(),
                    )]
                }))
                .into()]
            );
        }

        #[test]
        fn test_parse_structLen14() {
            let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/structLen14.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![Data::Struct(Some(Struct {
                    fields: vec![(
                        SymbolToken::Known {
                            text: String::from("name")
                        },
                        Data::String(Some(String::from("123456789ABC"))).into(),
                    )]
                }))
                .into()]
            );
        }

        #[test]
        fn test_parse_structLen15() {
            let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/structLen15.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![Data::Struct(Some(Struct {
                    fields: vec![(
                        SymbolToken::Known {
                            text: String::from("name")
                        },
                        Data::String(Some(String::from("123456789ABCD"))).into(),
                    )]
                }))
                .into()]
            );
        }

        #[test]
        fn test_parse_structOrdered() {
            let bytes = include_bytes!("../../tests/ion-tests/iontestdata/good/structOrdered.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![Data::Struct(Some(Struct {
                    fields: vec![
                        (
                            SymbolToken::Known {
                                text: String::from("name")
                            },
                            Data::Null.into(),
                        ),
                        (
                            SymbolToken::Known {
                                text: String::from("version")
                            },
                            Data::Bool(Some(false)).into(),
                        ),
                        (
                            SymbolToken::Known {
                                text: String::from("imports")
                            },
                            Data::Bool(Some(true)).into()
                        )
                    ]
                }))
                .into()],
            );
        }

        #[test]
        fn test_parse_structAnnotatedOrdered() {
            let bytes =
                include_bytes!("../../tests/ion-tests/iontestdata/good/structAnnotatedOrdered.10n");
            let (remaining_bytes, value) = parse(bytes).unwrap();
            assert_eq!(remaining_bytes, &[] as &[u8]);
            assert_eq!(
                value,
                vec![Value {
                    value: Data::Struct(Some(Struct {
                        fields: vec![
                            (
                                SymbolToken::Known {
                                    text: String::from("name")
                                },
                                Data::Null.into(),
                            ),
                            (
                                SymbolToken::Known {
                                    text: String::from("version")
                                },
                                Data::Bool(Some(false)).into()
                            ),
                            (
                                SymbolToken::Known {
                                    text: String::from("imports")
                                },
                                Data::Bool(Some(true)).into(),
                            )
                        ]
                    })),
                    annotations: vec![
                        SymbolToken::Known {
                            text: String::from("symbols")
                        },
                        SymbolToken::Known {
                            text: String::from("max_id")
                        }
                    ],
                }],
            );
        }

        // Bad

        //        structOrderedEmpty.10n
        //    ----------------------
        //        Contains an ordered Struct (type ID `0xD1`) with a length of `0` (`0x80`).
        //        Ordered structs must contain at least one symbol/value pair.
        #[test]
        fn test_parse_structOrderedEmpty() {
            let bytes =
                include_bytes!("../../tests/ion-tests/iontestdata/bad/structOrderedEmpty.10n");
            let index_of_error = strip_bvm(bytes.as_bytes());
            let err = parse(bytes).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::StructEmpty)
                ))
            );
        }
    }

    // Parse annotation tests
    mod annotation {
        use self::assert_eq;
        use super::*;
        use nom::error::ErrorKind;

        //    annotationLengthTooLongScalar.10n
        //---------------------------------
        //    Contains an Annotation wrapper whose declared length is too long for its
        //    subfields (including its wrapped scalar value).
        #[test]
        fn test_parse_annotationLengthTooLongScalar() {
            let bytes = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/annotationLengthTooLongScalar.10n"
            );
            let index_of_error = strip_bvm(bytes.as_bytes());
            let err = parse(bytes).err().unwrap();
            assert_eq!(
                err,
                Err::Error(IonError::from_error_kind(index_of_error, ErrorKind::Eof,))
            );
        }

        //    annotationLengthTooLongContainer.10n
        //---------------------------------
        //    Contains an Annotation wrapper whose declared length is too long for its
        //    subfields (including its wrapped container value).
        #[test]
        fn test_parse_annotationLengthTooLongContainer() {
            let bytes = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/annotationLengthTooLongContainer.10n"
            );
            let index_of_error = strip_bvm(bytes.as_bytes());
            let err = parse(bytes).err().unwrap();
            assert_eq!(
                err,
                Err::Error(IonError::from_error_kind(index_of_error, ErrorKind::Eof,))
            );
        }

        //    annotationLengthTooShortScalar.10n
        //---------------------------------
        //    Contains an Annotation wrapper whose declared length is too short for its
        //    subfields (including its wrapped scalar value).
        #[test]
        fn test_parse_annotationLengthTooShortScalar() {
            let bytes = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/annotationLengthTooShortScalar.10n"
            );
            let index_of_error = strip_bvm(bytes.as_bytes());
            let err = parse(bytes).err().unwrap();
            assert_eq!(
                err,
                Err::Error(IonError::from_error_kind(index_of_error, ErrorKind::Eof,))
            );
        }

        //    annotationLengthTooShortContainer.10n
        //---------------------------------
        //    Contains an Annotation wrapper whose declared length is too short for its
        //    subfields (including its wrapped container value).
        #[test]
        fn test_parse_annotationLengthTooShortContainer() {
            let bytes = include_bytes!(
                "../../tests/ion-tests/iontestdata/bad/annotationLengthTooShortContainer.10n"
            );
            let index_of_error = strip_bvm(bytes.as_bytes());
            let err = parse(bytes).err().unwrap();
            assert_eq!(
                err,
                Err::Error(IonError::from_error_kind(index_of_error, ErrorKind::Eof,))
            );
        }

        //    annotationNested.10n
        //--------------------
        //    Contains an Annotation wrapper which contains another annotation wrapper as
        //    its value.
        #[test]
        fn test_parse_annotationNested() {
            let bytes =
                include_bytes!("../../tests/ion-tests/iontestdata/bad/annotationNested.10n");
            let index_of_error = strip_bvm(bytes.as_bytes());
            let err = parse(bytes).err().unwrap();
            assert_eq!(
                err,
                Err::Failure(IonError::from_format_error(
                    index_of_error,
                    FormatError::Binary(BinaryFormatError::AnnotatedAnnotation)
                ))
            );
        }

        //    annotationWithNoValue.10n
        //-------------------------
        //    Contains an Annotation wrapper with no value.
        #[test]
        fn test_parse_annotationWithNoValue() {
            let bytes =
                include_bytes!("../../tests/ion-tests/iontestdata/bad/annotationWithNoValue.10n");
            let index_of_error = strip_bvm(bytes.as_bytes());
            let err = parse(bytes).err().unwrap();
            assert_eq!(
                err,
                Err::Error(IonError::from_error_kind(index_of_error, ErrorKind::Eof,))
            );
        }

        //    emptyAnnotatedInt.10n
        //-------------------------
        //    Contains an Annotation wrapper with an annot_length subfield value of zero,
        //    which is illegal because at least one annotation must exist.
        //
        // Redundant: This is already covered by the invalid type descriptor tests.
        #[test]
        fn test_parse_emptyAnnotatedInt() {
            let bytes =
                include_bytes!("../../tests/ion-tests/iontestdata/bad/emptyAnnotatedInt.10n");
            let index_of_error = strip_bvm(bytes.as_bytes());
            let err = parse(bytes).err().unwrap();
            assert_eq!(
                err,
                Err::Error(IonError::from_error_kind(index_of_error, ErrorKind::Eof,))
            );
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
