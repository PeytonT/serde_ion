use crate::parse::parse;
use crate::ser::ion_1_0::binary::Writer;
use crate::Version;
use crate::{
    symbols::SymbolToken,
    value::{Clob, Data, Decimal, Struct, Timestamp, Value},
};
use num_bigint::{BigInt, BigUint};
use num_traits::identities::Zero;
use num_traits::Num;
use pretty_assertions::assert_eq;
use std::str::FromStr;

// null tests
mod null {
    use self::assert_eq;
    use super::*;

    #[test]
    fn test_null() {
        let bytes = include_bytes!("../tests/ion-tests/iontestdata/good/null.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(value, vec![Data::Null.into()]);

        let mut writer = Writer::new(Version::Ion_1_0);
        writer.extend(Version::Ion_1_0, value).unwrap();
        writer.write();

        assert_eq!(bytes.to_vec(), writer.get());
    }
}

// bool tests
mod bool {
    use self::assert_eq;
    use super::*;

    #[test]
    fn test_nullBool() {
        let bytes = include_bytes!("../tests/ion-tests/iontestdata/good/nullBool.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(value, vec![Data::Bool(None).into()]);

        let mut writer = Writer::new(Version::Ion_1_0);
        writer.extend(Version::Ion_1_0, value).unwrap();
        writer.write();

        assert_eq!(bytes.to_vec(), writer.get());
    }
}

// int tests
mod int {
    use self::assert_eq;
    use super::*;

    #[test]
    fn test_nullInt() {
        let pos_null_bytes = include_bytes!("../tests/ion-tests/iontestdata/good/nullInt2.10n");
        let (remaining_bytes, pos_null_int) = parse(pos_null_bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(pos_null_int, vec![Data::Int(None).into()]);

        let neg_null_bytes = include_bytes!("../tests/ion-tests/iontestdata/good/nullInt3.10n");
        let (remaining_bytes, neg_null_int) = parse(neg_null_bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(neg_null_int, vec![Data::Int(None).into()]);

        assert_eq!(pos_null_int, neg_null_int);

        let mut writer = Writer::new(Version::Ion_1_0);
        writer.extend(Version::Ion_1_0, pos_null_int).unwrap();
        writer.write();
        assert_eq!(pos_null_bytes.to_vec(), writer.get());

        let mut writer = Writer::new(Version::Ion_1_0);
        writer.extend(Version::Ion_1_0, neg_null_int).unwrap();
        writer.write();
        assert_eq!(pos_null_bytes.to_vec(), writer.get());
    }

    #[test]
    fn test_intBigSize13() {
        let bytes = include_bytes!("../tests/ion-tests/iontestdata/good/intBigSize13.10n");
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

        let mut writer = Writer::new(Version::Ion_1_0);
        writer.extend(Version::Ion_1_0, value).unwrap();
        writer.write();

        assert_eq!(bytes.to_vec(), writer.get());
    }

    #[test]
    fn test_intBigSize14() {
        let bytes = include_bytes!("../tests/ion-tests/iontestdata/good/intBigSize14.10n");
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

        let mut writer = Writer::new(Version::Ion_1_0);
        writer.extend(Version::Ion_1_0, value).unwrap();
        writer.write();

        assert_eq!(bytes.to_vec(), writer.get());
    }

    #[test]
    fn test_intBigSize16() {
        let bytes = include_bytes!("../tests/ion-tests/iontestdata/good/intBigSize16.10n");
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

        let mut writer = Writer::new(Version::Ion_1_0);
        writer.extend(Version::Ion_1_0, value).unwrap();
        writer.write();

        assert_eq!(bytes.to_vec(), writer.get());
    }

    #[test]
    fn test_intBigSize256() {
        let bytes = include_bytes!("../tests/ion-tests/iontestdata/good/intBigSize256.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(
            value,
            vec![Value {
                value: Data::Int(Some(BigInt::from_str("18173238162219679736857031944447898744767430095109316084451026048678348094928854458274167288816962557611640075817315237016025726423548207924331642028847993938530524659112028449811515920726159569583847554301932799584192974700038250645135419704389244690214111003505621818033044965879076306690914532152840279256440975668846810694285470204245958782248405612488959069641454132691581386219910938587286910894148564397155066367399697230287047229035630842240888106685623631032505806388903066971508775182055551847210338095961815021030725796281642316166745051164958432783938535334657296749823645911331793861360616240344479015948").unwrap())),
                annotations: vec![],
            }]
        );

        let mut writer = Writer::new(Version::Ion_1_0);
        writer.extend(Version::Ion_1_0, value).unwrap();
        writer.write();

        assert_eq!(bytes.to_vec(), writer.get());
    }

    #[test]
    fn test_intBigSize1201() {
        let bytes = include_bytes!("../tests/ion-tests/iontestdata/good/intBigSize1201.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(
            value,
            vec![Value {
                value: Data::Int(Some(BigInt::from_str("-1209128330559208931735157079714115327865467964544370434819273077281224253405897103253290060918328914269463548631899645071812308993986982899415853489556522914554197390898738467539955638781239721353861037542922228152511825826456912479378000673148777515993780024795965930386155831186851955289881533965149418914029839383181842590611487224038227950198273128401548693883348401713634393145568121375459993071647448334348553273120825005547585005621905073557843647189389560417506785579189625794989355856149183678322368222017018053568223785843929618593862499036492881073539980186415837244215975091572070942626974486564192256958870281322433040920315629734420987799562134859339432681020864249825609107947443531203425338244143718177298760335807180501940870979035190848164558685567858444672451312593269633227097423878836115900713114887858077919450332372875880559910336249647028506892661241039085896754501554098250949667498984527106411008473035598074453738033716735238857637883477584250988319600774704456547850029372678707016484445435837022560093034271501880951998098283635724938896074423676575855970580069558260838440776035309445202541369670343925535423267260321422417815886413965353793233899700897139073058066763460217508771586525779901054710589009898000736916250763054841961990521459898743088196176584920012484449003850415078047222253686138267781078154495739671375572620663611552052114631900097945731921931095439124887671316064979896919603928988752398415764834722916930019335653630848969128050805919315243078165985152015169877839178130387274766520583849671414220244002835502730720905678296803140521721085874378275602166101879729520776205229110852860553922542875230905159577338887649938908711476126858731074810957223191985517430020061634257423447429047325734932149959639471077024578340172259498546287924773415569833549397495640648614921761740168603455621891605149029592116913444302003920541200298752613589073483704610085840643889518256883148865787569858833891480831853856235551962128752809483345650727329021797514398073382036743665491673472479062011149095552433898966603748025801917890013472824997063123013419469749078424510044454623139056612794416244953389598213366235639642844382108732370684721779546784123677197282416405412362675255768942931047761176059716707799470526871320177566835321501036030176667992290586158695325659258671221148046949617049967874189736192735806203766537313325095316665417565973046061927398904976505377574210036630257905963635310360480288366554574794982632248367478895180644304690231616206223328659325815377826124200213287802738293522676316584277245261826068265818652402000647997210503653388421722858961196639376695063759358340090199462437811680941857986339665514464814889259040298676717688682171451172594386941873269946865868540098809537684236649629809401295385789364940663064010519934464670550298619167439135479484913009111720326277206469486950548710197421832465756835597047652974").unwrap())),
                annotations: vec![],
            }]
        );

        let mut writer = Writer::new(Version::Ion_1_0);
        writer.extend(Version::Ion_1_0, value).unwrap();
        writer.write();

        assert_eq!(bytes.to_vec(), writer.get());
    }

    #[test]
    fn test_intLongMaxValuePlusOne() {
        let bytes =
            include_bytes!("../tests/ion-tests/iontestdata/good/intLongMaxValuePlusOne.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(
            value,
            vec![Data::Int(Some(BigInt::from_str("9223372036854775808").unwrap())).into()]
        );

        let mut writer = Writer::new(Version::Ion_1_0);
        writer.extend(Version::Ion_1_0, value).unwrap();
        writer.write();

        assert_eq!(bytes.to_vec(), writer.get());
    }

    #[test]
    fn test_intLongMinValue() {
        let bytes = include_bytes!("../tests/ion-tests/iontestdata/good/intLongMinValue.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(
            value,
            vec![Data::Int(Some(BigInt::from_str("-9223372036854775808").unwrap())).into()]
        );

        let mut writer = Writer::new(Version::Ion_1_0);
        writer.extend(Version::Ion_1_0, value).unwrap();
        writer.write();

        assert_eq!(bytes.to_vec(), writer.get());
    }
}

// float tests
mod float {
    use self::assert_eq;
    use super::*;

    #[test]
    fn test_nullFloat() {
        let bytes = include_bytes!("../tests/ion-tests/iontestdata/good/nullFloat.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(value, vec![Data::Float(None).into()]);

        let mut writer = Writer::new(Version::Ion_1_0);
        writer.extend(Version::Ion_1_0, value).unwrap();
        writer.write();

        assert_eq!(bytes.to_vec(), writer.get());
    }
}

// decimal tests
mod decimal {
    use self::assert_eq;
    use super::*;

    #[test]
    fn test_nullDecimal() {
        let bytes = include_bytes!("../tests/ion-tests/iontestdata/good/nullDecimal.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(value, vec![Data::Decimal(None).into()]);

        let mut writer = Writer::new(Version::Ion_1_0);
        writer.extend(Version::Ion_1_0, value).unwrap();
        writer.write();

        assert_eq!(bytes.to_vec(), writer.get());
    }

    #[test]
    fn test_decimalNegativeOneDotZero() {
        let bytes =
            include_bytes!("../tests/ion-tests/iontestdata/good/decimalNegativeOneDotZero.10n");
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

        let mut writer = Writer::new(Version::Ion_1_0);
        writer.extend(Version::Ion_1_0, value).unwrap();
        writer.write();

        assert_eq!(bytes.to_vec(), writer.get());
    }

    #[test]
    fn test_decimalNegativeZeroDot() {
        let bytes =
            include_bytes!("../tests/ion-tests/iontestdata/good/decimalNegativeZeroDot.10n");
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

        let mut writer = Writer::new(Version::Ion_1_0);
        writer.extend(Version::Ion_1_0, value.clone()).unwrap();
        writer.write();

        assert_eq!(value, parse(&writer.get()).unwrap().1);
    }

    #[test]
    fn test_decimalNegativeZeroDotZero() {
        let bytes =
            include_bytes!("../tests/ion-tests/iontestdata/good/decimalNegativeZeroDotZero.10n");
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

        let mut writer = Writer::new(Version::Ion_1_0);
        writer.extend(Version::Ion_1_0, value.clone()).unwrap();
        writer.write();

        assert_eq!(value, parse(&writer.get()).unwrap().1);
    }

    #[test]
    fn test_decimalOneDotZero() {
        let bytes = include_bytes!("../tests/ion-tests/iontestdata/good/decimalOneDotZero.10n");
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

        let mut writer = Writer::new(Version::Ion_1_0);
        writer.extend(Version::Ion_1_0, value).unwrap();
        writer.write();

        assert_eq!(bytes.to_vec(), writer.get());
    }

    #[test]
    fn test_decimalZeroDot() {
        let bytes = include_bytes!("../tests/ion-tests/iontestdata/good/decimalZeroDot.10n");
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

        let mut writer = Writer::new(Version::Ion_1_0);
        writer.extend(Version::Ion_1_0, value).unwrap();
        writer.write();

        assert_eq!(bytes.to_vec(), writer.get());
    }
}

// timestamp tests
mod timestamp {
    use self::assert_eq;
    use super::*;

    #[test]
    fn test_nullTimestamp() {
        let bytes = include_bytes!("../tests/ion-tests/iontestdata/good/nullTimestamp.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(value, vec![Data::Timestamp(None).into()]);

        let mut writer = Writer::new(Version::Ion_1_0);
        writer.extend(Version::Ion_1_0, value).unwrap();
        writer.write();

        assert_eq!(bytes.to_vec(), writer.get());
    }

    #[test]
    fn test_timestamp2011() {
        let bytes =
            include_bytes!("../tests/ion-tests/iontestdata/good/timestamp/timestamp2011.10n");
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

        let mut writer = Writer::new(Version::Ion_1_0);
        writer.extend(Version::Ion_1_0, value).unwrap();
        writer.write();

        assert_eq!(bytes.to_vec(), writer.get());
    }

    #[test]
    fn test_timestamp2011_02() {
        let bytes =
            include_bytes!("../tests/ion-tests/iontestdata/good/timestamp/timestamp2011-02.10n");
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

        let mut writer = Writer::new(Version::Ion_1_0);
        writer.extend(Version::Ion_1_0, value).unwrap();
        writer.write();

        assert_eq!(bytes.to_vec(), writer.get());
    }

    #[test]
    fn test_timestamp2011_02_20() {
        let bytes =
            include_bytes!("../tests/ion-tests/iontestdata/good/timestamp/timestamp2011-02-20.10n");
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

        let mut writer = Writer::new(Version::Ion_1_0);
        writer.extend(Version::Ion_1_0, value).unwrap();
        writer.write();

        assert_eq!(bytes.to_vec(), writer.get());
    }

    #[test]
    fn test_timestamp2011_02_20T19_30_59_100_8_00() {
        let bytes = include_bytes!(
            "../tests/ion-tests/iontestdata/good/timestamp/timestamp2011-02-20T19_30_59_100-08_00.10n"
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

        let mut writer = Writer::new(Version::Ion_1_0);
        writer.extend(Version::Ion_1_0, value).unwrap();
        writer.write();

        assert_eq!(bytes.to_vec(), writer.get());
    }
}

// symbol tests
mod symbol {
    use self::assert_eq;
    use super::*;

    #[test]
    fn test_nullSymbol() {
        let bytes = include_bytes!("../tests/ion-tests/iontestdata/good/nullSymbol.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(value, vec![Data::Symbol(None).into()]);

        let mut writer = Writer::new(Version::Ion_1_0);
        writer.extend(Version::Ion_1_0, value).unwrap();
        writer.write();

        assert_eq!(bytes.to_vec(), writer.get());
    }

    #[test]
    fn test_symbolExplicitZero() {
        let bytes = include_bytes!("../tests/ion-tests/iontestdata/good/symbolExplicitZero.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(value, vec![Data::Symbol(Some(SymbolToken::Zero)).into()]);

        let mut writer = Writer::new(Version::Ion_1_0);
        writer.extend(Version::Ion_1_0, value).unwrap();
        writer.write();

        let output = writer.get();

        // It is not the case that bytes.to_vec() == writer.get(), because the read bytes use the
        // explicit form [113, 0], but the output bytes use the more compact implicit form [112].
        assert_eq!(bytes.to_vec(), [224, 1, 0, 234, 113, 0]);
        assert_eq!(output, [224, 1, 0, 234, 112]);
        let implicit_zero_bytes =
            include_bytes!("../tests/ion-tests/iontestdata/good/symbolImplicitZero.10n");
        assert_eq!(implicit_zero_bytes.to_vec(), output);
    }

    #[test]
    fn test_symbolImplicitZero() {
        let bytes = include_bytes!("../tests/ion-tests/iontestdata/good/symbolImplicitZero.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(value, vec![Data::Symbol(Some(SymbolToken::Zero)).into()]);

        let mut writer = Writer::new(Version::Ion_1_0);
        writer.extend(Version::Ion_1_0, value).unwrap();
        writer.write();

        assert_eq!(bytes.to_vec(), writer.get());
    }
}

// string tests
mod string {
    use self::assert_eq;
    use super::*;

    #[test]
    fn test_nullString() {
        let bytes = include_bytes!("../tests/ion-tests/iontestdata/good/nullString.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(value, vec![Data::String(None).into()]);

        let mut writer = Writer::new(Version::Ion_1_0);
        writer.extend(Version::Ion_1_0, value).unwrap();
        writer.write();

        assert_eq!(bytes.to_vec(), writer.get());
    }
}

// clob tests
mod clob {
    use self::assert_eq;
    use super::*;

    #[test]
    fn test_nullClob() {
        let bytes = include_bytes!("../tests/ion-tests/iontestdata/good/nullClob.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(value, vec![Data::Clob(None).into()]);

        let mut writer = Writer::new(Version::Ion_1_0);
        writer.extend(Version::Ion_1_0, value).unwrap();
        writer.write();

        assert_eq!(bytes.to_vec(), writer.get());
    }

    #[test]
    fn test_clobWithDel() {
        let bytes = include_bytes!("../tests/ion-tests/iontestdata/good/clobWithDel.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(
            value,
            vec![Data::Clob(Some(Clob { data: vec![127u8] })).into()]
        );

        let mut writer = Writer::new(Version::Ion_1_0);
        writer.extend(Version::Ion_1_0, value).unwrap();
        writer.write();

        assert_eq!(bytes.to_vec(), writer.get());
    }

    #[test]
    fn test_clobWithNonAsciiCharacter() {
        let bytes =
            include_bytes!("../tests/ion-tests/iontestdata/good/clobWithNonAsciiCharacter.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(
            value,
            vec![Data::Clob(Some(Clob { data: vec![128u8] })).into()]
        );

        let mut writer = Writer::new(Version::Ion_1_0);
        writer.extend(Version::Ion_1_0, value).unwrap();
        writer.write();

        assert_eq!(bytes.to_vec(), writer.get());
    }

    #[test]
    fn test_clobWithNullCharacter() {
        let bytes = include_bytes!("../tests/ion-tests/iontestdata/good/clobWithNullCharacter.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(
            value,
            vec![Data::Clob(Some(Clob { data: vec![0u8] })).into()]
        );

        let mut writer = Writer::new(Version::Ion_1_0);
        writer.extend(Version::Ion_1_0, value).unwrap();
        writer.write();

        assert_eq!(bytes.to_vec(), writer.get());
    }
}

// blob tests
mod blob {
    use self::assert_eq;
    use super::*;

    #[test]
    fn test_nullBlob() {
        let bytes = include_bytes!("../tests/ion-tests/iontestdata/good/nullBlob.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(value, vec![Data::Blob(None).into()]);

        let mut writer = Writer::new(Version::Ion_1_0);
        writer.extend(Version::Ion_1_0, value).unwrap();
        writer.write();

        assert_eq!(bytes.to_vec(), writer.get());
    }
}

// list tests
mod list {
    use self::assert_eq;
    use super::*;

    #[test]
    fn test_nullList() {
        let bytes = include_bytes!("../tests/ion-tests/iontestdata/good/nullList.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(value, vec![Data::List(None).into()]);

        let mut writer = Writer::new(Version::Ion_1_0);
        writer.extend(Version::Ion_1_0, value).unwrap();
        writer.write();

        assert_eq!(bytes.to_vec(), writer.get());
    }
}

// sexp tests
mod sexp {
    use self::assert_eq;
    use super::*;

    #[test]
    fn test_nullSexp() {
        let bytes = include_bytes!("../tests/ion-tests/iontestdata/good/nullSexp.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(value, vec![Data::Sexp(None).into()]);

        let mut writer = Writer::new(Version::Ion_1_0);
        writer.extend(Version::Ion_1_0, value).unwrap();
        writer.write();

        assert_eq!(bytes.to_vec(), writer.get());
    }
}

// struct tests
mod r#struct {
    use self::assert_eq;
    use super::*;
    use std::string::String;

    #[test]
    fn test_nullStruct() {
        let bytes = include_bytes!("../tests/ion-tests/iontestdata/good/nullStruct.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(value, vec![Data::Struct(None).into()]);

        let mut writer = Writer::new(Version::Ion_1_0);
        writer.extend(Version::Ion_1_0, value).unwrap();
        writer.write();

        assert_eq!(bytes.to_vec(), writer.get());
    }

    #[test]
    fn test_nopPadInsideEmptyStructNonZeroSymbolId() {
        let bytes = include_bytes!(
            "../tests/ion-tests/iontestdata/good/nopPadInsideEmptyStructNonZeroSymbolId.10n"
        );
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(
            value,
            vec![Data::Struct(Some(Struct { fields: vec![] })).into()]
        );

        let mut writer = Writer::new(Version::Ion_1_0);
        writer.extend(Version::Ion_1_0, value.clone()).unwrap();
        writer.write();

        assert_eq!(value, parse(&writer.get()).unwrap().1);
    }

    #[test]
    fn test_nopPadInsideEmptyStructZeroSymbolId() {
        let bytes = include_bytes!(
            "../tests/ion-tests/iontestdata/good/nopPadInsideEmptyStructZeroSymbolId.10n"
        );
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(
            value,
            vec![Data::Struct(Some(Struct { fields: vec![] })).into()]
        );

        let mut writer = Writer::new(Version::Ion_1_0);
        writer.extend(Version::Ion_1_0, value.clone()).unwrap();
        writer.write();

        assert_eq!(value, parse(&writer.get()).unwrap().1);
    }

    #[test]
    fn test_nopPadInsideStructWithNopPadThenValueZeroSymbolId() {
        let bytes = include_bytes!(
            "../tests/ion-tests/iontestdata/good/nopPadInsideStructWithNopPadThenValueZeroSymbolId.10n"
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

        let mut writer = Writer::new(Version::Ion_1_0);
        writer.extend(Version::Ion_1_0, value.clone()).unwrap();
        writer.write();

        assert_eq!(value, parse(&writer.get()).unwrap().1);
    }

    #[test]
    fn test_nopPadInsideStructWithValueThenNopPad() {
        let bytes = include_bytes!(
            "../tests/ion-tests/iontestdata/good/nopPadInsideStructWithValueThenNopPad.10n"
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

        let mut writer = Writer::new(Version::Ion_1_0);
        writer.extend(Version::Ion_1_0, value.clone()).unwrap();
        writer.write();

        assert_eq!(value, parse(&writer.get()).unwrap().1);
    }

    #[test]
    fn test_structAnnotatedEmpty() {
        let bytes = include_bytes!("../tests/ion-tests/iontestdata/good/structAnnotatedEmpty.10n");
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

        let mut writer = Writer::new(Version::Ion_1_0);
        writer.extend(Version::Ion_1_0, value.clone()).unwrap();
        writer.write();

        assert_eq!(value, parse(&writer.get()).unwrap().1);
    }

    #[test]
    fn test_structEmpty() {
        let bytes = include_bytes!("../tests/ion-tests/iontestdata/good/structEmpty.10n");
        let (remaining_bytes, value) = parse(bytes).unwrap();
        assert_eq!(remaining_bytes, &[] as &[u8]);
        assert_eq!(
            value,
            vec![Data::Struct(Some(Struct { fields: vec![] })).into()]
        );

        let mut writer = Writer::new(Version::Ion_1_0);
        writer.extend(Version::Ion_1_0, value).unwrap();
        writer.write();

        assert_eq!(bytes.to_vec(), writer.get());
    }

    #[test]
    fn test_structLen13() {
        let bytes = include_bytes!("../tests/ion-tests/iontestdata/good/structLen13.10n");
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

        let mut writer = Writer::new(Version::Ion_1_0);
        writer.extend(Version::Ion_1_0, value.clone()).unwrap();
        writer.write();

        assert_eq!(value, parse(&writer.get()).unwrap().1);
    }

    #[test]
    fn test_structLen14() {
        let bytes = include_bytes!("../tests/ion-tests/iontestdata/good/structLen14.10n");
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

        let mut writer = Writer::new(Version::Ion_1_0);
        writer.extend(Version::Ion_1_0, value.clone()).unwrap();
        writer.write();

        assert_eq!(value, parse(&writer.get()).unwrap().1);
    }

    #[test]
    fn test_structLen15() {
        let bytes = include_bytes!("../tests/ion-tests/iontestdata/good/structLen15.10n");
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

        let mut writer = Writer::new(Version::Ion_1_0);
        writer.extend(Version::Ion_1_0, value.clone()).unwrap();
        writer.write();

        assert_eq!(value, parse(&writer.get()).unwrap().1);
    }

    #[test]
    fn test_structOrdered() {
        let bytes = include_bytes!("../tests/ion-tests/iontestdata/good/structOrdered.10n");
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

        let mut writer = Writer::new(Version::Ion_1_0);
        writer.extend(Version::Ion_1_0, value.clone()).unwrap();
        writer.write();

        assert_eq!(value, parse(&writer.get()).unwrap().1);
    }

    #[test]
    fn test_structAnnotatedOrdered() {
        let bytes =
            include_bytes!("../tests/ion-tests/iontestdata/good/structAnnotatedOrdered.10n");
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

        let mut writer = Writer::new(Version::Ion_1_0);
        writer.extend(Version::Ion_1_0, value.clone()).unwrap();
        writer.write();

        assert_eq!(value, parse(&writer.get()).unwrap().1);
    }
}

// annotation tests
mod annotation {}
