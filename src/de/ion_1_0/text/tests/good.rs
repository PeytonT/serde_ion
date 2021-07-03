use core::iter;

use num_bigint::BigUint;
use time::UtcOffset;

use crate::de::ion_1_0::text::tests::{fractional_second, minute};
use crate::text::TextDate;
use crate::{symbols::SymbolToken, value as ion};

use super::{
    annot, blob_decoded, blob_encoded, blob_encoded_data, boolean, clob, clob_data, decimal, float,
    int_i64, int_i64_data, int_s, list, map, map_data, parse_file, sexp, sexp_data, string, symbol,
    symbol_data, test_path, timestamp, value, verify_tlvs,
};

// TODO: find a way to guarantee that all good test files are checked

#[test]
fn test_all_nulls() {
    let result = parse_file(&test_path("good/allNulls.ion"));

    let expected: ion::Value = list(vec![
        ion::Data::Null.into(),
        ion::Data::Null.into(),
        ion::Data::Bool(None).into(),
        ion::Data::Int(None).into(),
        ion::Data::Float(None).into(),
        ion::Data::Decimal(None).into(),
        ion::Data::Timestamp(None).into(),
        ion::Data::String(None).into(),
        ion::Data::Symbol(None).into(),
        ion::Data::Blob(None).into(),
        ion::Data::Clob(None).into(),
        ion::Data::Struct(None).into(),
        ion::Data::List(None).into(),
        ion::Data::Sexp(None).into(),
    ]);

    verify_tlvs(vec![expected], result);
}

#[test]
fn test_annotation_quoted_false() {
    let result = parse_file(&test_path("good/annotationQuotedFalse.ion"));

    let expected = value(int_i64_data(23), vec![annot("false")]);

    verify_tlvs(vec![expected], result);
}

#[test]
fn test_annotation_quoted_nan() {
    let result = parse_file(&test_path("good/annotationQuotedNan.ion"));

    let expected = value(int_i64_data(23), vec![annot("nan")]);

    verify_tlvs(vec![expected], result);
}

#[test]
fn test_annotation_quoted_neg_inf() {
    let result = parse_file(&test_path("good/annotationQuotedNegInf.ion"));

    let expected = value(int_i64_data(23), vec![annot("-inf")]);

    verify_tlvs(vec![expected], result);
}

#[test]
fn test_annotation_quoted_null() {
    let result = parse_file(&test_path("good/annotationQuotedNull.ion"));

    let expected = value(int_i64_data(23), vec![annot("null")]);

    verify_tlvs(vec![expected], result);
}

#[test]
fn test_annotation_quoted_null_int() {
    let result = parse_file(&test_path("good/annotationQuotedNullInt.ion"));

    let expected = value(int_i64_data(23), vec![annot("null.int")]);

    verify_tlvs(vec![expected], result);
}

#[test]
fn test_annotation_quoted_operator() {
    let result = parse_file(&test_path("good/annotationQuotedOperator.ion"));

    let expected = value(int_i64_data(23), vec![annot("@")]);

    verify_tlvs(vec![expected], result);
}

#[test]
fn test_annotation_quoted_pos_inf() {
    let result = parse_file(&test_path("good/annotationQuotedPosInf.ion"));

    let expected = value(int_i64_data(23), vec![annot("+inf")]);

    verify_tlvs(vec![expected], result);
}

#[test]
fn test_annotation_quoted_true() {
    let result = parse_file(&test_path("good/annotationQuotedTrue.ion"));

    let expected = value(int_i64_data(23), vec![annot("true")]);

    verify_tlvs(vec![expected], result);
}

#[test]
fn test_blank() {
    let result = parse_file(&test_path("good/blank.ion"));

    verify_tlvs(vec![], result);
}

#[test]
fn test_blobs() {
    let result = parse_file(&test_path("good/blobs.ion"));

    let expected = vec![
        blob_decoded(br"a b c d e f g h i j k l m n o p q r s t u v w x y z"),
        blob_decoded(br"A B C D E F G H I J K L M N O P Q R S T U V W X Y Z"),
        blob_decoded(br"1 2 3 4 5 6 7 8 9 0"),
        blob_decoded(br", . ; / [ ' ] \ = - 0 9 8 7 6 5 4 3 2 1 ` ~ ! @ # $ % ^ & * ( ) _ + | : < > ?"),
        blob_decoded(b"\x3a\x20\x53\x20\xa5\x20\x4f\x20\x00\x49\xbf"),
        blob_decoded(b"\xff\xfe\xfd\xfc\xfb\x00\x01\x02\x03\x04\x05"),
        blob_decoded(b"\x01\x11\x19\x1e\x2c\x37\x3c\x48\x51\x63\x67\x75\x7d\x8b\x8e\x9c\xa5\xb1\xb5\xc6\xcc\xd3\xdf\xef\xf6\xff\x00"),
        blob_decoded(br"A Very Very Very Very Large Test Blob"),
    ];

    verify_tlvs(expected, result);
}

#[test]
fn test_booleans() {
    let result = parse_file(&test_path("good/booleans.ion"));

    let expected = vec![
        ion::Data::Bool(Some(true)).into(),
        ion::Data::Bool(Some(false)).into(),
    ];

    verify_tlvs(expected, result);
}

#[test]
fn test_clobs() {
    let result = parse_file(&test_path("good/clobs.ion"));

    let expected = vec![
        clob(b"a b c d e f g h i j k l m n o p q r s t u v w x y z"),
        clob(b"A B C D E F G H I J K L M N O P Q R S T U V W X Y Z"),
        clob(b"1 2 3 4 5 6 7 8 9 0"),
        clob(b", . ; / [ ' ] \\ = - 0 9 8 7 6 5 4 3 2 1 ` ~ ! @ # $ % ^ & * ( ) _ + | : < > ?"),
        clob(b"\x00 \x07 \x08 \t \n \x0c \r \x0b \" \' ? \\\\ / \x00\x07\x08\t\n\x0c\r\x0b\"\'?\\\\/"),
        clob(b"\x7f \x66 \x00 \x5a\x5b\x00\x1c\x2d\x3f\xFf"),
        clob(b"\x7F \x66 \x00 \x5A\x5B\x00\x1C\x2D\x3F\xfF"),
        clob(b"Stuff to write on multiple lines if you want to"),
        clob(b""),
        clob(b""),
        clob(b""),
        clob(b"concatenated from a single line"),
        clob(b""),
        clob(b"a b c d e f g h i j k l m n o p q r s t u v w x y z A B C D E F G H I J K L M N O P Q R S T U V W X Y Z , . ; / [ ' ] \\ = - 0 9 8 7 6 5 4 3 2 1 ` ~ ! @ # $ % ^ & * ( ) _ + | : < > ? \x00 \x07 \x08 \t \n \x0c \r \x0b \" \' ? \\\\ / \x00\x07\x08\t\n\x0c\r\x0b\"\'?\\\\/\x7f \x66 \x00 \x5a\x5b\x00\x1c\x2d\x3f\x7F \x66 \x00 \x5A\x5B\x00\x1C\x2D\x3F"),
        clob(b"multi-line string\nwith embedded\nnew line\ncharacters"),
    ];

    verify_tlvs(expected, result);
}

#[test]
fn test_clobs_with_quotes() {
    let result = parse_file(&test_path("good/clobsWithQuotes.ion"));

    let expected = vec![
        clob(b"'''"),
        clob(b"''''''"),
        clob(b"\""),
        clob(b"\"\""),
        clob(b"'''\n12345678901234567890123456789012345678901234567890123456789012345678901234567890\n'''\n12345678901234567890123456789012345678901234567890123456789012345678901234567890\n'''\n"),
    ];

    verify_tlvs(expected, result);
}

#[test]
fn test_clobs_with_whitespace() {
    let result = parse_file(&test_path("good/clobsWithWhitespace.ion"));

    let expected = vec![
        clob(b"	"),
        clob(b""),
        clob(b""),
        clob(b" "),
        clob(b"	"),
        clob(b""),
        clob(b""),
        clob(b" "),
    ];

    verify_tlvs(expected, result);
}

#[test]
fn test_clobs_with_del() {
    let result = parse_file(&test_path("good/clobWithDel.ion"));

    let expected = vec![clob(b""), clob(b"")];

    verify_tlvs(expected, result);
}

#[test]
fn test_comment_multi_line_then_eof() {
    let result = parse_file(&test_path("good/commentMultiLineThenEof.ion"));

    let expected = symbol("abc");

    verify_tlvs(vec![expected], result);
}

#[test]
fn test_comment_single_line_then_eof() {
    let result = parse_file(&test_path("good/commentSingleLineThenEof.ion"));

    let expected = symbol("abc");

    verify_tlvs(vec![expected], result);
}

#[test]
fn test_decimal_64_bit_boundary() {
    let result = parse_file(&test_path("good/decimal64BitBoundary.ion"));

    let expected = vec![
        decimal("18446744073709551615", "0"),
        decimal("-18446744073709551615", "0"),
        decimal("18446744073709551616", "0"),
        decimal("-18446744073709551616", "0"),
    ];

    verify_tlvs(expected, result);
}

#[test]
fn test_decimal_e_values() {
    let result = parse_file(&test_path("good/decimal_e_values.ion"));

    let pos = iter::repeat_with(|| decimal("2718281828459045", "-15"));
    let pos_prec = iter::repeat_with(|| decimal("27182818284590450000000000", "-25"));
    let neg = iter::repeat_with(|| decimal("-2718281828459045", "-15"));
    let neg_prec = iter::repeat_with(|| decimal("-27182818284590450000000000", "-25"));

    let mut expected = vec![];

    expected.extend(pos.take(5));
    expected.extend(pos_prec.take(3));
    expected.extend(pos.take(2));

    expected.extend(neg.take(5));
    expected.extend(neg_prec.take(3));
    expected.extend(neg.take(2));

    expected.extend(pos.take(4));
    expected.extend(pos_prec.take(3));
    expected.extend(pos.take(2));

    expected.extend(neg.take(4));
    expected.extend(neg_prec.take(3));
    expected.extend(neg.take(2));

    verify_tlvs(expected, result);
}

#[test]
fn test_decimal_values() {
    let result = parse_file(&test_path("good/decimal_values.ion"));

    let expected = vec![
        decimal("1234560", "-1"),
        decimal("123456", "0"),
        decimal("123456", "1"),
        decimal("123456", "2"),
        decimal("123456", "3"),
        decimal("123456", "42"),
        decimal("123456", "-0"),
        decimal("123456", "-1"),
        decimal("123456", "-2"),
        decimal("123456", "-42"),
        decimal("123456", "-6"),
        decimal("123456", "-5"),
        decimal("123456", "-4"),
        decimal("123456", "-3"),
        decimal("123456", "-2"),
        decimal("123456", "-1"),
        decimal("1234560", "-2"),
        decimal("12345600", "-3"),
        decimal("123004560", "-1"),
        decimal("12300456", "-5"),
        decimal("12300456", "-4"),
        decimal("12300456", "-3"),
        decimal("123456", "39"),
        decimal("123456", "39"),
        decimal("123456", "-45"),
        decimal("777777", "6"),
        decimal("777777", "-8"),
        decimal("777777", "6"),
        decimal("777777", "699"),
        decimal("777777", "-701"),
        decimal("777777", "699"),
        decimal("-1234560", "-1"),
        decimal("-123456", "0"),
        decimal("-123456", "1"),
        decimal("-123456", "2"),
        decimal("-123456", "3"),
        decimal("-123456", "42"),
        decimal("-123456", "-0"),
        decimal("-123456", "-1"),
        decimal("-123456", "-2"),
        decimal("-123456", "-42"),
        decimal("-123456", "-6"),
        decimal("-123456", "-5"),
        decimal("-123456", "-4"),
        decimal("-123456", "-3"),
        decimal("-123456", "-2"),
        decimal("-123456", "-1"),
        decimal("-1234560", "-2"),
        decimal("-12345600", "-3"),
        decimal("-123004560", "-1"),
        decimal("-12300456", "-5"),
        decimal("-12300456", "-4"),
        decimal("-12300456", "-3"),
        decimal("-123456", "39"),
        decimal("-123456", "39"),
        decimal("-123456", "-45"),
        decimal("-777777", "6"),
        decimal("-777777", "-8"),
        decimal("-777777", "6"),
        decimal("-777777", "699"),
        decimal("-777777", "-701"),
        decimal("-777777", "699"),
    ];

    verify_tlvs(expected, result);
}

#[test]
fn test_decimal_zeroes() {
    let result = parse_file(&test_path("good/decimal_zeros.ion"));

    let expected = vec![
        decimal("0", "0"),
        decimal("0", "0"),
        decimal("0", "0"),
        decimal("0", "0"),
        decimal("0", "0"),
        decimal("0", "-1"),
        decimal("0", "-0"),
        decimal("0", "-0"),
        decimal("0", "-42"),
        decimal("0", "-313"),
        decimal("0", "+103"),
        decimal("0", "99"),
        decimal("0", "666"),
        decimal("0", "98"),
        decimal("0", "-90"),
        decimal("0", "-4"),
        decimal("-0", "0"),
        decimal("-0", "0"),
        decimal("-0", "0"),
        decimal("-0", "-1"),
        decimal("-0", "-0"),
        decimal("-0", "-0"),
        decimal("-0", "-42"),
        decimal("-0", "-313"),
        decimal("-0", "103"),
        decimal("-0", "99"),
        decimal("-0", "666"),
        decimal("-0", "98"),
        decimal("-0", "-90"),
        decimal("-0", "-4"),
    ];

    verify_tlvs(expected, result);
}

#[test]
fn test_negative_one_dot_two_eight() {
    let result = parse_file(&test_path("good/decimalNegativeOneDotTwoEight.ion"));

    let expected = vec![decimal("-128", "-2")];

    verify_tlvs(expected, result);
}

#[test]
fn test_decimals_with_underscores() {
    let result = parse_file(&test_path("good/decimalsWithUnderscores.ion"));

    let expected = vec![
        decimal("12345678", "-4"),
        decimal("1234", "0"),
        decimal("12345678", "-4"),
    ];

    verify_tlvs(expected, result);
}

#[test]
fn test_decimals_with_terminating_eof() {
    pretty_env_logger::try_init().ok();
    let result = parse_file(&test_path("good/decimalWithTerminatingEof.ion"));

    let expected = vec![decimal("123", "-2")];

    verify_tlvs(expected, result);
}

#[test]
fn test_empty() {
    let result = parse_file(&test_path("good/empty.ion"));

    verify_tlvs(vec![], result);
}

#[test]
fn test_eol_comment_cr() {
    let result = parse_file(&test_path("good/eolCommentCr.ion"));

    let expected = list(vec![]);

    verify_tlvs(vec![expected], result);
}

#[test]
fn test_eol_comment_crlf() {
    let result = parse_file(&test_path("good/eolCommentCrLf.ion"));

    let expected = list(vec![]);

    verify_tlvs(vec![expected], result);
}

#[test]
fn test_field_name_inf() {
    let result = parse_file(&test_path("good/fieldNameInf.ion"));

    let expected = map(vec![("inf".into(), boolean(false))]);

    verify_tlvs(vec![expected], result);
}

#[test]
fn test_field_name_quoted_false() {
    let result = parse_file(&test_path("good/fieldNameQuotedFalse.ion"));

    let expected = map(vec![("false".into(), boolean(false))]);

    verify_tlvs(vec![expected], result);
}

#[test]
fn test_field_name_quoted_nan() {
    let result = parse_file(&test_path("good/fieldNameQuotedNan.ion"));

    let expected = map(vec![("nan".into(), boolean(false))]);

    verify_tlvs(vec![expected], result);
}

#[test]
fn test_field_name_quoted_neg_inf() {
    let result = parse_file(&test_path("good/fieldNameQuotedNegInf.ion"));

    // TODO(amzn/ion-tests#64): when this is fixed the test will fail. s/+/-/.
    let expected = map(vec![("+inf".into(), boolean(false))]);

    verify_tlvs(vec![expected], result);
}

#[test]
fn test_field_name_quoted_null() {
    let result = parse_file(&test_path("good/fieldNameQuotedNull.ion"));

    let expected = map(vec![("null".into(), boolean(false))]);

    verify_tlvs(vec![expected], result);
}

#[test]
fn test_field_name_quoted_null_int() {
    let result = parse_file(&test_path("good/fieldNameQuotedNullInt.ion"));

    let expected = map(vec![("null.int".into(), boolean(false))]);

    verify_tlvs(vec![expected], result);
}

#[test]
fn test_field_name_quoted_pos_inf() {
    let result = parse_file(&test_path("good/fieldNameQuotedPosInf.ion"));

    let expected = map(vec![("+inf".into(), boolean(false))]);

    verify_tlvs(vec![expected], result);
}

#[test]
fn test_field_name_quoted_true() {
    let result = parse_file(&test_path("good/fieldNameQuotedTrue.ion"));

    let expected = map(vec![("true".into(), boolean(false))]);

    verify_tlvs(vec![expected], result);
}

#[test]
fn test_float_values() {
    let result = parse_file(&test_path("good/float_values.ion"));

    let expected = vec![
        float("123456.0e0"),
        float("123456e0"),
        float("123456e1"),
        float("123456e2"),
        float("123456e3"),
        float("123456e42"),
        float("123456e-0"),
        float("123456e-1"),
        float("123456e-2"),
        float("123456e-42"),
        float("0.123456e0"),
        float("1.23456e0"),
        float("12.3456e0"),
        float("123.456e0"),
        float("1234.56e0"),
        float("12345.6e0"),
        float("12345.60e0"),
        float("12345.600e0"),
        float("12300456.0e0"),
        float("123.00456e0"),
        float("1230.0456e0"),
        float("12300.456e0"),
        float("123.456e42"),
        float("123.456e+42"),
        float("123.456e-42"),
        float("77777.7e0007"),
        float("77777.7e-0007"),
        float("77777.7e+0007"),
    ];

    verify_tlvs(expected, result);
}

#[test]
fn test_float_zeroes() {
    let result = parse_file(&test_path("good/float_zeros.ion"));

    let expected = vec![
        float("0e0"),
        float("0E0"),
        float("0.0e0"),
        float("0e-0"),
        float("0E-0"),
        float("0e-42"),
        float("0E-313"),
        float("0e+103"),
        float("0E+99"),
        float("0E666"),
        float("0.0e99"),
        float("0.000e-87"),
        float("0.0000E45"),
        float("-0e0"),
        float("-0E0"),
        float("-0.0e0"),
        float("-0e-0"),
        float("-0E-0"),
        float("-0e-42"),
        float("-0E-313"),
        float("-0e+103"),
        float("-0E+99"),
        float("-0E666"),
        float("-0.0e99"),
        float("-0.000e-87"),
        float("-0.0000E45"),
    ];

    verify_tlvs(expected, result);
}

#[test]
fn test_float_dbl_min() {
    let result = parse_file(&test_path("good/floatDblMin.ion"));

    let expected = vec![
        float("2.2250738585072012e-308"),
        float("0.00022250738585072012e-304"),
        float("2.225073858507201200000e-308"),
        float("2.2250738585072012e-00308"),
        float("2.2250738585072012997800001e-308"),
    ];

    verify_tlvs(expected, result);
}

#[test]
fn test_float_specials() {
    let result = parse_file(&test_path("good/floatSpecials.ion"));

    match result.unwrap().get(0) {
        Some(ion::Value {
            value: ion::Data::List(Some(ion::List { values: list })),
            ..
        }) => {
            let mut values = list.iter();

            let mut next_float = || match values.next() {
                Some(ion::Value {
                    value: ion::Data::Float(Some(value)),
                    ..
                }) => value,
                other => {
                    log::error!("not what we're looking for: {:?}", other);
                    panic!("aaah");
                }
            };

            let first = next_float();
            assert!(first.is_nan());
            let second = next_float();
            assert!(second.is_infinite() && second.is_sign_positive());
            let third = next_float();
            assert!(third.is_infinite() && third.is_sign_negative());
        }
        _ => panic!("aaaaaaahhhhhhhhhh"),
    }
}

#[test]
fn test_floats_with_underscores() {
    let result = parse_file(&test_path("good/floatsWithUnderscores.ion"));

    let expected = vec![
        float("1234.5678e0"),
        float("1234e56"),
        float("1234.5678e90"),
    ];

    verify_tlvs(expected, result);
}

#[test]
fn test_float_with_terminating_eof() {
    let result = parse_file(&test_path("good/floatWithTerminatingEof.ion"));

    let expected = vec![float("1.23e1")];

    verify_tlvs(expected, result);
}

#[test]
fn test_hex_with_terminating_eof() {
    let result = parse_file(&test_path("good/hexWithTerminatingEof.ion"));

    let expected = vec![int_s("3", 16)];

    verify_tlvs(expected, result);
}

#[test]
fn test_inner_version_identifiers() {
    let result = parse_file(&test_path("good/innerVersionIdentifiers.ion"));

    let expected = vec![
        sexp(vec![
            symbol("$ion_1_0"),
            symbol("$ion_2300_34"),
            value(symbol_data("$ion_1_0"), vec![annot("foo")]),
            value(symbol_data("$ion_1_0"), vec![annot("$ion_1_0")]),
            sexp(vec![symbol("$ion_1_0")]),
        ]),
        list(vec![
            symbol("$ion_1_0"),
            symbol("$ion_2300_34"),
            value(symbol_data("$ion_1_0"), vec![annot("foo")]),
            value(symbol_data("$ion_1_0"), vec![annot("$ion_1_0")]),
            list(vec![symbol("$ion_1_0")]),
        ]),
        map(vec![
            ("a".into(), symbol("$ion_1_0")),
            ("b".into(), symbol("$ion_2300_34")),
            (
                "c".into(),
                value(symbol_data("$ion_1_0"), vec![annot("foo")]),
            ),
            (
                "d".into(),
                value(symbol_data("$ion_1_0"), vec![annot("$ion_1_0")]),
            ),
            ("e".into(), map(vec![("f".into(), symbol("$ion_1_0"))])),
        ]),
    ];

    verify_tlvs(expected, result);
}

#[test]
fn test_int_big_size_256() {
    let result = parse_file(&test_path("good/intBigSize256.ion"));

    let expected = vec![int_s("18173238162219679736857031944447898744767430095109316084451026048678348094928854458274167288816962557611640075817315237016025726423548207924331642028847993938530524659112028449811515920726159569583847554301932799584192974700038250645135419704389244690214111003505621818033044965879076306690914532152840279256440975668846810694285470204245958782248405612488959069641454132691581386219910938587286910894148564397155066367399697230287047229035630842240888106685623631032505806388903066971508775182055551847210338095961815021030725796281642316166745051164958432783938535334657296749823645911331793861360616240344479015948", 10)];

    verify_tlvs(expected, result);
}

#[test]
fn test_int_big_size_512() {
    let result = parse_file(&test_path("good/intBigSize512.ion"));

    let expected = vec![int_s("-FE95F4CFF19A8EE2EDBBEE30C7C0ACBB83BFC4C0A58E8B94BB6250AEEAF3DB8F41B0ACDBB94B990C518D96C5EE3C02E276E06E07570A2B6E5DEA9FE4FAC8475A84EFCA8A8432D6D463BF0CEB470B4AD9B3B0C80730492E5EE660BCA86932D933C471F178140C5256AFFE4EF5C0404D74B4B7776E77178B3281E1C5B65AD8866BCBAA6225C4E1C5B9624B19DCC6001AFC3535A3769C8E937B7E3F9073AB0053CC0FFEB34124D5D570749D0181F4D4DEDCED7D28F038247BEFA18CE02A3D1293DA637BB1AB6598BB6617A6A5CE0512C390236DBCA283ADF0291E6903FBD6819D4C5A8216C69E2083DA5B3FEFB0928B208345A39207C8461E38F793036146107559ADF2F40612D25F14D45D7E2780B45E2CF9B5790D91AAAF327AF3323E20242C2632A64725844F1D9E218AAB0D56EE99AE486034D7B3FBFC4DCE8C9CC2A793CE93AFFE81DEE7158DAD7F0623CE692C8ED0975DBEEF9A717A0B63F90AF4FEBC96785A6FF4E06B090A65D33C98932DF39F7C5B807956A19897E0C3463046DF2EB4DF624C7C43BEF48FAB381A857B9F5B6C1BDBD6B3270C107CD3BC1C41FE04E1DDAC69F14119DE961AF773285544F819F3951542F704B501FF0364BF54D14A86E19BEC39394C85A6B256C6233DA801A44F5DB98CCDD8D9BB6788C014216DD57CB64573333CEED5B5C72A4EE296E75B3E32ED69083675A6A8F6B8AC85DEAED88AD0A7", 16)];

    verify_tlvs(expected, result);
}

#[test]
fn test_integer_values() {
    let result = parse_file(&test_path("good/integer_values.ion"));

    let expected = vec![
        int_i64(0),
        int_i64(42),
        int_i64(2112),
        int_i64(-999),
        int_i64(-0),
        int_i64(987_654_321),
        int_i64(-123_456_789),
        int_s("10", 16),
        int_s("ff", 16),
        int_s("FF", 16),
        int_s("A", 16),
        int_s("AbCdEf", 16),
        int_s("123456789", 16),
        int_s("1234567890abcdef", 16),
        int_s("-1234567890ABCDEF", 16),
        int_s("0", 16),
        int_s("-0", 16),
        int_s("-FFFF", 16),
        int_s("00FF", 16),
        int_s("-00FF", 16),
    ];

    verify_tlvs(expected, result);
}

#[test]
fn test_int_negative_one_two_eight() {
    let result = parse_file(&test_path("good/intNegativeOneTwoEight.ion"));

    let expected = vec![int_i64(-128)];

    verify_tlvs(expected, result);
}

#[test]
fn test_int_neg_zero() {
    let result = parse_file(&test_path("good/intNegZero.ion"));

    let expected = vec![int_i64(-0)];

    verify_tlvs(expected, result);
}

#[test]
fn test_ints_with_underscores() {
    let result = parse_file(&test_path("good/intsWithUnderscores.ion"));

    let expected = vec![
        int_i64(123),
        int_s("abcd", 16),
        int_s("11110000", 2),
        int_s("100000", 10),
        int_s("-123", 10),
        int_s("-abcd", 16),
        int_s("-11110000", 2),
        int_s("-100000", 10),
        sexp(vec![int_i64(123)]),
        sexp(vec![int_s("abcd", 16)]),
        sexp(vec![int_s("11110000", 2)]),
        sexp(vec![int_s("100000", 10)]),
        sexp(vec![int_s("-123", 10)]),
        sexp(vec![int_s("-abcd", 16)]),
        sexp(vec![int_s("-11110000", 2)]),
        sexp(vec![int_s("-100000", 10)]),
        sexp(vec![int_i64(123), int_i64(123)]),
        sexp(vec![int_s("abcd", 16), int_s("abcd", 16)]),
        sexp(vec![int_s("11110000", 2), int_s("11110000", 2)]),
        sexp(vec![int_s("100000", 10), int_s("100000", 10)]),
        sexp(vec![int_s("-123", 10), int_s("-123", 10)]),
        sexp(vec![int_s("-abcd", 16), int_s("-abcd", 16)]),
        sexp(vec![int_s("-11110000", 2), int_s("-11110000", 2)]),
        sexp(vec![int_s("-100000", 10), int_s("-100000", 10)]),
    ];

    verify_tlvs(expected, result);
}

#[test]
fn test_int_with_terminating_eof() {
    let result = parse_file(&test_path("good/intWithTerminatingEof.ion"));

    let expected = vec![int_i64(1247)];

    verify_tlvs(expected, result);
}

#[test]
fn test_lists() {
    pretty_env_logger::try_init().ok();
    let result = parse_file(&test_path("good/lists.ion"));

    let expected = vec![
        list(vec![
            int_i64(1),
            int_i64(2),
            int_i64(3),
            int_i64(4),
            int_i64(5),
        ]),
        list(vec![
            int_i64(1),
            int_i64(2),
            int_i64(3),
            int_i64(4),
            int_i64(5),
        ]),
        list(vec![
            int_i64(1),
            list(vec![int_i64(2), int_i64(3)]),
            list(vec![list(vec![list(vec![int_i64(5)])])]),
        ]),
        list(vec![
            int_i64(1),
            sexp(vec![int_i64(2), int_i64(3)]),
            list(vec![int_i64(4), sexp(vec![int_i64(5)])]),
        ]),
        list(vec![
            boolean(true),
            decimal("34", "-1"),
            decimal("3", "6"),
            float("2.3e8"),
            string("string"),
            string("multi-string"),
            symbol("Symbol"),
            symbol("qSymbol"),
            clob(b"clob data"),
            blob_encoded(b"YmxvYiBkYXRh"),
            timestamp(TextDate::day(1970, 6, 6).unwrap(), None),
            ion::Data::Struct(None).into(),
        ]),
        list(vec![
            map(vec![("one".into(), int_i64(1))]),
            int_i64(2),
            int_i64(3),
        ]),
        list(vec![int_s("ab", 16)]),
        list(vec![symbol("symbol")]),
        list(vec![string("string")]),
        list(vec![symbol("symbol")]),
        list(vec![float("+inf")]),
    ];

    verify_tlvs(expected, result);
}

#[test]
fn test_local_symbol_table_import_zero_max_id() {
    let result = parse_file(&test_path("good/localSymbolTableImportZeroMaxId.ion"));

    let expected = vec![symbol("a")];

    verify_tlvs(expected, result);
}

#[test]
fn test_message_2() {
    let result = parse_file(&test_path("good/message2.ion"));

    let expected = vec![value(
        map_data(vec![
            ("submission_id".into(), int_i64(99999)),
            ("customer_id".into(), int_i64(1234)),
            ("sku".into(), string("XXX")),
            ("version".into(), int_i64(1)),
            ("marketplace_ids".into(), list(vec![int_i64(1)])),
            (
                "offer_listings".into(),
                list(vec![map(vec![("marketplace_id".into(), int_i64(1))])]),
            ),
            (
                "product".into(),
                map(vec![
                    (
                        "one".into(),
                        list(vec![map(vec![("value".into(), string("A"))])]),
                    ),
                    (
                        "two".into(),
                        list(vec![
                            map(vec![("value".into(), string("A"))]),
                            map(vec![("value".into(), string("B"))]),
                        ]),
                    ),
                    (
                        "three".into(),
                        list(vec![
                            map(vec![("value".into(), string("A"))]),
                            map(vec![("value".into(), string("B"))]),
                            map(vec![("value".into(), string("C"))]),
                        ]),
                    ),
                ]),
            ),
        ]),
        vec![annot("contribution")],
    )];

    verify_tlvs(expected, result);
}

#[test]
fn test_multiple_annotations() {
    let result = parse_file(&test_path("good/multipleAnnotations.ion"));

    let expected = vec![value(
        symbol_data("value"),
        vec![annot("annot1"), annot("annot2")],
    )];

    verify_tlvs(expected, result);
}

#[test]
fn test_non_nulls() {
    let result = parse_file(&test_path("good/nonNulls.ion"));

    let expected = vec![
        int_i64(0),
        decimal("0", "-1"),
        decimal("0", "0"),
        float("0e0"),
        string(""),
        string(""),
        blob_decoded(b""),
        clob(b""),
        list(vec![]),
        sexp(vec![]),
        map(vec![]),
    ];

    verify_tlvs(expected, result);
}

#[test]
fn test_not_version_marker() {
    let result = parse_file(&test_path("good/notVersionMarkers.ion"));

    let expected = vec![
        value(symbol_data("$ion_1_0"), vec![annot("a1")]),
        value(symbol_data("$ion_1234_1"), vec![annot("a2")]),
        value(symbol_data("$ion_1_0"), vec![annot("$ion_1_0")]),
        value(
            symbol_data("$ion_1_0"),
            vec![annot("a3"), annot("$ion_1234_2")],
        ),
        value(symbol_data("$ion_1_0"), vec![annot("$ion_symbol_table")]),
    ];

    verify_tlvs(expected, result);
}

#[test]
fn test_nulls() {
    let result = parse_file(&test_path("good/nulls.ion"));

    let expected = vec![
        ion::Data::Null.into(),
        ion::Data::Null.into(),
        ion::Data::Int(None).into(),
        ion::Data::Float(None).into(),
        ion::Data::Decimal(None).into(),
        ion::Data::Symbol(None).into(),
        ion::Data::String(None).into(),
        ion::Data::Timestamp(None).into(),
        ion::Data::Blob(None).into(),
        ion::Data::Clob(None).into(),
        ion::Data::Bool(None).into(),
        ion::Data::List(None).into(),
        ion::Data::Sexp(None).into(),
        ion::Data::Struct(None).into(),
    ];

    verify_tlvs(expected, result);
}

#[test]
fn test_octal_000() {
    let result = parse_file(&test_path("good/octal000.ion"));

    let expected = vec![string("0\x00000")];

    verify_tlvs(expected, result);
}

#[test]
fn test_one() {
    let result = parse_file(&test_path("good/one.ion"));

    let expected = vec![int_i64(1)];

    verify_tlvs(expected, result);
}

#[test]
fn test_operators() {
    let result = parse_file(&test_path("good/operators.ion"));

    let expected = vec![sexp(vec![
        symbol("!"),
        symbol("#"),
        symbol("%"),
        symbol("&"),
        symbol("*"),
        symbol("+"),
        symbol("-"),
        symbol("."),
        symbol("/"),
        symbol(";"),
        symbol("<"),
        symbol("="),
        symbol(">"),
        symbol("?"),
        symbol("@"),
        symbol("^"),
        symbol("`"),
        symbol("|"),
        symbol("~"),
    ])];

    verify_tlvs(expected, result);
}

#[test]
fn test_sexp_annotation_quoted_operator() {
    let result = parse_file(&test_path("good/sexpAnnotationQuotedOperator.ion"));

    let expected = vec![sexp(vec![value(int_i64_data(23), vec![annot("@")])])];

    verify_tlvs(expected, result);
}

#[test]
fn test_sexps() {
    let result = parse_file(&test_path("good/sexps.ion"));

    let expected = vec![
        sexp(vec![
            symbol("this"),
            symbol("is"),
            symbol("a"),
            symbol("sexp"),
            symbol("list"),
        ]),
        sexp(vec![
            symbol("`~!@/%^&*-+=|;<>?."),
            int_i64(3),
            symbol("--"),
            symbol("-"),
            int_i64(4),
        ]),
        sexp(vec![
            symbol("+"),
            symbol("++"),
            symbol("+-+"),
            symbol("-++"),
            symbol("-"),
            symbol("--"),
            symbol("---"),
            int_i64(-3),
            symbol("-"),
            int_i64(3),
            symbol("--"),
            int_i64(3),
            symbol("--"),
            int_i64(3),
        ]),
        sexp(vec![
            symbol("+"),
            symbol("++"),
            symbol("+-+"),
            symbol("-++"),
            symbol("-"),
            symbol("--"),
            symbol("---"),
            int_i64(-3),
            symbol("-"),
            int_i64(3),
            symbol("--"),
            int_i64(3),
            symbol("--"),
            int_i64(3),
        ]),
        sexp(vec![
            symbol("&"),
            sexp(vec![
                symbol("%"),
                symbol("-"),
                list(vec![int_i64(42), int_i64(3)]),
                symbol("+"),
                sexp(vec![int_i64(2)]),
                symbol("-"),
            ]),
        ]),
        sexp(vec![sexp(vec![sexp(vec![])])]),
        sexp(vec![list(vec![])]),
        sexp(vec![
            ion::Data::Null.into(),
            symbol("."),
            symbol("timestamps"),
        ]),
        sexp(vec![symbol("op1"), symbol("."), symbol("op2")]),
        sexp(vec![
            value(symbol_data("+++"), vec![annot("a_plus_plus_plus_operator")]),
            value(int_i64_data(3), vec![annot("a_3")]),
        ]),
        sexp(vec![
            value(symbol_data("+++"), vec![annot("a_plus_plus_plus_operator")]),
            value(int_i64_data(3), vec![annot("a_3")]),
        ]),
        sexp(vec![value(
            symbol_data("+++"),
            vec![annot("a_plus_plus_plus_operator")],
        )]),
    ];

    verify_tlvs(expected, result);
}

#[test]
fn test_strings() {
    let result = parse_file(&test_path("good/strings.ion"));

    let expected = vec![
        string("a b c d e f g h i j k l m n o p q r s t u v w x y z"),
        string("A B C D E F G H I J K L M N O P Q R S T U V W X Y Z"),
        string("1 2 3 4 5 6 7 8 9 0"),
        string(", . ; / [ ' ] \\ = - 0 9 8 7 6 5 4 3 2 1 ` ~ ! @ # $ % ^ & * ( ) _ + | : < > ?"),
        string(
            "\x00 \x07 \x08 \t \n \x0c \r \x0b \" \' ? \\\\ / \x00\x07\x08\t\n\x0c\r\x0b\"\'?\\\\/",
        ),
        string("\u{aa5f}"),
        string("\u{abcd} \u{d7ff} \u{ffff} \u{1234} \u{4e6a} \u{d37b}\u{f4c2}\u{0000}\x00\u{ff}"),
        string("\u{ABCD} \u{D7FF} \u{FFFF} \u{1234} \u{4E6A} \u{D37B}\u{F4C2}\u{0000}\x00\u{ff}"),
        string("\u{aBcD} \u{D7ff} \u{FffF} \u{1234} \u{4E6a} \u{d37B}\u{F4c2}\u{0000}\x00\u{ff}"),
        string("Stuff to write on multiple lines if you want to"),
        string(""),
        string(""),
        string(""),
        string("concatenated from a single line"),
        string(""),
        string("a b c d e f g h i j k l m n o p q r s t u v w x y z A B C D E F G H I J K L M N O P Q R S T U V W X Y Z , . ; / [ ' ] \\ = - 0 9 8 7 6 5 4 3 2 1 ` ~ ! @ # $ % ^ & * ( ) _ + | : < > ? \x00 \x07 \x08 \t \n \x0c \r \x0b \" \' ? \\\\ / \x00\x07\x08\t\n\x0c\r\x0b\"\'?\\\\/\u{abcd} \u{d7ff} \u{ffff} \u{1234} \u{4e6a} \u{d37b}\u{f4c2}\u{0000}\x00\u{ff}\u{ABCD} \u{D7FF} \u{FFFF} \u{1234} \u{4E6A} \u{D37B}\u{F4C2}\u{0000}\x00\u{ff}\u{aBcD} \u{D7ff} \u{FffF} \u{1234} \u{4E6a} \u{d37B}\u{F4c2}\u{0000}\x00\u{ff}"),
        string(""),
        string("multi-line string\nwith embedded\nnew line\ncharacters\u{1234}"),
        string("\x7f"),
        string("\x7f"),
    ];

    verify_tlvs(expected, result);
}

#[test]
fn test_strings2() {
    let result = parse_file(&test_path("good/strings2.ion"));

    let expected = vec![
        string("a b c d e f g h i j k l m n o p q r s t u v w x y z"),
        string("A B C D E F G H I J K L M N O P Q R S T U V W X Y Z"),
        string("1 2 3 4 5 6 7 8 9 0"),
        string(", . ; / [ ' ] \\ = - 0 9 8 7 6 5 4 3 2 1 ` ~ ! @ # $ % ^ & * ( ) _ + | : < > ?"),
        string(
            "\x00 \x07 \x08 \t \n \x0c \r \x0b \" \' ? \\\\ / \x00\x07\x08\t\n\x0c\r\x0b\"\'?\\\\/",
        ),
        string("\u{abcd} \u{ffff} \u{1234} \u{4e6a} \u{d37b}\u{f4c2}\u{0000}\x00\u{ff}"),
        string("\u{ABCD} \u{cFFF} \u{1234} \u{4E6A} \u{D37B}\u{F4C2}\u{0000}\x00\u{ff}"),
        string("\u{aBcD} \u{cffF} \u{1234} \u{4E6a} \u{d37B}\u{F4c2}\u{0000}\x00\u{ff}"),
        string("\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}"),
        string(".\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}"),
        string("..\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}"),
        string("...\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}\u{F987}"),
        string("Stuff to write on multiple lines if you want to"),
        string(""),
        string(""),
        string(""),
        string("concatenated from a single line"),
        string(""),
        string("a b c d e f g h i j k l m n o p q r s t u v w x y z A B C D E F G H I J K L M N O P Q R S T U V W X Y Z , . ; / [ ' ] \\ = - 0 9 8 7 6 5 4 3 2 1 ` ~ ! @ # $ % ^ & * ( ) _ + | : < > ? \x00 \x07 \x08 \t \n \x0c \r \x0b \" \' ? \\\\ / \x00\x07\x08\t\n\x0c\r\x0b\"\'?\\\\/\u{abcd} \u{cfff} \u{1234} \u{4e6a} \u{d37b}\u{f4c2}\u{0000}\x00\u{ff}\u{ABCD} \u{CFFF} \u{1234} \u{4E6A} \u{D37B}\u{F4C2}\u{0000}\x00\u{FF}\u{aBcD} \u{CffF} \u{1234} \u{4E6a} \u{d37B}\u{F4c2}\u{0000}\x00\u{fF}"),
        string(""),
        string("multi-line string\nwith embedded\nnew line\ncharacters"),
    ];

    verify_tlvs(expected, result);
}

#[test]
fn test_strings_cr_nl() {
    let result = parse_file(&test_path("good/strings_cr_nl.ion"));

    let expected = vec![string(
        "short1multi-line string\r\nwith embedded\nnew line\r\ncharacters",
    )];

    verify_tlvs(expected, result);
}

#[test]
fn test_strings_nl() {
    let result = parse_file(&test_path("good/strings_nl.ion"));

    let expected = vec![string(
        "short1multi-line string\nwith embedded\nnew line\ncharacters",
    )];

    verify_tlvs(expected, result);
}

#[test]
fn test_strings_with_whitespace() {
    let result = parse_file(&test_path("good/stringsWithWhitespace.ion"));

    let expected = vec![string("	"), string(""), string(""), string(" "), string("	 ")];

    verify_tlvs(expected, result);
}

#[test]
fn test_struct_field_annotations_unquoted_then_quoted() {
    let result = parse_file(&test_path(
        "good/structFieldAnnotationsUnquotedThenQuoted.ion",
    ));

    let expected = vec![map(vec![(
        "f".into(),
        value(ion::Data::Null, vec![annot("a"), annot("b")]),
    )])];

    verify_tlvs(expected, result);
}

#[test]
fn test_structs() {
    let result = parse_file(&test_path("good/structs.ion"));

    let symbol_iter = iter::repeat_with(|| {
        map(vec![
            ("a".into(), symbol("b")),
            ("c".into(), int_i64(42)),
            ("d".into(), map(vec![("e".into(), symbol("f"))])),
            ("g".into(), int_i64(3)),
        ])
    });

    let string_iter = iter::repeat_with(|| {
        map(vec![
            ("a".into(), string("b")),
            ("c".into(), int_i64(42)),
            ("d".into(), map(vec![("e".into(), string("f"))])),
            ("g".into(), int_i64(3)),
        ])
    });

    let simple_iter = iter::repeat_with(|| map(vec![("123456789ABCDEF".into(), symbol("v"))]));

    let mut expected = vec![];

    expected.extend(symbol_iter.take(2));
    expected.extend(string_iter.take(1));
    expected.extend(symbol_iter.take(2));
    expected.extend(string_iter.take(1));
    expected.extend(symbol_iter.take(2));
    expected.extend(string_iter.take(1));
    expected.extend(symbol_iter.take(2));
    expected.extend(string_iter.take(1));
    expected.extend(simple_iter.take(3));
    expected.push(map(vec![(
        "123456789ABCDEF123456789ABCDEF".into(),
        symbol("v"),
    )]));
    expected.push(map(vec![("123\n455".into(), symbol("v"))]));
    expected.push(map(vec![("123456789ABCDEF\nGHI".into(), symbol("v"))]));

    verify_tlvs(expected, result);
}

#[test]
fn test_subfield_int() {
    let result = parse_file(&test_path("good/subfieldInt.ion"));

    let expected = vec![
        decimal("126", "0"),
        decimal("127", "0"),
        decimal("128", "0"),
        decimal("-126", "0"),
        decimal("-127", "0"),
        decimal("-128", "0"),
        decimal("32766", "0"),
        decimal("32767", "0"),
        decimal("32768", "0"),
        decimal("-32766", "0"),
        decimal("-32767", "0"),
        decimal("-32768", "0"),
        decimal("8388606", "0"),
        decimal("8388607", "0"),
        decimal("8388608", "0"),
        decimal("-8388606", "0"),
        decimal("-8388607", "0"),
        decimal("-8388608", "0"),
        decimal("2147483646", "0"),
        decimal("2147483647", "0"),
        decimal("2147483648", "0"),
        decimal("-2147483646", "0"),
        decimal("-2147483647", "0"),
        decimal("-2147483648", "0"),
        decimal("549755813886", "0"),
        decimal("549755813887", "0"),
        decimal("549755813888", "0"),
        decimal("-549755813886", "0"),
        decimal("-549755813887", "0"),
        decimal("-549755813888", "0"),
        decimal("140737488355326", "0"),
        decimal("140737488355327", "0"),
        decimal("140737488355328", "0"),
        decimal("-140737488355326", "0"),
        decimal("-140737488355327", "0"),
        decimal("-140737488355328", "0"),
        decimal("36028797018963966", "0"),
        decimal("36028797018963967", "0"),
        decimal("36028797018963968", "0"),
        decimal("-36028797018963966", "0"),
        decimal("-36028797018963967", "0"),
        decimal("-36028797018963968", "0"),
        decimal("9223372036854775806", "0"),
        decimal("9223372036854775807", "0"),
        decimal("9223372036854775808", "0"),
        decimal("-9223372036854775806", "0"),
        decimal("-9223372036854775807", "0"),
        decimal("-9223372036854775808", "0"),
        decimal("18446744073709551614", "0"),
        decimal("18446744073709551615", "0"),
        decimal("18446744073709551616", "0"),
        decimal("-18446744073709551614", "0"),
        decimal("-18446744073709551615", "0"),
        decimal("-18446744073709551616", "0"),
        decimal("2361183241434822606846", "0"),
        decimal("2361183241434822606847", "0"),
        decimal("2361183241434822606848", "0"),
        decimal("-2361183241434822606846", "0"),
        decimal("-2361183241434822606847", "0"),
        decimal("-2361183241434822606848", "0"),
        decimal("604462909807314587353086", "0"),
        decimal("604462909807314587353087", "0"),
        decimal("604462909807314587353088", "0"),
        decimal("-604462909807314587353086", "0"),
        decimal("-604462909807314587353087", "0"),
        decimal("-604462909807314587353088", "0"),
    ];

    verify_tlvs(expected, result);
}

#[test]
fn test_subfield_uint() {
    let result = parse_file(&test_path("good/subfieldUint.ion"));

    let expected = vec![
        int_s("254", 10),
        int_s("255", 10),
        int_s("256", 10),
        int_s("65534", 10),
        int_s("65535", 10),
        int_s("65536", 10),
        int_s("16777214", 10),
        int_s("16777215", 10),
        int_s("16777216", 10),
        int_s("2147483646", 10),
        int_s("2147483647", 10),
        int_s("2147483648", 10),
        int_s("4294967294", 10),
        int_s("4294967295", 10),
        int_s("4294967296", 10),
        int_s("1099511627774", 10),
        int_s("1099511627775", 10),
        int_s("1099511627776", 10),
        int_s("281474976710654", 10),
        int_s("281474976710655", 10),
        int_s("281474976710656", 10),
        int_s("72057594037927934", 10),
        int_s("72057594037927935", 10),
        int_s("72057594037927936", 10),
        int_s("9223372036854775806", 10),
        int_s("9223372036854775807", 10),
        int_s("9223372036854775808", 10),
        int_s("18446744073709551614", 10),
        int_s("18446744073709551615", 10),
        int_s("18446744073709551616", 10),
        int_s("4722366482869645213694", 10),
        int_s("4722366482869645213695", 10),
        int_s("4722366482869645213696", 10),
        int_s("1208925819614629174706174", 10),
        int_s("1208925819614629174706175", 10),
        int_s("1208925819614629174706176", 10),
    ];

    verify_tlvs(expected, result);
}

#[test]
fn test_subfield_var_int() {
    let result = parse_file(&test_path("good/subfieldVarInt.ion"));

    let expected = vec![
        decimal("0", "62"),
        decimal("0", "63"),
        decimal("0", "64"),
        decimal("0", "8190"),
        decimal("0", "8191"),
        decimal("0", "8192"),
        decimal("0", "1048574"),
        decimal("0", "1048575"),
        decimal("0", "1048576"),
        decimal("0", "134217726"),
        decimal("0", "134217727"),
        decimal("0", "134217728"),
        decimal("0", "2147483646"),
        decimal("0", "2147483647"),
        // decimal("0", "2147483648"), // Outstanding bug in Amazon internal implementation.
        decimal("123456789012345678901234567890123456789012345678901234567890", "-61"),
        decimal("1234567890123456789012345678901234567890123456789012345678901", "-62"),
        decimal("12345678901234567890123456789012345678901234567890123456789012", "-63"),
        decimal("1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678", "-8189"),
        decimal("12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789", "-8190"),
        decimal("123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890", "-8191"),
    ];

    verify_tlvs(expected, result);
}

// TODO(#13): need a sparse data structure for symbol tables before this test is reasonable.
#[ignore]
#[test]
fn test_subfield_var_uint() {
    let result = parse_file(&test_path("good/subfieldVarUInt.ion"));

    let expected = vec![
        value(int_i64_data(1), vec![annot("boundary-1")]),
        value(int_i64_data(1), vec![annot("boundary")]),
        value(int_i64_data(1), vec![annot("boundary+1")]),
        value(int_i64_data(1), vec![annot("boundary-1")]),
        value(int_i64_data(1), vec![annot("boundary")]),
        value(int_i64_data(1), vec![annot("boundary+1")]),
        value(int_i64_data(1), vec![annot("boundary-1")]),
        value(int_i64_data(1), vec![annot("boundary")]),
        value(int_i64_data(1), vec![annot("boundary+1")]),
        value(int_i64_data(1), vec![annot("boundary-1")]),
        value(int_i64_data(1), vec![annot("boundary")]),
        value(int_i64_data(1), vec![annot("boundary+1")]),
    ];

    verify_tlvs(expected, result);
}

// TODO(#13): need a sparse data structure for symbol tables before this test is reasonable.
#[ignore]
#[test]
fn test_subfield_var_uint_15bit() {
    let result = parse_file(&test_path("good/subfieldVarUInt15bit.ion"));

    let expected = vec![];

    verify_tlvs(expected, result);
}

// TODO(#13): need a sparse data structure for symbol tables before this test is reasonable.
#[ignore]
#[test]
fn test_subfield_var_uint_16bit() {
    let result = parse_file(&test_path("good/subfieldVarUInt16bit.ion"));

    let expected = vec![];

    verify_tlvs(expected, result);
}

// TODO(#13): need a sparse data structure for symbol tables before this test is reasonable.
#[ignore]
#[test]
fn test_subfield_var_uint_32bit() {
    let result = parse_file(&test_path("good/subfieldVarUInt32bit.ion"));

    let expected = vec![];

    verify_tlvs(expected, result);
}

#[test]
fn test_symbol_empty() {
    let result = parse_file(&test_path("good/symbolEmpty.ion"));

    let expected = vec![
        symbol(""),
        map(vec![("".into(), symbol("abc"))]),
        value(symbol_data("abc"), vec![annot("")]),
        value(symbol_data(""), vec![annot("")]),
        map(vec![("".into(), value(symbol_data(""), vec![annot("")]))]),
        value(symbol_data(""), vec![annot("abc")]),
        map(vec![("".into(), symbol("abc"))]),
    ];

    verify_tlvs(expected, result);
}

#[test]
fn test_symbol_empty_with_cr() {
    let result = parse_file(&test_path("good/symbolEmptyWithCR.ion"));

    let expected = vec![symbol("")];

    verify_tlvs(expected, result);
}

#[test]
fn test_symbol_empty_with_cr_lf() {
    let result = parse_file(&test_path("good/symbolEmptyWithCRLF.ion"));

    let expected = vec![symbol("")];

    verify_tlvs(expected, result);
}

#[test]
fn test_symbol_empty_with_lf() {
    let result = parse_file(&test_path("good/symbolEmptyWithLF.ion"));

    let expected = vec![symbol("")];

    verify_tlvs(expected, result);
}

#[test]
fn test_symbol_empty_with_lflf() {
    let result = parse_file(&test_path("good/symbolEmptyWithLFLF.ion"));

    let expected = vec![symbol("")];

    verify_tlvs(expected, result);
}

#[test]
fn test_symbols() {
    let result = parse_file(&test_path("good/symbols.ion"));

    let expected = vec![
        symbol("a b c d e f g h i j k l m n o p q r s t u v w x y z"),
        symbol("A B C D E F G H I J K L M N O P Q R S T U V W X Y Z"),
        symbol("1 2 3 4 5 6 7 8 9 0"),
        symbol(", . ; / [ \' ] \\ = - 0 9 8 7 6 5 4 3 2 1 ` ~ ! @ # $ % ^ & * ( ) _ + | : < > ?"),
        symbol(
            "\x00 \x07 \x08 \t \n \x0c \r \x0b \" \' ? \\\\ / \x00\x07\x08\t\n\x0c\r\x0b\"\'?\\\\/",
        ),
        symbol("\u{abcd} \u{d7ff} \u{ffff} \u{1234} \u{4e6a} \u{d37b}\u{f4c2}\u{0000}\x00\u{ff}"),
        symbol("\u{ABCD} \u{D7FF} \u{FFFF} \u{1234} \u{4E6A} \u{D37B}\u{F4C2}\u{0000}\x00\u{ff}"),
        symbol("\u{aBcD} \u{D7ff} \u{FffF} \u{1234} \u{4E6a} \u{d37B}\u{F4c2}\u{0000}\x00\u{ff}"),
        symbol("bareSymbol"),
        symbol("BareSymbol"),
        symbol("$bare"),
        symbol("_bare"),
        symbol("zzzzz"),
        symbol("aaaaa"),
        symbol("ZZZZZ"),
        symbol("AAAAA"),
        symbol("z"),
        symbol("Z"),
        symbol("a"),
        symbol("A"),
        symbol("_"),
        symbol("$"),
        symbol("_9876543210"),
        symbol("$ion_symbol_table"),
        symbol("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789$_"),
        symbol("$99"),
    ];

    verify_tlvs(expected, result);
}

#[test]
fn test_symbol_with_del() {
    let result = parse_file(&test_path("good/symbolWithDel.ion"));

    let expected = vec![symbol("\x7f")];

    verify_tlvs(expected, result);
}

#[test]
fn test_symbol_with_whitespace() {
    let result = parse_file(&test_path("good/symbolWithSpecialWhitespace.ion"));

    let expected = vec![symbol("\x09"), symbol("\x0B"), symbol("\x0C")];

    verify_tlvs(expected, result);
}

#[test]
fn test_symbol_zero() {
    let result = parse_file(&test_path("good/symbolZero.ion"));

    let expected = vec![
        ion::Data::Symbol(Some(SymbolToken::Zero)).into(),
        value(symbol_data("abc"), vec![SymbolToken::Zero]),
        map(vec![(SymbolToken::Zero, symbol("abc"))]),
        map(vec![(
            SymbolToken::Zero,
            value(symbol_data("abc"), vec![SymbolToken::Zero]),
        )]),
        map(vec![(
            SymbolToken::Zero,
            value(
                ion::Data::Symbol(Some(SymbolToken::Zero)),
                vec![SymbolToken::Zero],
            ),
        )]),
        sexp(vec![
            ion::Data::Symbol(Some(SymbolToken::Zero)).into(),
            value(
                ion::Data::Symbol(Some(SymbolToken::Zero)),
                vec![SymbolToken::Zero],
            ),
        ]),
    ];

    verify_tlvs(expected, result);
}

#[test]
fn test_testfile0() {
    let result = parse_file(&test_path("good/testfile0.ion"));

    let expected = vec![value(
        map_data(vec![
            ("lname".into(), string("smith")),
            ("fname".into(), string("john")),
            (
                "phonelist".into(),
                list(vec![
                    map(vec![
                        ("ac".into(), int_i64(206)),
                        ("prefix".into(), int_i64(234)),
                        ("suffix".into(), int_i64(2934)),
                    ]),
                    map(vec![
                        ("ac".into(), int_i64(444)),
                        ("prefix".into(), int_i64(333)),
                        ("suffix".into(), int_i64(2222)),
                    ]),
                ]),
            ),
            ("age".into(), string("6483020949")),
        ]),
        vec![annot("contact")],
    )];

    verify_tlvs(expected, result);
}

#[test]
fn test_testfile1() {
    let result = parse_file(&test_path("good/testfile1.ion"));

    let expected = vec![value(
        map_data(vec![
            ("name".into(), string("widgets")),
            ("quantity".into(), string("6483021036")),
        ]),
        vec![annot("PurchaseOrder")],
    )];

    verify_tlvs(expected, result);
}

// testfile 2 is conspicuously missing.

#[test]
fn test_testfile3() {
    let result = parse_file(&test_path("good/testfile3.ion"));

    let expected = vec![value(
        map_data(vec![
            (
                "Header".into(),
                map(vec![(
                    "alertcontrol".into(),
                    map(vec![
                        ("priority".into(), int_i64(1)),
                        ("expires".into(), string("6483021034")),
                    ]),
                )]),
            ),
            (
                "Body".into(),
                map(vec![(
                    "alert".into(),
                    map(vec![("msg".into(), string("The printer is on fire!"))]),
                )]),
            ),
        ]),
        vec![annot("Envelope")],
    )];

    verify_tlvs(expected, result);
}

#[test]
fn test_testfile4() {
    let result = parse_file(&test_path("good/testfile4.ion"));

    let expected = vec![value(
        map_data(vec![("v".into(), string(""))]),
        vec![annot("b")],
    )];

    verify_tlvs(expected, result);
}

#[test]
fn test_testfile5() {
    let result = parse_file(&test_path("good/testfile5.ion"));

    let expected = vec![value(
        map_data(vec![("v".into(), string(""))]),
        vec![annot("b")],
    )];

    verify_tlvs(expected, result);
}

#[test]
fn test_testfile6() {
    let result = parse_file(&test_path("good/testfile6.ion"));

    let expected = vec![value(
        map_data(
            (1..=9)
                .map(|i| {
                    (
                        (format!("v{}", i).as_str()).into(),
                        string(&format!("xv{}", i)),
                    )
                })
                .collect(),
        ),
        vec![annot("b")],
    )];

    verify_tlvs(expected, result);
}

#[test]
fn test_testfile7() {
    let result = parse_file(&test_path("good/testfile7.ion"));

    let expected = vec![value(
        map_data(vec![
            ("x".into(), string("6483021024")),
            ("prefix".into(), symbol("xs")),
            (
                "schema".into(),
                map(vec![("attributeFormDefault".into(), string("qualified"))]),
            ),
        ]),
        vec![annot("root5678901234")],
    )];

    verify_tlvs(expected, result);
}

#[test]
fn test_testfile8() {
    let result = parse_file(&test_path("good/testfile8.ion"));

    let expected = vec![value(
        map_data(vec![
            ("x".into(), string("6483021025")),
            ("targetNamespace".into(), string("x-schema:ado-schema.xml")),
            (
                "xmlns1".into(),
                map(vec![(
                    "namespace1".into(),
                    string("x-schema:ado-schema.xml"),
                )]),
            ),
            (
                "xmlns2".into(),
                map(vec![
                    ("prefix".into(), symbol("xs")),
                    (
                        "namespace2".into(),
                        string("http://www.w3.org/2001/XMLSchema"),
                    ),
                ]),
            ),
            (
                "schema".into(),
                map(vec![
                    ("attributeFormDefault".into(), string("qualified")),
                    ("elementFormDefault".into(), string("qualified")),
                    ("element".into(), map(vec![("name".into(), string("data"))])),
                ]),
            ),
        ]),
        vec![annot("root")],
    )];

    verify_tlvs(expected, result);
}

#[test]
fn test_testfile9() {
    let result = parse_file(&test_path("good/testfile9.ion"));

    let expected = vec![value(map_data(vec![]), vec![annot("b")])];

    verify_tlvs(expected, result);
}

#[test]
fn test_testfile10() {
    let result = parse_file(&test_path("good/testfile10.ion"));

    let expected = vec![value(
        map_data(vec![
            ("u".into(), int_i64(1)),
            ("i".into(), int_i64(-2)),
            ("f".into(), float("3.1e1")),
            ("d".into(), decimal("420", "-2")),
            ("s".into(), string("hi")),
            ("id".into(), symbol("hi")),
            ("id2".into(), symbol("by")),
        ]),
        vec![annot("m")],
    )];

    verify_tlvs(expected, result);
}

#[test]
fn test_testfile11() {
    let result = parse_file(&test_path("good/testfile11.ion"));

    let expected = vec![value(
        map_data(vec![
            (
                "thisisaverylongidentifier_to_keep_the_parser_busy_u".into(),
                int_i64(1),
            ),
            ("i23456789012".into(), int_i64(-2)),
            ("f234567890123".into(), float("3.1e1")),
            ("d2345678901234".into(), decimal("420", "-2")),
            ("s23456789012345".into(), string("hi")),
            ("id".into(), symbol("hi")),
            ("id2".into(), symbol("by")),
        ]),
        vec![annot("m")],
    )];

    verify_tlvs(expected, result);
}

#[test]
fn test_testfile12() {
    let result = parse_file(&test_path("good/testfile12.ion"));

    let expected = vec![value(
        map_data(vec![
            ("u".into(), map(vec![])),
            ("i".into(), list(vec![int_i64(-2)])),
            ("i0".into(), list(vec![])),
            ("i2".into(), list(vec![list(vec![]), list(vec![])])),
            ("f".into(), float("3.1e1")),
            ("d".into(), decimal("420", "-2")),
            ("s".into(), string("hi")),
            ("id".into(), symbol("hi")),
            ("id2".into(), symbol("by")),
        ]),
        vec![annot("m")],
    )];

    verify_tlvs(expected, result);
}

#[test]
fn test_testfile13() {
    let result = parse_file(&test_path("good/testfile13.ion"));

    let expected = vec![value(
        map_data(vec![
            ("b1".into(), boolean(true)),
            ("b2".into(), boolean(false)),
            ("i".into(), int_i64(0)),
            ("d".into(), decimal("0", "-1")),
            ("e".into(), float("0.0e0")),
        ]),
        vec![annot("m")],
    )];

    verify_tlvs(expected, result);
}

#[test]
fn test_testfile14() {
    let result = parse_file(&test_path("good/testfile14.ion"));

    let expected = vec![value(
        map_data(vec![
            ("f1".into(), float("0.0e0")),
            ("f2".into(), float("0.0001e0")),
            ("f3".into(), float("1.00e0")),
            ("f4".into(), float("1.0e5")),
            ("f5".into(), float("123456789012345.0e0")),
        ]),
        vec![annot("m")],
    )];

    verify_tlvs(expected, result);
}

// Are these...
#[test]
fn test_testfile15() {
    let result = parse_file(&test_path("good/testfile15.ion"));

    let expected = vec![value(
        map_data(vec![
            ("f1".into(), float("0.0e0")),
            ("f2".into(), float("0.0001e0")),
            ("f3".into(), float("1.00e0")),
            ("f4".into(), float("1.0e5")),
            ("f5".into(), float("123456789012345.0e0")),
        ]),
        vec![annot("m")],
    )];

    verify_tlvs(expected, result);
}

// all the same?
#[test]
fn test_testfile16() {
    let result = parse_file(&test_path("good/testfile16.ion"));

    let expected = vec![value(
        map_data(vec![
            ("f1".into(), float("0.0e0")),
            ("f2".into(), float("0.0001e0")),
            ("f3".into(), float("1.00e0")),
            ("f4".into(), float("1.0e5")),
            ("f5".into(), float("123456789012345.0e0")),
        ]),
        vec![annot("m")],
    )];

    verify_tlvs(expected, result);
}

#[test]
fn test_testfile17() {
    let result = parse_file(&test_path("good/testfile17.ion"));

    let expected = vec![int_i64(42)];

    verify_tlvs(expected, result);
}

#[test]
fn test_testfile18() {
    let result = parse_file(&test_path("good/testfile18.ion"));

    let expected = vec![int_i64(42)];

    verify_tlvs(expected, result);
}

#[test]
fn test_testfile19() {
    let result = parse_file(&test_path("good/testfile19.ion"));

    let expected = vec![value(
        sexp_data(vec![
            symbol("this"),
            symbol("is"),
            symbol("an"),
            symbol("expression"),
        ]),
        vec![annot("testexpr")],
    )];

    verify_tlvs(expected, result);
}

#[test]
fn test_testfile20() {
    let result = parse_file(&test_path("good/testfile20.ion"));

    let expected = vec![value(
        sexp_data(vec![symbol("aa"), string("ss"), symbol("bb")]),
        vec![annot("te2")],
    )];

    verify_tlvs(expected, result);
}

#[test]
fn test_testfile21() {
    let result = parse_file(&test_path("good/testfile21.ion"));

    let expected = vec![value(
        sexp_data(vec![
            symbol("this"),
            symbol("is"),
            string("a string"),
            symbol("an"),
            symbol("expression"),
            symbol("with"),
            decimal("140", "-1"),
            symbol("nested"),
            symbol("stuff"),
            list(vec![symbol("some"), symbol("data")]),
            map(vec![("a".into(), int_i64(1)), ("b".into(), int_i64(3))]),
        ]),
        vec![annot("te3")],
    )];

    verify_tlvs(expected, result);
}

#[test]
fn test_testfile22() {
    let result = parse_file(&test_path("good/testfile22.ion"));

    let expected = vec![sexp(vec![
        ion::Data::Null.into(),
        ion::Data::Null.into(),
        ion::Data::Bool(None).into(),
        ion::Data::Int(None).into(),
        ion::Data::Float(None).into(),
        ion::Data::Decimal(None).into(),
        ion::Data::Timestamp(None).into(),
        ion::Data::Symbol(None).into(),
        ion::Data::String(None).into(),
        ion::Data::List(None).into(),
        ion::Data::Struct(None).into(),
    ])];

    verify_tlvs(expected, result);
}

#[test]
fn test_testfile23() {
    let result = parse_file(&test_path("good/testfile23.ion"));

    let expected = vec![sexp(vec![
        boolean(true),
        boolean(false),
        int_i64(0),
        int_i64(1),
        int_i64(2),
        int_i64(12_345_678),
        int_i64(2_000_000_000),
        int_i64(4_000_000_000),
        int_i64(5_000_000_000),
        int_i64(20_000_000_000),
        int_i64(-0),
        int_i64(-1),
        int_i64(-2),
        int_i64(-12_345_678),
        int_i64(-2_000_000_000),
        int_i64(-4_000_000_000),
        int_i64(-5_000_000_000),
        int_i64(-20_000_000_000),
        decimal("0", "-1"),
        decimal("10", "-1"),
        decimal("20", "-1"),
        decimal("12345678", "-6"),
        decimal("2000000000", "0"),
        decimal("4000000000", "0"),
        decimal("5000000000", "0"),
        decimal("2000000000000", "-2"),
        decimal("-0", "-1"),
        decimal("-10", "-1"),
        decimal("-20", "-1"),
        decimal("-12345678", "-6"),
        decimal("-2000000000", "0"),
        decimal("-4000000000", "0"),
        decimal("-5000000000", "0"),
        decimal("-2000000000000", "-2"),
        float("0.0e0"),
        float("1.0e0"),
        float("2.0e0"),
        float("12.345678e0"),
    ])];

    verify_tlvs(expected, result);
}

#[test]
fn test_testfile24() {
    let result = parse_file(&test_path("good/testfile24.ion"));

    let expected = vec![sexp(vec![
        float("12.345678e0"),
        float("2000000000e0"),
        float("4000000000e0"),
        float("5000000000e0"),
        float("20000000000.00e0"),
        float("-0.0e0"),
        float("-1.0e0"),
        float("-2.0e0"),
        float("-12.345678e0"),
        float("-2000000000e0"),
        float("-4000000000e0"),
        float("-5000000000e0"),
        float("-20000000000.00e0"),
    ])];

    verify_tlvs(expected, result);
}

#[test]
fn test_testfile25() {
    let result = parse_file(&test_path("good/testfile25.ion"));

    let expected = vec![sexp(vec![
        timestamp(TextDate::day(2007, 11, 20).unwrap(), None),
        timestamp(
            TextDate::day(2008, 12, 23).unwrap(),
            Some(minute(Some(UtcOffset::UTC), 23, 0)),
        ),
        timestamp(
            TextDate::day(2008, 12, 23).unwrap(),
            Some(fractional_second(
                Some(UtcOffset::east_hours(7)),
                23,
                0,
                1,
                BigUint::from(123u32),
                -3,
            )),
        ),
        timestamp(
            TextDate::day(2008, 12, 23).unwrap(),
            Some(fractional_second(
                Some(UtcOffset::west_hours(6)),
                23,
                0,
                2,
                BigUint::from(456u32),
                -3,
            )),
        ),
        timestamp(
            TextDate::day(2008, 12, 23).unwrap(),
            Some(fractional_second(
                Some(UtcOffset::east_hours(8)),
                23,
                0,
                3,
                BigUint::from(789u32),
                -3,
            )),
        ),
    ])];

    verify_tlvs(expected, result);
}

#[test]
fn test_testfile26() {
    let result = parse_file(&test_path("good/testfile26.ion"));

    let expected = vec![sexp(vec![blob_encoded(b"2dDS")])];

    verify_tlvs(expected, result);
}

#[test]
fn test_testfile28() {
    let result = parse_file(&test_path("good/testfile28.ion"));

    let expected = vec![sexp(vec![value(
        clob_data(b"2007-\x00sdf-11-20"),
        vec![annot("sjis")],
    )])];

    verify_tlvs(expected, result);
}

#[test]
fn test_testfile29() {
    let result = parse_file(&test_path("good/testfile29.ion"));

    let expected = vec![sexp(vec![value(
        blob_encoded_data(b"2dDSGZ/0az07+sdf+11+230="),
        vec![annot("ablob")],
    )])];

    verify_tlvs(expected, result);
}

#[test]
fn test_testfile30() {
    let result = parse_file(&test_path("good/testfile30.ion"));

    let expected = vec![map(vec![
        ("st1".into(), symbol("v1")),
        ("st2".into(), symbol("v2")),
    ])];

    verify_tlvs(expected, result);
}

#[test]
fn test_testfile31() {
    let result = parse_file(&test_path("good/testfile31.ion"));

    let expected = vec![sexp(vec![
        decimal("2000000000", "0"),
        decimal("4000000000", "0"),
        decimal("5000000000", "0"),
        decimal("2000000000000", "-2"),
        decimal("-0", "-1"),
        decimal("-10", "-1"),
    ])];

    verify_tlvs(expected, result);
}

// Where hath thou gone, testfile32.ion?

#[test]
fn test_testfile33() {
    let result = parse_file(&test_path("good/testfile33.ion"));

    let expected = vec![value(
        map_data(vec![
            (
                "whenDate".into(),
                timestamp(TextDate::day(2007, 1, 31).unwrap(), None),
            ),
            (
                "whenDate".into(),
                timestamp(
                    TextDate::day(2007, 1, 31).unwrap(),
                    Some(minute(Some(UtcOffset::UTC), 1, 2)),
                ),
            ),
            (
                "whenDate".into(),
                timestamp(
                    TextDate::day(2007, 1, 31).unwrap(),
                    Some(fractional_second(
                        Some(UtcOffset::UTC),
                        1,
                        4,
                        5,
                        BigUint::from(385u32),
                        -3,
                    )),
                ),
            ),
            (
                "whenDate".into(),
                timestamp(
                    TextDate::day(2007, 1, 31).unwrap(),
                    Some(fractional_second(
                        Some(UtcOffset::east_minutes(60 + 11)),
                        1,
                        4,
                        5,
                        BigUint::from(385u32),
                        -3,
                    )),
                ),
            ),
        ]),
        vec![annot("dates")],
    )];

    verify_tlvs(expected, result);
}

#[test]
fn test_testfile34() {
    let result = parse_file(&test_path("good/testfile34.ion"));

    let expected = vec![list(vec![
        map(vec![
            ("precision".into(), string("zip")),
            ("Latitude".into(), decimal("377668", "-4")),
            ("Longitude".into(), decimal("-1223959", "-4")),
            ("Address".into(), string("")),
            ("City".into(), string("SAN FRANCISCO")),
            ("State".into(), string("CA")),
            ("Zip".into(), string("94107")),
            ("Country".into(), string("US")),
        ]),
        map(vec![
            ("precision".into(), string("zip")),
            ("Latitude".into(), decimal("37371991", "-6")),
            ("Longitude".into(), decimal("-122026020", "-6")),
            ("Address".into(), string("")),
            ("City".into(), string("SUNNYVALE")),
            ("State".into(), string("CA")),
            ("Zip".into(), string("94085")),
            ("Country".into(), string("US")),
        ]),
    ])];

    verify_tlvs(expected, result);
}

#[test]
fn test_testfile35() {
    let result = parse_file(&test_path("good/testfile35.ion"));

    let expected = vec![value(
        map_data(vec![
            (
                "whenDate".into(),
                timestamp(TextDate::day(2007, 1, 31).unwrap(), None),
            ),
            (
                "whenDate".into(),
                timestamp(
                    TextDate::day(2007, 1, 31).unwrap(),
                    Some(minute(Some(UtcOffset::UTC), 1, 2)),
                ),
            ),
            (
                "whenDate".into(),
                timestamp(
                    TextDate::day(2007, 1, 31).unwrap(),
                    Some(fractional_second(
                        Some(UtcOffset::UTC),
                        1,
                        4,
                        5,
                        BigUint::from(385u32),
                        -3,
                    )),
                ),
            ),
            (
                "whenDate".into(),
                timestamp(
                    TextDate::day(2007, 1, 31).unwrap(),
                    Some(fractional_second(
                        Some(UtcOffset::east_minutes(60 + 11)),
                        1,
                        4,
                        5,
                        BigUint::from(385u32),
                        -3,
                    )),
                ),
            ),
        ]),
        vec![annot("dates")],
    )];

    verify_tlvs(expected, result);
}

// Thirty-six, our final casualty.

#[test]
fn test_testfile37() {
    let result = parse_file(&test_path("good/testfile37.ion"));

    let expected = vec![sexp(vec![
        decimal("2000000000", "0"),
        decimal("12345678", "-6"),
        decimal("4000000000", "0"),
    ])];

    verify_tlvs(expected, result);
}

#[test]
fn test_whitespace() {
    let result = parse_file(&test_path("good/whitespace.ion"));

    let expected = vec![
        int_i64(1),
        symbol("a"),
        sexp(vec![int_i64(1), symbol("a")]),
        int_i64(1),
        symbol("a"),
        sexp(vec![int_i64(1), symbol("a")]),
        int_i64(1),
        symbol("a"),
        sexp(vec![int_i64(1), symbol("a")]),
    ];

    verify_tlvs(expected, result);
}
