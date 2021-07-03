mod bad;
mod equivalencies;
mod good;

use super::*;
use crate::parse::parse_ion_text_1_0;
use itertools::{EitherOrBoth, Itertools};
use log::error;
use std::{
    convert::TryInto,
    ffi::OsStr,
    fs::{self},
    io,
    path::{Path, PathBuf},
    str::FromStr,
};

/// This file includes some machinery for handling the variety of tests we put the text parser
/// through. Namely, it abstracts away the test file location as well as the comparison between
/// expected and actual values. Following is an example of how a simple test might look:
///
/// #[test]
/// fn test_example() {
///     let result = parse_file(&test_path("good/<someTest>.ion"));
///
///     let expected = vec![];
///
///     verify_tlvs(expected, result);
/// }

const TEST_ROOT: &str = "tests/ion-tests/iontestdata/";

fn test_path(test: &str) -> PathBuf {
    Path::new(TEST_ROOT).join(test)
}

fn parse_file(file: &Path) -> Result<Vec<ion::Value>, String> {
    match fs::read_to_string(file) {
        Ok(data) => match parse_ion_text_1_0(&data) {
            Ok((_, v)) => Ok(v),
            Err(err) => Err(format!("{:?}", err)),
        },
        Err(e) => Err(e.to_string()),
    }
}

/// Recursively locates .ion test files in the provided directory
fn find_ion_text(dir: &Path) -> io::Result<Vec<PathBuf>> {
    let mut tests = vec![];

    if dir.is_dir() {
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_dir() {
                tests.append(&mut find_ion_text(&path)?);
            } else if path.extension() == Some(OsStr::new("ion")) {
                tests.push(path);
            }
        }
    }
    Ok(tests)
}

/// Verifies a list of expected values against a list of actual parsed top level values.
fn verify_tlvs(expected: Vec<ion::Value>, actuals: Result<Vec<ion::Value>, String>) {
    if let Err(e) = actuals {
        pretty_env_logger::try_init().ok();
        panic!("test failed: {}", e)
    }

    for (count, result) in expected
        .into_iter()
        .zip_longest(actuals.unwrap().into_iter())
        .enumerate()
    {
        match result {
            EitherOrBoth::Both(expected, actual) => {
                if expected != actual {
                    pretty_env_logger::try_init().ok();
                    error!("Failed on top level value {}:", count + 1);
                    error!("Expected:",);
                    error!("{:?}", expected);
                    error!("Actual:",);
                    error!("{:?}", actual);
                    panic!("expected/actual differ at {}", count + 1);
                }
            }
            EitherOrBoth::Left(_) | EitherOrBoth::Right(_) => {
                panic!("expected/actuals lists differ in length (short one ends at {}), all good until here", count + 1)
            }
        }
    }
}

// A set of helpers to remove boiler plate for the massive number of tests below.
fn value(value: ion::Data, annotations: Vec<SymbolToken>) -> ion::Value {
    ion::Value { value, annotations }
}

fn boolean(b: bool) -> ion::Value {
    ion::Data::Bool(Some(b)).into()
}

fn minute(offset: Option<UtcOffset>, hour: u8, minute: u8) -> TextTime {
    TextTime::Minute {
        offset,
        hour,
        minute,
    }
}

fn fractional_second(
    offset: Option<UtcOffset>,
    hour: u8,
    minute: u8,
    second: u8,
    fraction_coefficient: BigUint,
    fraction_exponent: i32,
) -> TextTime {
    TextTime::FractionalSecond {
        offset,
        hour,
        minute,
        second,
        fraction_coefficient,
        fraction_exponent,
    }
}

fn timestamp(date: TextDate, time: Option<TextTime>) -> ion::Value {
    timestamp_data(date, time).into()
}

fn timestamp_data(date: TextDate, time: Option<TextTime>) -> ion::Data {
    ion::Data::Timestamp(Some(TextTimestamp::new(date, time).try_into().unwrap()))
}

fn decimal(coefficient: &str, exponent: &str) -> ion::Value {
    let coefficient = BigInt::from_str(coefficient).unwrap();
    let exponent = BigInt::from_str(exponent).unwrap();
    ion::Data::Decimal(Some(ion::Decimal {
        coefficient,
        exponent,
    }))
    .into()
}

fn float(s: &str) -> ion::Value {
    ion::Data::Float(Some(lexical_core::parse(s.as_bytes()).unwrap())).into()
}

fn int_i64_data(i: i64) -> ion::Data {
    let int = BigInt::from(i);
    ion::Data::Int(Some(int))
}

fn int_i64(i: i64) -> ion::Value {
    int_i64_data(i).into()
}

fn int_s(s: &str, radix: u32) -> ion::Value {
    let int = BigInt::from_str_radix(s, radix).unwrap();
    ion::Data::Int(Some(int)).into()
}

fn string(s: &str) -> ion::Value {
    ion::Data::String(Some(s.to_string())).into()
}

fn annot(s: &str) -> SymbolToken {
    let text = s.to_string();
    SymbolToken::Known { text }
}

fn symbol(s: &str) -> ion::Value {
    symbol_data(s).into()
}

fn symbol_data(s: &str) -> ion::Data {
    let text = s.to_owned();
    ion::Data::Symbol(Some(SymbolToken::Known { text }))
}

fn clob(d: &[u8]) -> ion::Value {
    clob_data(d).into()
}

fn clob_data(d: &[u8]) -> ion::Data {
    let data = d.to_vec();
    ion::Data::Clob(Some(ion::Clob { data }))
}

fn blob_decoded(d: &[u8]) -> ion::Value {
    let data = d.to_vec();
    ion::Data::Blob(Some(ion::Blob { data })).into()
}

fn blob_encoded(d: &[u8]) -> ion::Value {
    blob_encoded_data(d).into()
}

fn blob_encoded_data(d: &[u8]) -> ion::Data {
    let data = base64::decode(d).unwrap();
    ion::Data::Blob(Some(ion::Blob { data }))
}

fn sexp_data(values: Vec<ion::Value>) -> ion::Data {
    ion::Data::Sexp(Some(ion::Sexp { values }))
}

fn sexp(values: Vec<ion::Value>) -> ion::Value {
    sexp_data(values).into()
}

fn list(values: Vec<ion::Value>) -> ion::Value {
    ion::Data::List(Some(ion::List { values })).into()
}

fn map_data(fields: Vec<(SymbolToken, ion::Value)>) -> ion::Data {
    ion::Data::Struct(Some(ion::Struct { fields }))
}

fn map(fields: Vec<(SymbolToken, ion::Value)>) -> ion::Value {
    map_data(fields).into()
}
