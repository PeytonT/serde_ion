extern crate base64;
extern crate num_bigint;
extern crate serde_bytes;

use std::str;

use crate::symbols::SymbolToken;
use base64::encode;
use num_bigint::{BigInt, BigUint};

//////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Debug, PartialEq)]
pub struct Value {
    pub value: Data,
    // The Ion specification notes that in the text format, annotations are denoted by a non-null
    // symbol token. The text and binary formats are semantically isomorphic, so it follows that a
    // null symbol cannot appear as an annotation, and correspondingly this is a Vec<SymbolToken>
    // rather than a Vec<Option<SymbolToken>>.
    pub annotations: Vec<SymbolToken>,
}

impl Value {
    pub(crate) fn has_annotation(&self, annotation: &str) -> bool {
        for token in &self.annotations {
            match token {
                SymbolToken::Known { text } if text.as_str() == annotation => return true,
                _ => (),
            }
        }
        false
    }
}

impl From<Data> for Value {
    fn from(value: Data) -> Self {
        Self {
            value,
            annotations: vec![],
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Data {
    // null - A generic null value
    Null,
    // bool - Boolean values
    Bool(Option<bool>),
    // int - Signed integers of arbitrary size
    Int(Option<BigInt>),
    // float - Binary-encoded floating point numbers (IEEE 64-bit)
    Float(Option<f64>),
    // decimal - Decimal-encoded real numbers of arbitrary precision
    Decimal(Option<Decimal>),
    // timestamp - Date/time/timezone moments of arbitrary precision
    Timestamp(Option<Timestamp>),
    // String - Unicode text literals
    String(Option<String>),
    // symbol - Interned, Unicode symbolic atoms (aka identifiers)
    Symbol(Option<SymbolToken>),
    // blob - Binary data of user-defined encoding
    Blob(Option<Blob>),
    // clob - Text data of user-defined encoding
    Clob(Option<Clob>),
    // struct - Unordered collections of tagged values
    Struct(Option<Struct>),
    // list - Ordered collections of values
    List(Option<List>),
    // sexp - Ordered collections of values with application-defined semantics
    Sexp(Option<Sexp>),
}

macro_rules! ion_type_promotions {
    ($ion_type:ty, $data_variant:expr) => {
        impl From<$ion_type> for Data {
            fn from(ion_value: $ion_type) -> Self {
                $data_variant(Some(ion_value))
            }
        }

        impl From<$ion_type> for Value {
            fn from(ion_value: $ion_type) -> Self {
                $data_variant(Some(ion_value)).into()
            }
        }
    };
}

ion_type_promotions!(bool, Data::Bool);
ion_type_promotions!(BigInt, Data::Int);
ion_type_promotions!(f64, Data::Float);
ion_type_promotions!(Decimal, Data::Decimal);
ion_type_promotions!(Timestamp, Data::Timestamp);
ion_type_promotions!(String, Data::String);
ion_type_promotions!(SymbolToken, Data::Symbol);
ion_type_promotions!(Blob, Data::Blob);
ion_type_promotions!(Clob, Data::Clob);
ion_type_promotions!(Struct, Data::Struct);
ion_type_promotions!(List, Data::List);
ion_type_promotions!(Sexp, Data::Sexp);

impl Data {
    pub fn to_text(&self) -> String {
        match self {
            Data::Null => String::from("null.null"),
            Data::Bool(bool) => match bool {
                Some(true) => String::from("true"),
                Some(false) => String::from("false"),
                None => String::from("null.bool"),
            },
            Data::Int(int) => match int {
                Some(int) => int.to_str_radix(10),
                None => String::from("null.int"),
            },
            Data::Float(float) => match float {
                Some(float) => float.to_string(),
                None => String::from("null.float"),
            },
            Data::Decimal(decimal) => match decimal {
                Some(decimal) => decimal.to_text(),
                None => String::from("null.decimal"),
            },
            Data::Timestamp(timestamp) => match timestamp {
                Some(timestamp) => timestamp.to_text(),
                None => String::from("null.timestamp"),
            },
            Data::String(string) => match string {
                // TODO: Handle escape sequences
                Some(string) => String::from(string),
                None => String::from("null.string"),
            },
            Data::Symbol(symbol) => match symbol {
                None => String::from("null.symbol"),
                Some(symbol) => symbol.to_text(),
            },
            Data::Blob(blob) => match blob {
                None => String::from("null.blob"),
                Some(blob) => blob.to_text(),
            },
            Data::Clob(clob) => match clob {
                None => String::from("null.clob"),
                Some(clob) => clob.to_text(),
            },
            Data::Struct(_struct) => match _struct {
                None => String::from("null.struct"),
                Some(_struct) => _struct.to_text(),
            },
            Data::List(list) => match list {
                None => String::from("null.list"),
                Some(list) => list.to_text(),
            },
            Data::Sexp(sexp) => match sexp {
                None => String::from("null.sexp"),
                Some(sexp) => sexp.to_text(),
            },
        }
    }
}

// decimal - Decimal-encoded real numbers of arbitrary precision
// Reference http://speleotrove.com/decimal/decarith.html
#[derive(Clone, Debug, PartialEq)]
pub struct Decimal {
    pub coefficient: BigInt,
    pub exponent: BigInt,
}

impl Decimal {
    pub fn to_text(&self) -> String {
        todo!()
    }
}

// timestamp - Date/time/timezone moments of arbitrary precision
// Mostly ISO 8601
// Enum variant names represent the precision of the variant
//
// In the text format, timestamps follow the W3C note on date and time formats
// (https://www.w3.org/TR/NOTE-datetime), but they must end with the literal “T” if not at least
// whole-day precision. Fractional seconds are allowed, with at least one digit of precision and an
// unlimited maximum. Local-time offsets may be represented as either hour:minute offsets from UTC,
// or as the literal “Z” to denote a local time of UTC.
//
// Ion follows the “Unknown Local Offset Convention” of RFC3339:
// > If the time in UTC is known, but the offset to local time is unknown, this can be
// > represented with an offset of “-00:00”. This differs semantically from an offset of “Z” or
// > “+00:00”, which imply that UTC is the preferred reference point for the specified time.
//
// Local-time offsets are required on timestamps with time and are not allowed on date values.
// Values that are precise only to the year, month, or date are assumed to be UTC values with
// unknown local offset. As a result, Timestamp variants with this level of precision do not need
// to store an offset field.
//
// Zero and negative dates are not valid, so the earliest instant in time that can be
// represented as a timestamp is Jan 01, 0001. As per the W3C note, leap seconds cannot be
// represented.
//
// ---
//
// The formats are as follows (merging https://www.w3.org/TR/NOTE-datetime and Ion-specific requirements).
//
//    Year:
//       YYYYT (eg 1997T)
//    Year and month:
//       YYYY-MMT (eg 1997-07T)
//    Complete date:
//       YYYY-MM-DD(T) (eg 1997-07-16(T)) (the T is optional)
//    Complete date plus hours and minutes:
//       YYYY-MM-DDThh:mmTZD (eg 1997-07-16T19:20+01:00)
//    Complete date plus hours, minutes and seconds:
//       YYYY-MM-DDThh:mm:ssTZD (eg 1997-07-16T19:20:30+01:00)
//    Complete date plus hours, minutes, seconds and a decimal fraction of a second
//       YYYY-MM-DDThh:mm:ss.sTZD (eg 1997-07-16T19:20:30.45+01:00)
//
// where:
//
//      YYYY = four-digit year
//      MM   = two-digit month (01=January, etc.)
//      DD   = two-digit day of month (01 through 31)
//      hh   = two digits of hour (00 through 23) (am/pm NOT allowed)
//      mm   = two digits of minute (00 through 59)
//      ss   = two digits of second (00 through 59)
//      s    = one or more digits representing a decimal fraction of a second
//      TZD  = time zone designator (Z or +hh:mm or -hh:mm)
// TODO: Look into enforcing validity.
//  https://github.com/amzn/ion-docs/issues/91
//  https://github.com/amzn/ion-docs/issues/151
#[derive(Clone, Debug, PartialEq)]
pub enum Timestamp {
    Year {
        year: u16,
    },
    Month {
        year: u16,
        month: u8,
    },
    Day {
        year: u16,
        month: u8,
        day: u8,
    },
    Minute {
        // Minutes difference from UTC. Option::None indicates an unknown local offset.
        offset: Option<i16>,
        year: u16,
        month: u8,
        day: u8,
        hour: u8,
        minute: u8,
    },
    Second {
        // Minutes difference from UTC. Option::None indicates an unknown local offset.
        offset: Option<i16>,
        year: u16,
        month: u8,
        day: u8,
        hour: u8,
        minute: u8,
        second: u8,
    },
    FractionalSecond {
        // Minutes difference from UTC. Option::None indicates an unknown local offset.
        offset: Option<i16>,
        year: u16,
        month: u8,
        day: u8,
        hour: u8,
        minute: u8,
        second: u8,
        fraction_coefficient: BigUint,
        // The restriction of fractional_exponent to i32 rather than BigInt should not pose an issue for any non-pathological use
        // TODO: Revisit this - absolute correctness to the spec is a compelling virtue.
        // TODO: It seems like this could be unsigned, since per the spec it should never need to be positive.
        fraction_exponent: i32,
    },
}

impl Timestamp {
    pub fn to_text(&self) -> String {
        todo!()
    }
}

// blob - Binary data of user-defined encoding
#[derive(Clone, Debug, PartialEq)]
pub struct Blob {
    pub data: Vec<u8>,
}

impl Blob {
    pub fn to_text(&self) -> String {
        format!("{{{{{}}}}}", encode(&self.data))
    }
}

// clob - Text data of user-defined encoding
// In the text format, clob values use similar syntax to blob,
// but the data between braces must be one String.
// The String may only contain legal 7-bit ASCII characters, using the same escaping syntax as
// String and symbol values. This guarantees that the value can be transmitted unscathed while
// remaining generally readable (at least for western language text).
// Like blobs, clobs disallow comments everywhere within the value.
#[derive(Clone, Debug, PartialEq)]
pub struct Clob {
    pub data: Vec<u8>,
}

impl Clob {
    pub fn to_text(&self) -> String {
        // from_utf8's constraints might not be sufficiently strong
        // needs to re-expand escape sequences
        format!("{{{{{}}}}}", str::from_utf8(&self.data).unwrap())
    }
}

// struct - Unordered collections of tagged values
#[derive(Clone, Debug, PartialEq)]
pub struct Struct {
    // When two fields in the same struct have the same name we say they are "repeated fields".
    // Repeated fields are preserved, so 'fields' is a Vec instead of some sort of Hash table.
    pub fields: Vec<(SymbolToken, Value)>,
}

impl Struct {
    pub fn to_text(&self) -> String {
        todo!()
    }
}

// list - Ordered collections of values
#[derive(Clone, Debug, PartialEq)]
pub struct List {
    pub values: Vec<Value>,
}

impl List {
    pub fn to_text(&self) -> String {
        todo!()
    }
}

impl IntoIterator for List {
    type Item = Value;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.values.into_iter()
    }
}

// sexp - Ordered collections of values with application-defined semantics
// A subset of symbols called identifiers can be denoted in text without single-quotes.
// An identifier is a sequence of ASCII letters, digits, or the
// characters $ (dollar sign) or _ (underscore), not starting with a digit.
// Within S-expressions, the rules for unquoted symbols include another set of tokens: operators.
// An operator is an unquoted sequence of one or more of the following
// nineteen ASCII characters: !#%&*+-./;<=>?@^`|~
// Operators and identifiers can be juxtaposed without whitespace.
#[derive(Clone, Debug, PartialEq)]
pub struct Sexp {
    pub values: Vec<Value>,
}

impl Sexp {
    pub fn to_text(&self) -> String {
        todo!()
    }
}

impl IntoIterator for Sexp {
    type Item = Value;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.values.into_iter()
    }
}
