extern crate base64;
extern crate num_bigint;
extern crate serde_bytes;

use std::str;
use std::string::String as StdString;

use crate::symbols::SymbolToken;
use base64::encode;
use num_bigint::BigInt;
use num_bigint::BigUint;

#[derive(Clone, Debug, PartialEq)]
pub struct Value {
    pub value: Data,
    pub annotations: Option<Vec<Symbol>>,
}

#[allow(clippy::large_enum_variant)]
#[derive(Clone, Debug, PartialEq)]
pub enum Data {
    // null - A generic null value
    Null,
    Bool(Bool),
    Int(Int),
    Float(Float),
    Decimal(Decimal),
    Timestamp(Timestamp),
    String(String),
    Symbol(Symbol),
    Blob(Blob),
    Clob(Clob),
    Struct(Struct),
    List(List),
    Sexp(Sexp),
}

// bool - Boolean values
#[derive(Clone, Debug, PartialEq)]
pub enum Bool {
    Null,
    True,
    False,
}

impl Bool {
    pub fn to_text(&self) -> StdString {
        match self {
            Bool::Null => StdString::from("null.bool"),
            Bool::True => StdString::from("true"),
            Bool::False => StdString::from("false"),
        }
    }
}

// int - Signed integers of arbitrary size
#[derive(Clone, Debug, PartialEq)]
pub enum Int {
    Null,
    Integer { value: BigInt },
}

impl Int {
    pub fn to_text(&self) -> StdString {
        match self {
            Int::Null => StdString::from("null.int"),
            Int::Integer { value } => todo!(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum IntegerTextFormat {
    Decimal,
    Hexadecimal,
    Binary,
}

// float - Binary-encoded floating point numbers (IEEE 64-bit)
#[derive(Clone, Debug, PartialEq)]
pub enum Float {
    Null,
    Float { value: f64 },
}

impl Float {
    pub fn to_text(&self) -> StdString {
        match self {
            Float::Null => StdString::from("null.float"),
            Float::Float { value } => todo!(),
        }
    }
}

// decimal - Decimal-encoded real numbers of arbitrary precision
// Reference http://speleotrove.com/decimal/decarith.html
#[derive(Clone, Debug, PartialEq)]
pub enum Decimal {
    Null,
    Decimal {
        coefficient: BigInt,
        exponent: BigInt,
    },
}

impl Decimal {
    pub fn to_text(&self) -> StdString {
        match self {
            Decimal::Null => StdString::from("null.decimal"),
            Decimal::Decimal {
                coefficient,
                exponent,
            } => todo!(),
        }
    }
}

// timestamp - Date/time/timezone moments of arbitrary precision
// Mostly ISO 8601
// Enum variant names represent the precision of the variant
#[allow(clippy::large_enum_variant)]
#[derive(Clone, Debug, PartialEq)]
pub enum Timestamp {
    Null,
    Year {
        offset: BigInt,
        year: BigUint,
    },
    Month {
        offset: BigInt,
        year: BigUint,
        month: BigUint,
    },
    Day {
        offset: BigInt,
        year: BigUint,
        month: BigUint,
        day: BigUint,
    },
    Minute {
        offset: BigInt,
        year: BigUint,
        month: BigUint,
        day: BigUint,
        hour: BigUint,
        minute: BigUint,
    },
    Second {
        offset: BigInt,
        year: BigUint,
        month: BigUint,
        day: BigUint,
        hour: BigUint,
        minute: BigUint,
        second: BigUint,
    },
    FractionalSecond {
        offset: BigInt,
        year: BigUint,
        month: BigUint,
        day: BigUint,
        hour: BigUint,
        minute: BigUint,
        second: BigUint,
        fraction_coefficient: BigUint,
        // The restriction of fractional_exponent to i32 rather than BigInt should not pose an issue for any non-pathological use
        fraction_exponent: i32,
    },
}

impl Timestamp {
    pub fn to_text(&self) -> StdString {
        match self {
            Timestamp::Null => StdString::from("null.timestamp"),
            _ => todo!(),
        }
    }
}

// String - Unicode text literals
#[derive(Clone, Debug, PartialEq)]
pub enum String {
    Null,
    String { value: StdString },
}

impl String {
    pub fn to_text(&self) -> StdString {
        match self {
            String::Null => StdString::from("null.string"),
            String::String { value } => todo!(), // needs to re-expand escape sequences
        }
    }
}

/// ## symbol - Interned, Unicode symbolic atoms (aka identifiers)
///
/// symbol - Interned, Unicode symbolic atoms (aka identifiers)
/// A subset of symbols called identifiers can be denoted in text without single-quotes.
/// An identifier is a sequence of ASCII letters, digits, or the
/// characters $ (dollar sign) or _ (underscore), not starting with a digit.
///
/// Ion symbols may have text that is unknown. That is, there is no binding to a (potentially empty) sequence of text.
/// This can happen as a result of not having access to a shared symbol table being imported,
/// or having a symbol table (shared or local) that contains a null slot.
///
/// A processor encountering a symbol with unknown text and a valid SID other than $0 MAY produce an error
/// because this means that the context of the data is missing.
/// Note: serde_ion does not support symbols other than $0 with unknown text.
#[derive(Clone, Debug, PartialEq)]
pub enum Symbol {
    Null,
    Symbol { token: SymbolToken },
}

impl Symbol {
    pub fn to_text(&self) -> StdString {
        match self {
            Symbol::Null => StdString::from("null.symbol"),
            Symbol::Symbol { token } => match token {
                SymbolToken::Known { text } => text.to_string(),
                SymbolToken::Unknown { .. } => todo!(), // should error, but how?
                SymbolToken::Zero => "$0".to_string(),
            },
        }
    }
}

// clob - Text data of user-defined encoding
// In the text format, clob values use similar syntax to blob,
// but the data between braces must be one StdString.
// The StdString may only contain legal 7-bit ASCII characters, using the same escaping syntax as
// StdString and symbol values. This guarantees that the value can be transmitted unscathed while
// remaining generally readable (at least for western language text).
// Like blobs, clobs disallow comments everywhere within the value.
#[derive(Clone, Debug, PartialEq)]
pub enum Clob {
    Null,
    Clob { data: Vec<u8> },
}

impl Clob {
    pub fn to_text(&self) -> StdString {
        match self {
            Clob::Null => StdString::from("null.clob"),
            // from_utf8's constraints might not be sufficiently strong
            Clob::Clob { data } => format!("{{{{{}}}}}", str::from_utf8(data).unwrap()),
        }
    }
}

// blob - Binary data of user-defined encoding
#[derive(Clone, Debug, PartialEq)]
pub enum Blob {
    Null,
    Blob { data: Vec<u8> },
}

impl Blob {
    pub fn to_text(&self) -> StdString {
        match self {
            Blob::Null => StdString::from("null.blob"),
            Blob::Blob { data } => format!("{{{{{}}}}}", encode(data)),
        }
    }
}

// struct - Unordered collections of tagged values
#[derive(Clone, Debug, PartialEq)]
pub enum Struct {
    Null,
    Struct { values: Vec<(SymbolToken, Value)> },
}

impl Struct {
    pub fn to_text(&self) -> StdString {
        match self {
            Struct::Null => StdString::from("null.struct"),
            Struct::Struct { values: entries } => todo!(),
        }
    }
}

// list - Ordered collections of values
#[derive(Clone, Debug, PartialEq)]
pub enum List {
    Null,
    List { values: Vec<Value> },
}

impl List {
    pub fn to_text(&self) -> StdString {
        match self {
            List::Null => StdString::from("null.list"),
            List::List { values } => todo!(),
        }
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
pub enum Sexp {
    Null,
    Sexp { values: Vec<Value> },
}

impl Sexp {
    pub fn to_text(&self) -> StdString {
        match self {
            Sexp::Null => StdString::from("null.sexp"),
            Sexp::Sexp { values } => todo!(),
        }
    }
}
