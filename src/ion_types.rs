extern crate base64;
extern crate chrono;
extern crate num_bigint;
extern crate serde_bytes;

use std::option::Option;
use std::str;
use std::string::String;

use base64::encode;
use chrono::{FixedOffset, NaiveDateTime};
use num_bigint::BigInt;
use num_bigint::BigUint;

/**
No promises of round-trip for optional representation details.
There are too many and they are too absurd.
Reference http://amzn.github.io/ion-docs/docs/spec.html
*/
#[derive(Clone, Debug, PartialEq)]
pub enum IonValue {
    IonNull(IonNull),
    IonBoolean(IonBoolean),
    IonInteger(IonInteger),
    IonFloat(IonFloat),
    IonDecimal(IonDecimal),
    IonTimestamp(IonTimestamp),
    IonString(IonString),
    IonSymbol(IonSymbol),
    IonBlob(IonBlob),
    IonClob(IonClob),
    IonStructure(IonStructure),
    IonList(IonList),
    IonSymbolicExpression(IonSymbolicExpression),
}

impl IonValue {
    pub fn to_text(&self) -> String {
        match self {
            IonValue::IonNull(val) => val.to_text(),
            IonValue::IonBoolean(val) => val.to_text(),
            IonValue::IonInteger(val) => val.to_text(),
            IonValue::IonFloat(val) => val.to_text(),
            IonValue::IonDecimal(val) => val.to_text(),
            IonValue::IonTimestamp(val) => val.to_text(),
            IonValue::IonString(val) => val.to_text(),
            IonValue::IonSymbol(val) => val.to_text(),
            IonValue::IonBlob(val) => val.to_text(),
            IonValue::IonClob(val) => val.to_text(),
            IonValue::IonStructure(val) => val.to_text(),
            IonValue::IonList(val) => val.to_text(),
            IonValue::IonSymbolicExpression(val) => val.to_text(),
        }
    }
}

// null - A generic null value
#[derive(Clone, Debug, PartialEq)]
pub enum IonNull {
    Null,
    Pad,
}

impl IonNull {
    pub fn to_text(&self) -> String {
        match self {
            IonNull::Null => String::from("null.null"),
            IonNull::Pad => unimplemented!(), // TODO(peyton): What error should go here?
        }
    }
}

// bool - Boolean values
#[derive(Clone, Debug, PartialEq)]
pub enum IonBoolean {
    Null,
    True,
    False,
}

impl IonBoolean {
    pub fn to_text(&self) -> String {
        match self {
            IonBoolean::Null => String::from("null.bool"),
            IonBoolean::True => String::from("true"),
            IonBoolean::False => String::from("false"),
        }
    }
}

// int - Signed integers of arbitrary size
#[derive(Clone, Debug, PartialEq)]
pub enum IonInteger {
    Null,
    Integer { value: BigInt },
}

impl IonInteger {
    pub fn to_text(&self) -> String {
        match self {
            IonInteger::Null => String::from("null.int"),
            IonInteger::Integer { value } => unimplemented!(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum IonIntegerTextFormat {
    Decimal,
    Hexadecimal,
    Binary,
}

// float - Binary-encoded floating point numbers (IEEE 64-bit)
#[derive(Clone, Debug, PartialEq)]
pub enum IonFloat {
    Null,
    Float { value: f64 },
}

impl IonFloat {
    pub fn to_text(&self) -> String {
        match self {
            IonFloat::Null => String::from("null.float"),
            IonFloat::Float { value } => unimplemented!(),
        }
    }
}

// decimal - Decimal-encoded real numbers of arbitrary precision
// Reference http://speleotrove.com/decimal/decarith.html
#[derive(Clone, Debug, PartialEq)]
pub enum IonDecimal {
    Null,
    Decimal {
        coefficient: BigInt,
        exponent: BigInt,
    },
}

impl IonDecimal {
    pub fn to_text(&self) -> String {
        match self {
            IonDecimal::Null => String::from("null.decimal"),
            IonDecimal::Decimal {
                coefficient,
                exponent,
            } => unimplemented!(),
        }
    }
}

// timestamp - Date/time/timezone moments of arbitrary precision
// Mostly ISO 8601
// Enum variant names represent the precision of the variant
#[derive(Clone, Debug, PartialEq)]
pub enum IonTimestamp {
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
        // The restriction of fractional_exponent to i32 rather than BigInt should not pose an issue for any non-pathological use
        fraction_coefficient: BigUint,
        fraction_exponent: i32,
    },
}

impl IonTimestamp {
    pub fn to_text(&self) -> String {
        match self {
            IonTimestamp::Null => String::from("null.timestamp"),
            _ => unimplemented!(),
        }
    }
}

// string - Unicode text literals
#[derive(Clone, Debug, PartialEq)]
pub enum IonString {
    Null,
    String(String),
}

impl IonString {
    pub fn to_text(&self) -> String {
        match self {
            IonString::Null => String::from("null.string"),
            IonString::String(val) => unimplemented!(), // needs to re-expand escape sequences
        }
    }
}

// symbol - Interned, Unicode symbolic atoms (aka identifiers)
// A subset of symbols called identifiers can be denoted in text without single-quotes.
// An identifier is a sequence of ASCII letters, digits, or the
// characters $ (dollar sign) or _ (underscore), not starting with a digit.
#[derive(Clone, Debug, PartialEq)]
pub enum IonSymbol {
    Null,
    Symbol(String),
}

impl IonSymbol {
    pub fn to_text(&self) -> String {
        match self {
            IonSymbol::Null => String::from("null.symbol"),
            IonSymbol::Symbol(val) => unimplemented!(), // needs to re-expand escape sequences
        }
    }
}

// blob - Binary data of user-defined encoding
#[derive(Clone, Debug, PartialEq)]
pub enum IonBlob {
    Null,
    Blob(Vec<u8>),
}

impl IonBlob {
    pub fn to_text(&self) -> String {
        match self {
            IonBlob::Null => String::from("null.blob"),
            IonBlob::Blob(data) => format!("{{{{{}}}}}", encode(data)),
        }
    }
}

// clob - Text data of user-defined encoding
// In the text format, clob values use similar syntax to blob,
// but the data between braces must be one string.
// The string may only contain legal 7-bit ASCII characters, using the same escaping syntax as
// string and symbol values. This guarantees that the value can be transmitted unscathed while
// remaining generally readable (at least for western language text).
// Like blobs, clobs disallow comments everywhere within the value.
#[derive(Clone, Debug, PartialEq)]
pub enum IonClob {
    Null,
    Clob(Vec<u8>),
}

impl IonClob {
    pub fn to_text(&self) -> String {
        match self {
            IonClob::Null => String::from("null.clob"),
            // from_utf8's constraints might not be sufficiently strong
            IonClob::Clob(data) => format!("{{{{{}}}}}", str::from_utf8(data).unwrap()),
        }
    }
}

// struct - Unordered collections of tagged values
#[derive(Clone, Debug, PartialEq)]
pub enum IonStructure {
    Null,
    Structure(Vec<(IonSymbol, IonValue)>),
}

impl IonStructure {
    pub fn to_text(&self) -> String {
        match self {
            IonStructure::Null => String::from("null.struct"),
            IonStructure::Structure(entries) => unimplemented!(),
        }
    }
}

// list - Ordered collections of values
#[derive(Clone, Debug, PartialEq)]
pub enum IonList {
    Null,
    List(Vec<(IonValue)>),
}

impl IonList {
    pub fn to_text(&self) -> String {
        match self {
            IonList::Null => String::from("null.list"),
            IonList::List(val) => format!(
                "[{}]",
                val.iter()
                    .map(|x| x.to_text())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
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
pub enum IonSymbolicExpression {
    Null,
    SymbolicExpression(Vec<(SymbolicExpressionSymbol)>),
}

impl IonSymbolicExpression {
    pub fn to_text(&self) -> String {
        match self {
            IonSymbolicExpression::Null => String::from("null.sexp"),
            IonSymbolicExpression::SymbolicExpression(sexp) => format!(
                "({})",
                sexp.iter()
                    .map(|x| x.to_text())
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum SymbolicExpressionSymbol {
    SexpSymbol(IonSymbol),
    SexpOperator(IonOperator),
}

impl SymbolicExpressionSymbol {
    pub fn to_text(&self) -> String {
        match self {
            SymbolicExpressionSymbol::SexpSymbol(val) => val.to_text(),
            SymbolicExpressionSymbol::SexpOperator(val) => val.seq_op_chars.clone(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct IonOperator {
    seq_op_chars: String,
}
