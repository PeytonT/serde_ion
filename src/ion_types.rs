extern crate base64;
extern crate num_bigint;
extern crate serde_bytes;

use std::str;
use std::string::String;

use base64::encode;
use num_bigint::BigInt;
use num_bigint::BigUint;

/** Reference http://amzn.github.io/ion-docs/docs/spec.html */
#[derive(Clone, Debug, PartialEq)]
pub struct IonValue {
    pub content: IonData,
    pub annotations: Option<Vec<IonSymbol>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum IonData {
    Null(IonNull),
    Bool(IonBool),
    Int(IonInt),
    Float(IonFloat),
    Decimal(IonDecimal),
    Timestamp(IonTimestamp),
    String(IonString),
    Symbol(IonSymbol),
    Blob(IonBlob),
    Clob(IonClob),
    Struct(IonStruct),
    List(IonList),
    Sexp(IonSexp),
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
pub enum IonBool {
    Null,
    True,
    False,
}

impl IonBool {
    pub fn to_text(&self) -> String {
        match self {
            IonBool::Null => String::from("null.bool"),
            IonBool::True => String::from("true"),
            IonBool::False => String::from("false"),
        }
    }
}

// int - Signed integers of arbitrary size
#[derive(Clone, Debug, PartialEq)]
pub enum IonInt {
    Null,
    Integer { value: BigInt },
}

impl IonInt {
    pub fn to_text(&self) -> String {
        match self {
            IonInt::Null => String::from("null.int"),
            IonInt::Integer { value } => unimplemented!(),
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
    String { value: String },
}

impl IonString {
    pub fn to_text(&self) -> String {
        match self {
            IonString::Null => String::from("null.string"),
            IonString::String { value } => unimplemented!(), // needs to re-expand escape sequences
        }
    }
}

/// ## symbol - Interned, Unicode symbolic atoms (aka identifiers)
///
/// ```text
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
/// ```
#[derive(Clone, Debug, PartialEq)]
pub enum IonSymbol {
    Null,
    // SID zero (i.e. $0) is a special symbol that is not assigned text by any symbol table.
    // $0 is only semantically equivalent to itself and to locally-declared SIDs with unknown text.
    SidZero,
    Symbol { text: String },
}

impl IonSymbol {
    pub fn to_text(&self) -> String {
        match self {
            IonSymbol::Null => String::from("null.symbol"),
            IonSymbol::SidZero => String::from("$0"),
            IonSymbol::Symbol { text } => unimplemented!(), // needs to re-expand escape sequences
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
    Clob { data: Vec<u8> },
}

impl IonClob {
    pub fn to_text(&self) -> String {
        match self {
            IonClob::Null => String::from("null.clob"),
            // from_utf8's constraints might not be sufficiently strong
            IonClob::Clob { data } => format!("{{{{{}}}}}", str::from_utf8(data).unwrap()),
        }
    }
}

// blob - Binary data of user-defined encoding
#[derive(Clone, Debug, PartialEq)]
pub enum IonBlob {
    Null,
    Blob { data: Vec<u8> },
}

impl IonBlob {
    pub fn to_text(&self) -> String {
        match self {
            IonBlob::Null => String::from("null.blob"),
            IonBlob::Blob { data } => format!("{{{{{}}}}}", encode(data)),
        }
    }
}

// struct - Unordered collections of tagged values
#[derive(Clone, Debug, PartialEq)]
pub enum IonStruct {
    Null,
    Structure(Vec<(IonSymbol, IonValue)>),
}

impl IonStruct {
    pub fn to_text(&self) -> String {
        match self {
            IonStruct::Null => String::from("null.struct"),
            IonStruct::Structure(entries) => unimplemented!(),
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
            IonList::List(val) => unimplemented!(),
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
pub enum IonSexp {
    Null,
    SymbolicExpression(Vec<(SymbolicExpressionSymbol)>),
}

impl IonSexp {
    pub fn to_text(&self) -> String {
        match self {
            IonSexp::Null => String::from("null.sexp"),
            IonSexp::SymbolicExpression(sexp) => format!(
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
