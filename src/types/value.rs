extern crate base64;
extern crate num_bigint;
extern crate serde_bytes;

use std::str;

use num_bigint::BigInt;

use crate::symbols::SymbolToken;
use crate::text::escape;
use crate::types::blob::Blob;
use crate::types::clob::Clob;
use crate::types::decimal::Decimal;
use crate::types::list::List;
use crate::types::r#struct::Struct;
use crate::types::sexp::Sexp;
use crate::types::timestamp::Timestamp;

//////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Debug, PartialEq)]
pub struct Value {
    pub value: Data,
    // The Ion specification notes that in the text format, annotations are denoted by a non-null
    // symbol token. The text and binary formats are semantically isomorphic, so it follows that a
    // null symbol cannot appear as an annotation.
    // Correspondingly this is a Vec<SymbolToken> rather than a Vec<Option<SymbolToken>>.
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

/// Represents an element of the Ion data model.
///
/// Rather than represent each Ion data model types as an enum with a Null variant,
/// nullable types (all types other than Null itself) are represented using an Option
/// in the Data enum, with the null value of each type represented using Option::None.
/// This is intended to align with standard Rust idioms, and allows users to skip
/// null-handling when working with underlying elements that are known to be non-null.
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
                Some(string) => escape(string),
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
