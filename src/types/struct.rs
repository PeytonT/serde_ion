use crate::symbols::SymbolToken;
use crate::types::Value;

// struct - Unordered collections of tagged values
#[derive(Clone, Debug, PartialEq)]
pub struct Struct {
    // When two fields in the same struct have the same name we say they are "repeated fields".
    // Repeated fields are preserved, so 'fields' is a Vec instead of some sort of map.
    pub fields: Vec<(SymbolToken, Value)>,
}

impl Struct {
    pub fn to_text(&self) -> String {
        todo!()
    }
}
