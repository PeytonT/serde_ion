use crate::types::Value;

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
