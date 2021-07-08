use std::str;

use crate::text::escape;

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
        // TODO: from_utf8 might not be sufficient?
        // Clob uses the same escaping syntax as string and symbol values.
        // TODO: Apparently not quite.
        //  https://github.com/amzn/ion-docs/issues/130
        //  https://amzn.github.io/ion-docs/docs/stringclob.html#ion-clob
        format!("{{{{{}}}}}", escape(str::from_utf8(&self.data).unwrap()))
    }
}
