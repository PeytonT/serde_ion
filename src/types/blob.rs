use base64::encode;

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
