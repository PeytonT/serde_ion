use thiserror::Error;

#[derive(Error, Debug, PartialEq)]
pub enum SymbolError {
    #[error("invalid symbol_id {symbol_id:?} is greater than local max_id {max_id:?})")]
    AboveMaxId { max_id: usize, symbol_id: usize },
    #[error("invalid symbol_id {symbol_id:?} is less than local min_local_id {min_local_id:?})")]
    BelowMinId {
        min_local_id: usize,
        symbol_id: usize,
    },
    #[error("the text for SID `{0}` is unknown")]
    UnknownSymbolText(usize),
    #[error("the text for SID `{0}` is undefined")]
    UndefinedSymbolText(usize),
}

#[derive(Error, Debug, PartialEq)]
pub enum FormatError {
    #[error("format error in binary data")]
    Binary(BinaryFormatError),
    #[error("format error in text data")]
    Text(TextFormatError),
}

#[derive(Error, Debug, PartialEq)]
pub enum BinaryFormatError {
    #[error("the type code 15 is reserved")]
    ReservedTypeCode,
    #[error("it is illegal for an annotation to wrap another annotation atomically")]
    AnnotatedAnnotation,
    #[error("it is illegal for an annotation to wrap a no-op pad since they are not Ion values")]
    AnnotatedPadding,
    #[error("annotation length code of `{0}` is not allowed")]
    AnnotationLength(u8),
    #[error("int 0 is stored with type code T == 2 and L == 0, other encodings are invalid.")]
    NegativeZero,
    #[error("bool value `{0}` is not allowed")]
    BoolValue(u8),
    #[error("float length code of `{0}` is not allowed")]
    FloatLength(u8),
    #[error("timestamp length code of `{0}` is not allowed")]
    TimestampLength(u8),
    #[error("string encoding must be utf8")]
    StringEncoding,
    #[error("structs with length_code L1 cannot be empty")]
    StructEmpty,
    #[error("structs with length_code L1 must have increasing field-name integers")]
    StructUnordered,
    #[error("invalid local symbol table")]
    LocalTable,
}

#[derive(Error, Debug, PartialEq)]
pub enum TextFormatError {
    #[error("TODO")]
    TODO,
}
