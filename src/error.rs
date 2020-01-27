use nom::{error::ParseError, Err};
use thiserror::Error;

/// Analogous to nom's IResult.
pub type IonResult<I, T> = Result<(I, T), Err<IonError<I>>>;

#[derive(Debug, PartialEq)]
pub struct IonError<I> {
    pub kind: ErrorKind<I>,
    backtrace: Vec<ErrorKind<I>>,
}

#[derive(Debug, PartialEq)]
pub enum ErrorKind<I> {
    Nom(I, nom::error::ErrorKind),
    // your error types as the rest of the variants
    Symbol(I, SymbolError),
    Format(I, FormatError),
}

impl<I> IonError<I> {
    pub(crate) fn from_symbol_error(input: I, error: SymbolError) -> Self {
        Self {
            kind: ErrorKind::Symbol(input, error),
            backtrace: Vec::new(),
        }
    }

    pub(crate) fn from_format_error(input: I, error: FormatError) -> Self {
        Self {
            kind: ErrorKind::Format(input, error),
            backtrace: Vec::new(),
        }
    }
}

impl<I> ParseError<I> for IonError<I> {
    fn from_error_kind(input: I, kind: nom::error::ErrorKind) -> Self {
        Self {
            kind: ErrorKind::Nom(input, kind),
            backtrace: Vec::new(),
        }
    }

    fn append(input: I, kind: nom::error::ErrorKind, other: Self) -> Self {
        Self::from_error_kind(input, nom::error::ErrorKind::Char)
    }

    fn from_char(input: I, _: char) -> Self {
        Self::from_error_kind(input, nom::error::ErrorKind::Char)
    }

    fn or(self, other: Self) -> Self {
        other
    }

    fn add_context(_input: I, _ctx: &'static str, other: Self) -> Self {
        other
    }
}

impl<I> From<(I, nom::error::ErrorKind)> for IonError<I> {
    fn from(err: (I, nom::error::ErrorKind)) -> Self {
        Self::from_error_kind(err.0, err.1)
    }
}

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
    EncodingOfZero,
    #[error("int 0 is stored with type code T == 2, type code 3 with length 0 is invalid")]
    NegativeZero,
    #[error("bool value `{0}` is not allowed")]
    BoolValue(u8),
    #[error("float length code of `{0}` is not allowed")]
    FloatLength(u8),
    #[error("timestamp length code of `{0}` is not allowed")]
    TimestampLength(u8),
    #[error("string encoding must be utf8")]
    StringEncoding,
    #[error("invalid local symbol table")]
    LocalTable,
}

#[derive(Error, Debug, PartialEq)]
pub enum TextFormatError {
    #[error("TODO")]
    TODO,
}
