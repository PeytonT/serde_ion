use std::str::from_utf8;

use itertools::Itertools;
use nom::{error::ContextError, error::FromExternalError, error::ParseError, Err, Offset};
use thiserror::Error;

use crate::symbols::ImportLocation;
use std::fmt::Display;

//////////////////////////////////////////////////////////////////////////////

pub type Result<T> = std::result::Result<T, Error>;

/// Analogous to nom's IResult.
pub type IonResult<I, T> = std::result::Result<(I, T), Err<IonError<I>>>;

#[derive(Error, Debug, PartialEq)]
pub enum Error {
    // TODO: Handle this better.
    #[error("nom parsing error not converted to any other form of error")]
    Nom(),
    // TODO: Handle this better.
    #[error("string message for Serde")]
    Serde(String),
    #[error("invalid symbol")]
    Symbol(SymbolError),
    #[error("invalid format")]
    Format(FormatError),
}

impl serde::de::Error for Error {
    fn custom<T>(msg: T) -> Self
    where
        T: Display,
    {
        Self::Serde(format!("{}", msg))
    }
}

impl serde::ser::Error for Error {
    fn custom<T>(msg: T) -> Self
    where
        T: Display,
    {
        Self::Serde(format!("{}", msg))
    }
}

impl From<SymbolError> for Error {
    fn from(error: SymbolError) -> Self {
        Error::Symbol(error)
    }
}

impl From<FormatError> for Error {
    fn from(error: FormatError) -> Self {
        Error::Format(error)
    }
}

impl<I> From<IonError<I>> for Error {
    fn from(error: IonError<I>) -> Self {
        match error.kind {
            ErrorKind::Nom(_, _) => Error::Nom(),
            ErrorKind::Symbol(_, error) => Error::Symbol(error),
            ErrorKind::Format(_, error) => Error::Format(error),
        }
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
    #[error("the provided import location `{0}` cannot be resolved")]
    UnresolvableImport(ImportLocation),
    #[error("the provided symbol table is invalid")]
    InvalidSymbolTable,
    #[error("invalid max_id for import in symbol table: {0:?}")]
    InvalidMaxId(String),
    #[error("unsupported version for import in symbol table: {0:?}")]
    UnsupportedVersion(String),
    #[error("invalid SID (outside numeric range): {0:?}")]
    SidTooLarge(String),
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
    #[error("int 0 is stored with type code T == 2, negative encodings are invalid.")]
    NegativeZero,
    #[error("bool value `{0}` is not allowed")]
    BoolValue(u8),
    #[error("floats may only have lengths of 4 or 8 bytes, length code `{0}` is not allowed")]
    FloatLength(u8),
    #[error("timestamps must contain an offset and a year, length code of `{0}` is not allowed")]
    TimestampLength(u8),
    #[error("strings must be encoded using utf8")]
    StringEncoding,
    #[error("structs with length_code L1 cannot be empty")]
    StructEmpty,
    #[error("structs with length_code L1 must have increasing field-name integers")]
    StructUnordered,
    #[error("invalid local symbol table")]
    LocalTable,
    #[error("time component has value outsize of allowed range: {0} - {1}")]
    TimeComponentRange(TimeComponent, String),
}

#[derive(Error, Debug, PartialEq)]
pub enum TimeComponent {
    #[error("offset")]
    Offset,
    #[error("year")]
    Year,
    #[error("month")]
    Month,
    #[error("day")]
    Day,
    #[error("hour")]
    Hour,
    #[error("minute")]
    Minute,
    #[error("second")]
    Second,
    #[error("fraction")]
    Fraction,
}

#[derive(Error, Debug, PartialEq)]
pub enum TextFormatError {
    #[error("invalid hex escape: {0}")]
    HexEscape(String),
    #[error("unterminated short quoted string")]
    OpenShortString,
    #[error("unterminated long quoted string")]
    OpenLongString,
    #[error("invalid biguint: {0}")]
    BigUint(String),
    #[error("invalid bigint: {0}")]
    BigInt(String),
    #[error("unable to decode Base64 value")]
    Base64Decode,
    #[error("unable to parse float value: {0}")]
    FloatParse(String),
    #[error("date out of range (invalid day)")]
    DateOutOfRange,
    #[error("Ion Version Marker indicates an unsupported version of Ion: {0}.{1}")]
    UnsupportedVersion(u32, u32),
    #[error("Ion Version Marker could not be parsed (int component too big)")]
    IvmParseError,
    #[error("Date is too imprecise to include time components")]
    ImpreciseDate,
}

#[derive(Debug, PartialEq)]
pub struct IonError<I> {
    pub kind: ErrorKind<I>,
    backtrace: Vec<ErrorKind<I>>,
}

impl<'a> IonError<&'a [u8]> {
    pub(crate) fn into_str_err(self, origin: &'a str) -> IonError<&'a str> {
        let kind = self.kind.into_str_kind(origin);
        let backtrace = self
            .backtrace
            .into_iter()
            .map(|b| b.into_str_kind(origin))
            .collect_vec();

        IonError { kind, backtrace }
    }
}

#[derive(Debug, PartialEq)]
pub enum ErrorKind<I> {
    Nom(I, nom::error::ErrorKind),
    // your error types as the rest of the variants
    Symbol(I, SymbolError),
    Format(I, FormatError),
}

impl<'a> ErrorKind<&'a [u8]> {
    pub(crate) fn into_str_kind(self, origin: &'a str) -> ErrorKind<&'a str> {
        let translate_offset =
            |i| {
                origin.offset(from_utf8(i).expect(
                    "parser should return a reference to the same utf-8 slice it was given",
                ))
            };
        match self {
            ErrorKind::Nom(i, kind) => ErrorKind::Nom(&origin[translate_offset(i)..], kind),
            ErrorKind::Symbol(i, e) => ErrorKind::Symbol(&origin[translate_offset(i)..], e),
            ErrorKind::Format(i, e) => ErrorKind::Format(&origin[translate_offset(i)..], e),
        }
    }
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
}

impl<I> ContextError<I> for IonError<I> {
    fn add_context(_input: I, _ctx: &'static str, other: Self) -> Self {
        other
    }
}

impl<I, E> FromExternalError<I, E> for IonError<I> {
    fn from_external_error(input: I, kind: nom::error::ErrorKind, _e: E) -> Self {
        IonError {
            kind: ErrorKind::Nom(input, kind),
            // TODO: Preserve useful backtrace.
            backtrace: vec![],
        }
    }
}

impl<I> From<(I, nom::error::ErrorKind)> for IonError<I> {
    fn from(err: (I, nom::error::ErrorKind)) -> Self {
        Self::from_error_kind(err.0, err.1)
    }
}
