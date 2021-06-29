use crate::error::{FormatError, SymbolError};
use itertools::Itertools;
use nom::{error::ContextError, error::FromExternalError, error::ParseError, Err, Offset};
use std::str::from_utf8;

/// Analogous to nom's IResult.
pub type IonResult<I, T> = Result<(I, T), Err<IonError<I>>>;

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

impl<'a> IonError<&'a str> {
    pub(crate) fn into_bytes_err(self, origin: &str) -> IonError<&'a [u8]> {
        todo!()
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
