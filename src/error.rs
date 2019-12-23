use nom::{error::ParseError, IResult};
use thiserror::Error;

pub type Result<I, T> = IResult<I, T, Error<I>>;

pub struct Error<I> {
    pub kind: ErrorKind<I>,
    backtrace: Vec<ErrorKind<I>>,
}

pub enum ErrorKind<I> {
    Nom(I, nom::error::ErrorKind),
    // your error types as the rest of the variants
    Symbol(SymbolError),
}

impl<I> ParseError<I> for Error<I> {
    fn from_error_kind(input: I, kind: nom::error::ErrorKind) -> Self {
        Self {
            kind: ErrorKind::Nom(input, kind),
            backtrace: Vec::new(),
        }
    }

    fn append(input: I, kind: nom::error::ErrorKind, mut other: Self) -> Self {
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

#[derive(Error, Debug)]
pub enum SymbolError {
    #[error("invalid symbol_id {symbol_id:?} is greater than local max_id {max_id:?})")]
    AboveMaxId { max_id: u32, symbol_id: u32 },
    #[error("invalid symbol_id {symbol_id:?} is less than local min_local_id {min_local_id:?})")]
    BelowMinId { min_local_id: u32, symbol_id: u32 },
    #[error("the text for SID `{0}` is unknown")]
    UnknownSymbolText(u32),
}
