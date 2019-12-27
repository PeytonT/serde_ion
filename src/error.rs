use nom::{error::ParseError, Err, IResult};
use thiserror::Error;

pub type IonIResult<I, T> = Result<(I, T), Err<IonError<I>>>;
//pub type IonIResult<I, T> = IResult<I, T, IonError<I>>;

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
}

impl<I> IonError<I> {
    pub(crate) fn from_symbol_error(input: I, error: SymbolError) -> Self {
        Self {
            kind: ErrorKind::Symbol(input, error),
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

impl<I> From<(I, nom::error::ErrorKind)> for IonError<I> {
    fn from(err: (I, nom::error::ErrorKind)) -> Self {
        Self::from_error_kind(err.0, err.1)
    }
}

pub fn convert_error<I>(err: Err<(I, nom::error::ErrorKind)>) -> Err<IonError<I>> {
    Err::convert(err)
}

pub fn convert_result<I, O>(result: IResult<I, O>) -> IonIResult<I, O> {
    match result {
        Ok(ok) => Ok(ok),
        Result::Err(err) => Err(Err::convert(err)),
    }
}

#[derive(Error, Debug, PartialEq)]
pub enum SymbolError {
    #[error("invalid symbol_id {symbol_id:?} is greater than local max_id {max_id:?})")]
    AboveMaxId { max_id: u32, symbol_id: u32 },
    #[error("invalid symbol_id {symbol_id:?} is less than local min_local_id {min_local_id:?})")]
    BelowMinId { min_local_id: u32, symbol_id: u32 },
    #[error("the text for SID `{0}` is unknown")]
    UnknownSymbolText(u32),
}
