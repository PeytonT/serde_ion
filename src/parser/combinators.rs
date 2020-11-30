use crate::parser::parse_error::{IonError, IonResult};
use nom::{
    error::{ErrorKind, ParseError},
    AsBytes, AsChar, Err, IResult, InputIter, InputLength, Slice,
};
use std::{
    fmt::Debug,
    ops::{RangeFrom, RangeTo},
};

/// Consumes end of input, or errors if there is more data.
pub fn eof(i: &str) -> IonResult<&str, &str> {
    if i.is_empty() {
        Ok((i, i))
    } else {
        Err(Err::Error(IonError::from_error_kind(i, ErrorKind::Eof)))
    }
}

/// Takes one element from input if it matches predicate f
pub fn one_if<I, F, Error: ParseError<I>>(
    f: F,
) -> impl Fn(I) -> IResult<I, <I as InputIter>::Item, Error>
where
    I: Slice<RangeFrom<usize>> + InputIter,
    <I as InputIter>::Item: AsChar + Copy,
    F: Fn(<I as InputIter>::Item) -> bool,
{
    move |i: I| match (i).iter_elements().next().filter(|c| f(*c)) {
        Some(c) => Ok((i.slice(c.len()..), c)),
        None => Err(Err::Error(Error::from_error_kind(i, ErrorKind::OneOf))),
    }
}

#[allow(dead_code)]
/// A helper method for debugging the text parser.
/// Displays parser input and output (whether the output is an error or a successfully created object)
pub(crate) fn dbg_dmp<Input, F, Output>(
    context: &'static str,
    f: F,
) -> impl Fn(Input) -> IonResult<Input, Output>
where
    Input: Clone + Slice<RangeTo<usize>> + AsBytes + Debug + InputLength,
    Output: Debug,
    F: Fn(Input) -> IonResult<Input, Output>,
{
    move |i: Input| {
        log::debug!(" {}: -> {:?}", context, &i);
        match f(i.clone()) {
            Err(e) => {
                match &e {
                    Err::Failure(e) => {
                        log::debug!("{}: Failure({:?}) at: {:?}", context, e.kind, &i)
                    }
                    Err::Error(e) => log::debug!("{}: Error({:?}) at: {:?}", context, e.kind, &i),
                    Err::Incomplete(n) => log::debug!("{}: Err::Incomplete({:?}) at:", context, n),
                }
                Err(e)
            }
            Ok((i, v)) => {
                log::debug!(" {}: <- {:?}", context, &v);
                Ok((i, v))
            }
        }
    }
}
