use crate::parser::parse_error::{IonError, IonResult};
use nom::{
    error::{ErrorKind, ParseError},
    AsBytes, AsChar, Err, IResult, InputIter, InputLength, Slice,
};
use std::{
    fmt::Debug,
    ops::{RangeFrom, RangeTo},
};

/// A collection of parser combinators for building Ion parsers. Mostly forked from nom for various reasons.
/// FIXME: Modifying code from nom like this is unfortunate, and hopefully at some point will be unnecessary.
/// See https://github.com/Geal/nom/pull/1002

/// Matches an object from the first parser and discards it,
/// then gets an object from the second parser.
/// Derived from nom::sequence::preceded to lower the trait bound from Fn to FnMut.
pub fn preceded<I, O1, O2, F, G>(mut first: F, mut second: G) -> impl FnMut(I) -> IonResult<I, O2>
where
    F: FnMut(I) -> IonResult<I, O1>,
    G: FnMut(I) -> IonResult<I, O2>,
{
    move |input: I| {
        let (input, _) = first(input)?;
        second(input)
    }
}

/// Repeats the embedded parser until it fails and returns the results in a `Vec`.
/// Derived from nom::multi::many0 to lower the trait bound from Fn to FnMut.
pub fn many0<I, O, F>(mut f: F) -> impl FnMut(I) -> IonResult<I, Vec<O>>
where
    I: Clone + PartialEq,
    F: FnMut(I) -> IonResult<I, O>,
{
    move |i: I| {
        let mut acc = std::vec::Vec::with_capacity(4);
        let mut i = i;
        loop {
            match f(i.clone()) {
                Err(Err::Error(_)) => return Ok((i, acc)),
                Err(e) => return Err(e),
                Ok((i1, o)) => {
                    if i1 == i {
                        return Err(Err::Error(IonError::from_error_kind(i, ErrorKind::Many0)));
                    }

                    i = i1;
                    acc.push(o);
                }
            }
        }
    }
}

/// succeeds if all the input has been consumed by its child parser
/// Derived from nom::combinator::all_consuming to lower the trait bound from Fn to FnMut.
pub fn all_consuming<I, O, F>(mut f: F) -> impl FnMut(I) -> IonResult<I, O>
where
    I: InputLength,
    F: FnMut(I) -> IonResult<I, O>,
{
    move |input: I| {
        let (input, res) = f(input)?;
        if input.input_len() == 0 {
            Ok((input, res))
        } else {
            Err(Err::Error(IonError::from_error_kind(input, ErrorKind::Eof)))
        }
    }
}

/// maps a function on the result of a parser
/// Derived from nom::combinator::map to lower the trait bound from Fn to FnMut.
pub fn map<I, O1, O2, F, G>(mut first: F, second: G) -> impl FnMut(I) -> IonResult<I, O2>
where
    F: FnMut(I) -> IonResult<I, O1>,
    G: Fn(O1) -> O2,
{
    move |input: I| {
        let (input, o1) = first(input)?;
        Ok((input, second(o1)))
    }
}

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
