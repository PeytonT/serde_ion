use crate::error::{IonError, IonResult};
use nom::error::ParseError;
use nom::InputLength;
use nom::{error::ErrorKind, Err};

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
