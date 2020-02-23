#![warn(dead_code, unused_variables)]
#[cfg(test)]
mod tests;

use crate::error::SymbolError;
use crate::parser::ion_1_0::current_symbol_table::update_current_symbol_table;
use crate::{
    error::{FormatError, TextFormatError},
    parser::{
        ion_1_0::current_symbol_table::CurrentSymbolTable,
        parse_error::{IonError, IonResult},
    },
    symbols::SymbolToken,
    value::{self as ion},
};
use log::{debug, warn};
use nom::{
    self,
    branch::alt,
    bytes::complete::{
        escaped_transform, tag, tag_no_case, take_till, take_until, take_while, take_while1,
        take_while_m_n,
    },
    character::complete::{char, crlf, one_of},
    combinator::{
        all_consuming, cut, map, map_parser, map_res, not, opt, peek, recognize, value, verify,
    },
    error::{ErrorKind, ParseError},
    multi::{many0, many1, separated_list, separated_nonempty_list},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    AsBytes, AsChar, Compare, Err, ExtendInto, IResult, InputIter, InputLength, InputTake,
    InputTakeAtPosition, Offset, Slice,
};
use num_bigint::{BigInt, Sign};
use num_traits::{pow, Num, One, Zero};
use std::num::ParseIntError;
use std::{
    cell::RefCell,
    convert::TryFrom,
    f64::{INFINITY, NAN, NEG_INFINITY},
    fmt::Debug,
    iter::Extend,
    ops::{Range, RangeFrom, RangeTo},
    rc::Rc,
    str::{self, from_utf8},
};
use time::UtcOffset;

fn eof(i: &str) -> IonResult<&str, &str> {
    if i.is_empty() {
        Ok((i, i))
    } else {
        Err(Err::Error(IonError::from_error_kind(i, ErrorKind::Eof)))
    }
}

fn one_if<I, F, Error: ParseError<I>>(
    f: F,
) -> impl Fn(I) -> IResult<I, <I as InputIter>::Item, Error>
where
    I: Slice<RangeFrom<usize>> + InputIter,
    <I as InputIter>::Item: AsChar + Copy,
    F: Fn(<I as InputIter>::Item) -> bool,
{
    move |i: I| match (i).iter_elements().next().filter(|c| f(*c)) {
        Some(c) => Ok((i.slice(c.len()..), c)),
        _ => Err(Err::Error(Error::from_error_kind(i, ErrorKind::OneOf))),
    }
}

#[allow(dead_code)]
fn dbg_dmp<Input, F, Output>(
    context: &'static str,
    f: F,
) -> impl Fn(Input) -> IonResult<Input, Output>
where
    Input: Clone + Slice<RangeTo<usize>> + AsBytes + Debug + InputLength,
    Output: Debug,
    F: Fn(Input) -> IonResult<Input, Output>,
{
    move |i: Input| {
        debug!(" {}: -> {:?}", context, &i);
        match f(i.clone()) {
            Err(e) => {
                match &e {
                    Err::Failure(e) => debug!("{}: Failure({:?}) at: {:?}", context, e.kind, &i),
                    Err::Error(e) => debug!("{}: Error({:?}) at: {:?}", context, e.kind, &i),
                    Err::Incomplete(n) => debug!("{}: Err::Incomplete({:?}) at:", context, n),
                }
                Err(e)
            }
            Ok((i, v)) => {
                debug!(" {}: <- {:?}", context, &v);
                Ok((i, v))
            }
        }
    }
}

/// Follows the following documents:
/// Ion Text Encoding: http://amzn.github.io/ion-docs/docs/text.html
/// Ion Specification: http://amzn.github.io/ion-docs/docs/spec.html

/// TODO: Use FnMut combinators to get rid of the Rc<RefCell<_>>
type Table = Rc<RefCell<CurrentSymbolTable>>;

/// Ion streams consist of zero or more top level values (TLVs).
///
/// It is assumed that each one starts with the Ion Version Marker (or IVM) if not otherwise
/// marked. The IVM can also be used to reset the symbol table if later encountered as a TLV.
pub fn parse_ion_1_0(input: &str) -> Result<Vec<ion::Value>, String> {
    let mut values = vec![];
    for result in ValueIterator::new(input) {
        match result {
            Ok(value) => values.push(value),
            Err(e) => return Err(e),
        }
    }
    Ok(values)
}

/// Parses the top level values of the Ion string.
///
/// Some values are delimited by values other than whitespace. For example, (1+1) is four
/// distinct values. In this case the two values are parsed as a pair. If the value has
/// whitespace as a delimiter, only that value is parsed before continuing.
///
/// The last parsed value allows the final value to be parsed without the same delimiting rules.
///
/// Encoding: top_level
struct ValueIterator<'a> {
    remaining: &'a str,
    next: Option<ion::Value>,
    current_table: Table,
}

impl<'a> ValueIterator<'a> {
    fn new(ion: &'a str) -> Self {
        Self {
            remaining: ion,
            next: None,
            current_table: Rc::new(RefCell::new(CurrentSymbolTable::SystemV1)),
        }
    }

    fn as_shared_symbol_table<'v>(&self, value: &'v ion::Value) -> Option<&'v ion::Struct> {
        let table = if let ion::Data::Struct(Some(s)) = &value.value {
            s
        } else {
            return None;
        };

        let tagged_as_shared_symbol_table: bool =
            value
                .annotations
                .as_ref()
                .map_or(false, |annotations| match annotations.get(0) {
                    Some(Some(SymbolToken::Known { text }))
                        if text == "$ion_shared_symbol_table" =>
                    {
                        true
                    }
                    _ => false,
                });

        if tagged_as_shared_symbol_table {
            Some(table)
        } else {
            None
        }
    }

    fn as_local_symbol_table<'v>(&self, value: &'v ion::Value) -> Option<&'v ion::Struct> {
        let table = if let ion::Data::Struct(Some(s)) = &value.value {
            s
        } else {
            return None;
        };

        let tagged_as_local_symbol_table: bool =
            value
                .annotations
                .as_ref()
                .map_or(false, |annotations| match annotations.get(0) {
                    Some(Some(SymbolToken::Known { text })) if text == "$ion_symbol_table" => true,
                    _ => false,
                });

        if tagged_as_local_symbol_table {
            Some(table)
        } else {
            None
        }
    }

    fn handle_ivm(&mut self, ivm: Ivm) -> Result<(), String> {
        if ivm.0 == 1 && ivm.1 == 0 {
            self.current_table.replace(CurrentSymbolTable::SystemV1);
            Ok(())
        } else {
            // IVMs for versions of ION we don't support must trigger an error according to the
            // tests.
            Err(format!(
                "IonVersionMarker found for an unknown version of Ion: {}.{}",
                ivm.0, ivm.1
            ))
        }
    }

    fn handle_tlvs(
        &mut self,
        value: ion::Value,
        next: Option<ion::Value>,
    ) -> Option<Result<ion::Value, String>> {
        let value = if is_system_value(&value) {
            None
        } else if let Some(table) = self.as_local_symbol_table(&value) {
            match update_current_symbol_table(
                &mut self.current_table.borrow_mut(),
                &Some(table.clone()),
            ) {
                Ok(_) => None,
                Err(e) => Some(Err(e.to_string())),
            }
        } else if let Some(_table) = self.as_shared_symbol_table(&value) {
            // TODO: handle shared symbol tables
            self.next()
        } else {
            self.next = next;
            Some(Ok(value))
        };

        match value {
            Some(v) => Some(v),
            None => self.next(),
        }
    }
}

impl<'a> Iterator for ValueIterator<'a> {
    type Item = Result<ion::Value, String>;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next) = self.next.take() {
            return Some(Ok(next));
        }

        if self.remaining.is_empty() {
            return None;
        }

        let maybe_ivm = take_ivm(self.remaining);
        if let Ok((remaining, ivm)) = maybe_ivm {
            self.remaining = remaining;
            if let Err(e) = self.handle_ivm(ivm) {
                return Some(Err(e));
            }
        }

        let maybe_tlv =
            preceded(eat_ws, take_top_level_value(self.current_table.clone()))(self.remaining);
        match maybe_tlv {
            Ok((remaining, (value, next))) => {
                self.remaining = remaining;
                return self.handle_tlvs(value, next);
            }
            Err(Err::Failure(e)) => return Some(Err(format!("{:?}", e))),
            _ => (),
        }

        let maybe_last = preceded(
            eat_ws,
            terminated(opt(take_value(self.current_table.clone())), eof),
        )(self.remaining);
        match maybe_last {
            Ok((remaining, value)) => {
                self.remaining = remaining;
                if let Some(value) = value {
                    return self.handle_tlvs(value, None);
                }
            }
            Err(Err::Failure(e)) => return Some(Err(format!("{:?}", e))),
            _ => (),
        }

        if self.remaining.is_empty() {
            None
        } else {
            Some(Err(format!(
                "unable to parse remaining data: {:x?}",
                self.remaining
            )))
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct Ivm(u32, u32);

/// System values are NOPs, currently triggered by a symbol which matches the IVM.
fn is_system_value(value: &ion::Value) -> bool {
    !value.has_annotations()
        && value.value
            == ion::Data::Symbol(Some(SymbolToken::Known {
                text: "$ion_1_0".to_owned(),
            }))
}

fn take_ivm(i: &str) -> IonResult<&str, Ivm> {
    let (i, ivm) = preceded(
        eat_ws,
        terminated(
            preceded(
                tag("$ion_"),
                separated_pair(
                    take_while1(is_dec_digit),
                    char('_'),
                    take_while1(is_dec_digit),
                ),
            ),
            nl,
        ),
    )(i)?;

    let parse_version_number = |_: ParseIntError| -> nom::Err<IonError<&str>> {
        Err::Failure(IonError::from_format_error(
            i,
            FormatError::Text(TextFormatError::TODO),
        ))
    };
    let major = ivm.0.parse::<u32>().map_err(parse_version_number)?;
    let minor = ivm.1.parse::<u32>().map_err(parse_version_number)?;
    Ok((i, Ivm(major, minor)))
}

/// top_level_value
///     : annotation+ top_level_value
///     | delimiting_entity
///     // numeric literals (if followed by something), need to be followed by
///     // whitespace or a token that is either quoted (e.g. string) or
///     // starts with punctuation (e.g. clob, struct, list)
///     | numeric_entity ws
///     | numeric_entity quoted_annotation value
///     | numeric_entity delimiting_entity
///     // literals that are unquoted symbols or keywords have a similar requirement
///     // as the numerics above, they have different productions because the
///     // rules for numerics are the same in s-expressions, but keywords
///     // have different rules between top-level and s-expressions.
///     | keyword_entity ws
///     | keyword_entity quoted_annotation value
///     | keyword_entity keyword_delimiting_entity
///     ;
fn take_top_level_value(
    table: Table,
) -> impl Fn(&str) -> IonResult<&str, (ion::Value, Option<ion::Value>)> {
    move |i: &str| {
        let (i, (annotations, (value, next_value))) = pair(
            many0(map(take_annotation(table.clone()), Some)),
            take_top_level_data(table.clone()),
        )(i)?;
        let annotations = if annotations.is_empty() {
            None
        } else {
            Some(annotations)
        };

        Ok((i, (ion::Value { value, annotations }, next_value)))
    }
}

fn take_top_level_data(
    table: Table,
) -> impl Fn(&str) -> IonResult<&str, (ion::Data, Option<ion::Value>)> {
    move |i: &str| {
        alt((
            map(take_delimiting_entity(table.clone()), |data| (data, None)),
            alt((
                // Pairs of (Value, Option<Value>) depending on if parsing the next value was required.
                take_top_level_numeric_entity(table.clone()),
                take_top_level_keyword_entity(table.clone()),
            )),
        ))(i)
    }
}

fn take_top_level_numeric_entity(
    table: Table,
) -> impl Fn(&str) -> IonResult<&str, (ion::Data, Option<ion::Value>)> {
    move |i: &str| {
        take_delimited_value(
            take_numeric_entity,
            take_delimiting_entity(table.clone()),
            None,
            table.clone(),
        )(i)
    }
}

fn take_top_level_keyword_entity(
    table: Table,
) -> impl Fn(&str) -> IonResult<&str, (ion::Data, Option<ion::Value>)> {
    move |i: &str| {
        take_delimited_value(
            take_keyword_entity(table.clone()),
            take_keyword_delimiting_entity(table.clone()),
            None,
            table.clone(),
        )(i)
    }
}

/// Handles the awkward-to-implement top level value and sexp value delimiting rules.
///
/// Most values are delimited by whitespace. Some are not, particularly in s-expressions. Since
/// delimiting rules within s-expressions there are even more combinations. This method encapsulates
/// value delimiting and returns one or two values, depending on if the next value was necessary
/// for delimiting the first.
///
/// It would be an improvement to peek at the next character and just execute based on that,
/// especially if we are dealing with any kind of token stream. That approach is more error prone,
/// however, as we have to build the list of acceptable delimiting characters by hand.
fn take_delimited_value<'a, F, D>(
    value_parser: F,
    delimiter_parser: D,
    delimiter: Option<char>,
    table: Table,
) -> impl Fn(&'a str) -> IonResult<&'a str, (ion::Data, Option<ion::Value>)>
where
    F: Fn(&'a str) -> IonResult<&'a str, ion::Data>,
    D: Fn(&'a str) -> IonResult<&'a str, ion::Data>,
{
    move |i: &str| {
        // First check if we should continue by trying to parse a value.
        let (i, data) = value_parser(i)?;

        // Drop out if the next character is the optional delimiter
        if let Some(delimiter) = delimiter {
            if i.chars().next().map(|c| c == delimiter).unwrap_or(false) {
                return Ok((i, (data, None)));
            }
        };

        // Find the delimiter, which may be a quoted symbol for example
        let (i, next) = cut(alt((
            value(None, peek(many1(ws))),
            map(
                pair(
                    take_quoted_annotation(table.clone()),
                    take_value_parts(table.clone()),
                ),
                |(head_annotation, (mut rest_annotation, value))| {
                    // It is mandatory to maintain the order of annotations applied to an object.
                    rest_annotation.insert(0, Some(head_annotation));
                    Some(ion::Value {
                        value,
                        annotations: Some(rest_annotation),
                    })
                },
            ),
            map(&delimiter_parser, |value| {
                Some(ion::Value {
                    value,
                    annotations: None,
                })
            }),
            // Addition (not from encoding):
            // Inspired by 'good/timestamp/timestampWithTerminatingEof.ion'.
            // A parsed value was terminated by the end of the file.
            value(None, peek(eof)),
        )))(i)?;

        Ok((i, (data, next)))
    }
}

/// value
///     : annotation* entity
///     ;
fn take_value(table: Table) -> impl Fn(&str) -> IonResult<&str, ion::Value> {
    move |i: &str| {
        map(take_value_parts(table.clone()), |(annotations, value)| {
            let annotations = if annotations.is_empty() {
                None
            } else {
                Some(annotations)
            };
            ion::Value { value, annotations }
        })(i)
    }
}

fn take_value_parts(
    table: Table,
) -> impl Fn(&str) -> IonResult<&str, (Vec<Option<SymbolToken>>, ion::Data)> {
    move |i: &str| {
        pair(
            many0(map(take_annotation(table.clone()), Some)),
            take_entity(table.clone()),
        )(i)
    }
}

/// An entity is what the Ion Text Encoding refers to as a value partway through parsing which
/// hasn't been combined with any annotations yet. In this library, it is referred to as "Data".
///
/// Encoding: entity
/// entity
///     : numeric_entity
///     | delimiting_entity
///     | keyword_entity
///     ;
fn take_entity(table: Table) -> impl Fn(&str) -> IonResult<&str, ion::Data> {
    move |i: &str| {
        alt((
            take_numeric_entity,
            take_delimiting_entity(table.clone()),
            take_keyword_entity(table.clone()),
        ))(i)
    }
}

/// CLOBs and BLOBs are both surrounded with double curly braces, or LOB_START and
/// LOB_END. Since these are allowed in all the same places, this method is a single
/// entry-point into LOB parsing.
///
/// Both forms of LOBs consist of ASCII characters and are parsed as bytes. This method handles
/// transforming the parser from utf-8 parsing to &[u8] parsing and back again. This is necessary
/// in certain situations, such as expanding an escape for a character that takes an additional byte
/// when encoded in utf-8.
fn take_lob(i: &str) -> IonResult<&str, ion::Data> {
    let b = i.as_bytes();

    let result = preceded(
        terminated(tag(LOB_START), eat_whitespace),
        cut(terminated(
            take_lob_body,
            preceded(eat_whitespace, tag(LOB_END)),
        )),
    )(b);

    match result {
        Ok((i2, r)) => {
            let offset =
                i.offset(from_utf8(i2).expect(
                    "parser should return a reference to the same utf-8 slice it was given",
                ));
            Ok((&i[offset..], r))
        }
        Err(Err::Error(e)) => Err(Err::Error(e.into_str_err(i))),
        Err(Err::Failure(e)) => Err(Err::Failure(e.into_str_err(i))),
        Err(Err::Incomplete(n)) => Err(Err::Incomplete(n)),
    }
}

fn take_lob_body(i: &[u8]) -> IonResult<&[u8], ion::Data> {
    alt((
        map(take_short_quoted_clob, |c| ion::Data::Clob(Some(c))),
        map(take_long_quoted_clob, |c| ion::Data::Clob(Some(c))),
        map(take_blob_body, |b| ion::Data::Blob(Some(b))),
    ))(i)
}

/// Encoding: delimiting_entity
/// delimiting_entity
///     : quoted_text
///     | SHORT_QUOTED_CLOB
///     | LONG_QUOTED_CLOB
///     | BLOB
///     | list
///     | sexp
///     | struct
///     ;
fn take_delimiting_entity(table: Table) -> impl Fn(&str) -> IonResult<&str, ion::Data> {
    move |i: &str| {
        alt((
            take_quoted_text,
            take_lob,
            map(take_list(table.clone()), |l| ion::Data::List(Some(l))),
            map(take_sexp(table.clone()), |s| ion::Data::Sexp(Some(s))),
            map(take_struct(table.clone()), |s| ion::Data::Struct(Some(s))),
        ))(i)
    }
}

/// keyword_delimiting_entity
///     : delimiting_entity
///     | numeric_entity
///     ;
fn take_keyword_delimiting_entity(table: Table) -> impl Fn(&str) -> IonResult<&str, ion::Data> {
    move |i: &str| alt((take_delimiting_entity(table.clone()), take_numeric_entity))(i)
}

/// Note: take_identifier_symbol covers all cases provided by take_types, so types are omitted.
///
/// keyword_entity
///     : any_null
///     | BOOL
///     | SPECIAL_FLOAT
///     | IDENTIFIER_SYMBOL
///     // note that this is because we recognize the type names for null
///     // they are ordinary symbols on their own
///     | TYPE
///     ;
fn take_keyword_entity(table: Table) -> impl Fn(&str) -> IonResult<&str, ion::Data> {
    move |i: &str| {
        alt((
            take_any_null,
            map(take_bool, |b| ion::Data::Bool(Some(b))),
            map(take_special_float, |f| ion::Data::Float(Some(f))),
            map_res(take_identifier_symbol, |s| {
                let result = get_or_make_symbol(table.clone(), s.to_owned());
                match result {
                    Ok(symbol) => Ok(ion::Data::Symbol(Some(symbol))),
                    Err(e) => Err(Err::Failure(IonError::from_symbol_error(i, e))),
                }
            }),
        ))(i)
    }
}

/// Note: sole entry point to all numeric entity parsing (this includes timestamps)
///
/// numeric_entity
///     : BIN_INTEGER
///     | DEC_INTEGER
///     | HEX_INTEGER
///     | TIMESTAMP
///     | FLOAT
///     | DECIMAL
///     ;
fn take_numeric_entity(i: &str) -> IonResult<&str, ion::Data> {
    alt((
        map(take_timestamp, |t| ion::Data::Timestamp(Some(t))),
        take_float_or_decimal,
        map(
            alt((
                take_bin_integer,
                take_hex_integer,
                map_res(take_dec_integer, |s| str_to_bigint(s, 10)),
            )),
            |bigint| ion::Data::Int(Some(bigint)),
        ),
    ))(i)
}

/// annotation
///     : symbol ws* COLON COLON ws*
///     ;
fn take_annotation(table: Table) -> impl Fn(&str) -> IonResult<&str, SymbolToken> {
    move |i: &str| {
        terminated(
            terminated(take_symbol(table.clone()), eat_ws),
            terminated(tag("::"), eat_ws),
        )(i)
    }
}

/// quoted_annotation
///     : QUOTED_SYMBOL ws* COLON COLON ws*
///     ;
fn take_quoted_annotation(table: Table) -> impl Fn(&str) -> IonResult<&str, SymbolToken> {
    move |i: &str| {
        let (i, result) = map(
            terminated(
                terminated(take_quoted_symbol, eat_ws),
                terminated(tag("::"), eat_ws),
            ),
            |s| make_symbol(table.clone(), s),
        )(i)?;

        match result {
            Ok(token) => Ok((i, token)),
            Err(e) => Err(Err::Failure(IonError::from_symbol_error(i, e))),
        }
    }
}

/// list
///     : L_BRACKET ws* value ws* (COMMA ws* value)* ws* (COMMA ws*)? R_BRACKET
///     | L_BRACKET ws* R_BRACKET
///     ;
fn take_list(table: Table) -> impl Fn(&str) -> IonResult<&str, ion::List> {
    move |i: &str| {
        map(
            preceded(
                terminated(char(L_BRACKET), eat_ws),
                cut(terminated(
                    separated_list(
                        pair(char(COMMA), eat_ws),
                        terminated(take_value(table.clone()), eat_ws),
                    ),
                    preceded(opt(pair(char(COMMA), eat_ws)), char(R_BRACKET)),
                )),
            ),
            |values| ion::List { values },
        )(i)
    }
}

/// sexp
///     : L_PAREN (ws* sexp_value)* ws* value? R_PAREN
///     ;
fn take_sexp(table: Table) -> impl Fn(&str) -> IonResult<&str, ion::Sexp> {
    move |i: &str| {
        let (i, grouped_values) = preceded(
            terminated(char(L_PAREN), eat_ws),
            cut(terminated(
                many0(preceded(eat_ws, take_sexp_value(table.clone()))),
                preceded(eat_ws, char(R_PAREN)),
            )),
        )(i)?;

        let count = grouped_values.iter().fold(
            0,
            |sum, (_, next)| if next.is_some() { sum + 2 } else { sum + 1 },
        );

        let mut values = Vec::with_capacity(count);

        grouped_values.into_iter().for_each(|(first, next)| {
            values.push(first);
            if let Some(second) = next {
                values.push(second);
            }
        });
        Ok((i, ion::Sexp { values }))
    }
}

fn take_sexp_numeric_entity_data(
    table: Table,
) -> impl Fn(&str) -> IonResult<&str, (ion::Data, Option<ion::Value>)> {
    move |i: &str| {
        take_delimited_value(
            take_numeric_entity,
            take_delimiting_entity(table.clone()),
            Some(R_PAREN),
            table.clone(),
        )(i)
    }
}

fn take_sexp_keyword_data(
    table: Table,
) -> impl Fn(&str) -> IonResult<&str, (ion::Data, Option<ion::Value>)> {
    move |i: &str| {
        take_delimited_value(
            take_sexp_keyword_entity(table.clone()),
            take_sexp_keyword_delimiting_entity(table.clone()),
            Some(R_PAREN),
            table.clone(),
        )(i)
    }
}

fn take_sexp_null_data(
    table: Table,
) -> impl Fn(&str) -> IonResult<&str, (ion::Data, Option<ion::Value>)> {
    move |i: &str| {
        take_delimited_value(
            value(ion::Data::Null, take_null),
            take_sexp_null_delimiting_entity(table.clone()),
            Some(R_PAREN),
            table.clone(),
        )(i)
    }
}

fn take_sexp_data(
    table: Table,
) -> impl Fn(&str) -> IonResult<&str, (ion::Data, Option<ion::Value>)> {
    move |i: &str| {
        alt((
            map(take_delimiting_entity(table.clone()), |data| (data, None)),
            alt((
                // Pairs of (Value, Option<Value>) depending on if parsing the next value was required.
                take_sexp_numeric_entity_data(table.clone()),
                take_sexp_keyword_data(table.clone()),
                take_sexp_null_data(table.clone()),
            )),
            map(
                map(take_operator(table.clone()), |s| ion::Data::Symbol(Some(s))),
                |data| (data, None),
            ),
        ))(i)
    }
}

/// Note: the body of logic is in take_sexp_data and take_delimited_value
///
/// sexp_value
///     : annotation+ sexp_value
///     | sexp_delimiting_entity
///     | operator
///     // much like at the top level, numeric/identifiers/keywords
///     // have similar delimiting rules
///     | numeric_entity ws
///     | numeric_entity quoted_annotation value
///     | numeric_entity sexp_delimiting_entity
///     | sexp_keyword_entity ws
///     | sexp_keyword_entity quoted_annotation value
///     | sexp_keyword_entity sexp_keyword_delimiting_entity
///     | NULL ws
///     | NULL quoted_annotation value
///     | NULL sexp_null_delimiting_entity
///     ;
fn take_sexp_value(
    table: Table,
) -> impl Fn(&str) -> IonResult<&str, (ion::Value, Option<ion::Value>)> {
    move |i: &str| {
        map(
            pair(
                many0(map(take_annotation(table.clone()), Some)),
                take_sexp_data(table.clone()),
            ),
            |(annotations, (value, next))| {
                let annotations = if annotations.is_empty() {
                    None
                } else {
                    Some(annotations)
                };
                (ion::Value { value, annotations }, next)
            },
        )(i)
    }
}

/// Omitted - same as delimiting_entity.
///
/// sexp_delimiting_entity
///     : delimiting_entity
///     ;

/// sexp_keyword_delimiting_entity
///     : sexp_delimiting_entity
///     | numeric_entity
///     | operator
///     ;
fn take_sexp_keyword_delimiting_entity(
    table: Table,
) -> impl Fn(&str) -> IonResult<&str, ion::Data> {
    move |i: &str| {
        alt((
            take_delimiting_entity(table.clone()),
            take_numeric_entity,
            map(take_operator(table.clone()), |s| ion::Data::Symbol(Some(s))),
        ))(i)
    }
}

/// sexp_null_delimiting_entity
///     : delimiting_entity
///     | NON_DOT_OPERATOR+
///     ;
fn take_sexp_null_delimiting_entity(table: Table) -> impl Fn(&str) -> IonResult<&str, ion::Data> {
    move |i: &str| {
        alt((
            take_delimiting_entity(table.clone()),
            map(take_while1(is_non_dot_operator), |s: &str| {
                ion::Data::Symbol(Some(SymbolToken::Known {
                    text: s.to_string(),
                }))
            }),
        ))(i)
    }
}

fn take_sexp_keyword(i: &str) -> IonResult<&str, ion::Data> {
    alt((
        take_typed_null,
        value(ion::Data::Null, terminated(take_null, peek(not(char('.'))))),
        map(take_bool, |b| ion::Data::Bool(Some(b))),
        map(take_special_float, |f| ion::Data::Float(Some(f))),
        take_sexp_type,
    ))(i)
}

fn take_sexp_type(i: &str) -> IonResult<&str, ion::Data> {
    let (i, null_type) = take_type(i)?;
    if null_type == "null" {
        peek(not(char('.')))(i)?;
    }
    Ok((
        i,
        ion::Data::Symbol(Some(SymbolToken::Known {
            text: null_type.to_string(),
        })),
    ))
}

/// sexp_keyword_entity
///     : typed_null
///     | BOOL
///     | SPECIAL_FLOAT
///     | IDENTIFIER_SYMBOL
///     // note that this is because we recognize the type names for null
///     // they are ordinary symbols on their own
///     | TYPE
///     ;
fn take_sexp_keyword_entity(table: Table) -> impl Fn(&str) -> IonResult<&str, ion::Data> {
    move |i: &str| {
        let result = map(
            verify(take_identifier_symbol, |s| {
                not(all_consuming(take_sexp_keyword))(s).is_ok()
            }),
            |s| get_or_make_symbol(table.clone(), s.to_owned()),
        )(i);

        match result {
            Err(_) => (),
            Ok((i, Ok(token))) => return Ok((i, ion::Data::Symbol(Some(token)))),
            Ok((i, Err(e))) => return Err(Err::Failure(IonError::from_symbol_error(i, e))),
        }

        take_sexp_keyword(i)
    }
}

/// operator
///     : (DOT | NON_DOT_OPERATOR)+
///     ;
fn take_operator(_table: Table) -> impl Fn(&str) -> IonResult<&str, SymbolToken> {
    move |i: &str| {
        map(
            map(
                take_while1(|c| is_non_dot_operator(c) || c == DOT),
                String::from,
            ),
            |text| SymbolToken::Known { text },
        )(i)
    }
}

/// Encoding: struct
/// struct
///     : L_CURLY ws* field (ws* COMMA ws* field)* ws* (COMMA ws*)? R_CURLY
///     | L_CURLY ws* R_CURLY
///     ;
fn take_struct(table: Table) -> impl Fn(&str) -> IonResult<&str, ion::Struct> {
    move |i: &str| {
        map(
            preceded(
                terminated(char(L_CURLY), eat_ws),
                cut(alt((
                    value(vec![], char(R_CURLY)),
                    terminated(
                        verify(
                            separated_list(
                                pair(char(','), eat_ws),
                                terminated(take_field(table.clone()), eat_ws),
                            ),
                            |list: &[_]| !list.is_empty(),
                        ),
                        preceded(opt(pair(char(','), eat_ws)), char(R_CURLY)),
                    ),
                ))),
            ),
            |fields| ion::Struct { fields },
        )(i)
    }
}

/// field
///     : field_name ws* COLON ws* annotation* entity
///     ;
fn take_field(table: Table) -> impl Fn(&str) -> IonResult<&str, (SymbolToken, ion::Value)> {
    move |i: &str| {
        map(
            separated_pair(
                take_field_name(table.clone()),
                tuple((eat_ws, char(COLON), eat_ws)),
                pair(
                    many0(map(take_annotation(table.clone()), Some)),
                    take_entity(table.clone()),
                ),
            ),
            |(field, (annotations, value))| {
                let annotations = if annotations.is_empty() {
                    None
                } else {
                    Some(annotations)
                };
                (field, ion::Value { value, annotations })
            },
        )(i)
    }
}

/// any_null
///     : NULL
///     | typed_null
///     ;
fn take_any_null(i: &str) -> IonResult<&str, ion::Data> {
    alt((take_typed_null, value(ion::Data::Null, take_null)))(i)
}

/// typed_null
///     : NULL DOT NULL
///     | NULL DOT TYPE
///     ;
fn take_typed_null(i: &str) -> IonResult<&str, ion::Data> {
    let (i, null_type) = preceded(take_null, preceded(char(DOT), alt((take_type, take_null))))(i)?;

    let data = {
        match null_type {
            "null" => ion::Data::Null,
            "bool" => ion::Data::Bool(None),
            "int" => ion::Data::Int(None),
            "float" => ion::Data::Float(None),
            "decimal" => ion::Data::Decimal(None),
            "timestamp" => ion::Data::Timestamp(None),
            "string" => ion::Data::String(None),
            "symbol" => ion::Data::Symbol(None),
            "blob" => ion::Data::Blob(None),
            "clob" => ion::Data::Clob(None),
            "struct" => ion::Data::Struct(None),
            "list" => ion::Data::List(None),
            "sexp" => ion::Data::Sexp(None),
            _ => unreachable!("condition covers all possible parsed results"),
        }
    };

    Ok((i, data))
}

/// TODO: add field names parsed as strings to the symbol table.
///
/// field_name
///     : symbol
///     | SHORT_QUOTED_STRING
///     | (ws* LONG_QUOTED_STRING)+
///     ;
fn take_field_name(table: Table) -> impl Fn(&str) -> IonResult<&str, SymbolToken> {
    move |i: &str| {
        alt((
            take_symbol(table.clone()),
            map(take_short_quoted_string, |text| SymbolToken::Known { text }),
            map(many1(preceded(eat_ws, take_long_quoted_string)), |v| {
                SymbolToken::Known { text: v.concat() }
            }),
        ))(i)
    }
}

/// quoted_text
///     : QUOTED_SYMBOL
///     | SHORT_QUOTED_STRING
///     | (ws* LONG_QUOTED_STRING)+
///     ;
fn take_quoted_text(i: &str) -> IonResult<&str, ion::Data> {
    alt((
        map(take_quoted_symbol, |text| {
            ion::Data::Symbol(Some(SymbolToken::Known { text }))
        }),
        map(take_short_quoted_string, |s| ion::Data::String(Some(s))),
        map(
            map(many1(preceded(eat_ws, take_long_quoted_string)), |v| {
                v.concat()
            }),
            |s| ion::Data::String(Some(s)),
        ),
    ))(i)
}

fn get_or_make_symbol(table: Table, text: String) -> Result<SymbolToken, SymbolError> {
    if let Ok((_, s)) = take_sid(&text) {
        let sid = s
            .parse::<usize>()
            .map_err(|_| SymbolError::InvalidSid(s.to_string()))?;
        match table.borrow().lookup_sid(sid) {
            Ok(token) => Ok(token),
            Err(e) => Err(e),
        }
    } else {
        make_symbol(table, text)
    }
}

fn make_symbol(table: Table, text: String) -> Result<SymbolToken, SymbolError> {
    let token = SymbolToken::Known { text };
    table.borrow_mut().add_symbol(&token);
    Ok(token)
}

/// Note: IDENTIFIER_SYMBOL covers all cases provided by TYPE.
///
/// symbol
///     : IDENTIFIER_SYMBOL
///     // note that this is because we recognize the type names for null
///     // they are ordinary symbols on their own
///     | TYPE
///     | QUOTED_SYMBOL
///     ;
fn take_symbol(table: Table) -> impl Fn(&str) -> IonResult<&str, SymbolToken> {
    move |i: &str| {
        let (i, result) = alt((
            map(take_identifier_symbol, |s| {
                get_or_make_symbol(table.clone(), s.to_string())
            }),
            map(take_quoted_symbol, |s| make_symbol(table.clone(), s)),
        ))(i)?;

        match result {
            Ok(token) => Ok((i, token)),
            Err(e) => Err(Err::Failure(IonError::from_symbol_error(i, e))),
        }
    }
}

fn take_sid(i: &str) -> IonResult<&str, &str> {
    preceded(char('$'), take_while1(is_dec_digit))(i)
}

fn eat_ws(i: &str) -> IonResult<&str, &str> {
    recognize(many0(ws))(i)
}

/// ws
///     : WHITESPACE
///     | INLINE_COMMENT
///     | BLOCK_COMMENT
///     ;
fn ws(i: &str) -> IonResult<&str, &str> {
    alt((whitespace, take_inline_comment, take_block_comment))(i)
}

///
/// Encoding Section: Ion Punctuation
///

/// L_BRACKET : '[';
const L_BRACKET: char = '[';

/// R_BRACKET : ']';
const R_BRACKET: char = ']';

/// L_PAREN   : '(';
const L_PAREN: char = '(';

/// R_PAREN   : ')';
const R_PAREN: char = ')';

/// L_CURLY   : '{';
const L_CURLY: char = '{';

/// R_CURLY   : '}';
const R_CURLY: char = '}';

/// COMMA     : ',';
const COMMA: char = ',';

/// COLON     : ':';
const COLON: char = ':';

/// DOT       : '.';
const DOT: char = '.';

/// Note: test 'bad/sexpBackslashNL.ion' suggest that \ is not a valid operator character.
///
/// NON_DOT_OPERATOR
///     : [!#%&*+\-/;<=>?@^`|~]
///     ;
fn is_non_dot_operator(c: char) -> bool {
    [
        '!', '#', '%', '&', '*', '+', '-', '/', ';', '<', '=', '>', '?', '@', '^', '`', '|', '~',
    ]
    .contains(&c)
}

///
/// Encoding Section: Ion Whitespace / Comments
///

/// Consumes whitespace (not comments). Used in LOBs.
fn eat_whitespace<Input>(i: Input) -> IonResult<Input, Input>
where
    Input: Clone
        + PartialEq
        + InputIter
        + InputTake
        + InputLength
        + InputTakeAtPosition
        + Slice<RangeTo<usize>>
        + Offset,
    <Input as InputTakeAtPosition>::Item: AsChar,
{
    recognize(many0(whitespace))(i)
}

/// WHITESPACE
///     : WS+
///     ;
fn whitespace<Input>(i: Input) -> IonResult<Input, Input>
where
    Input: Clone + InputIter + InputTake + InputLength + InputTakeAtPosition,
    <Input as InputTakeAtPosition>::Item: AsChar,
{
    take_while1(is_ws)(i)
}

/// INLINE_COMMENT
///     : '//' .*? (NL | EOF)
///     ;
fn take_inline_comment(i: &str) -> IonResult<&str, &str> {
    preceded(
        tag("//"),
        terminated(take_till(|c| c == '\r' || c == '\n'), alt((nl, eof))),
    )(i)
}

/// BLOCK_COMMENT
///     : '/*' .*? '*/'
///     ;
fn take_block_comment(i: &str) -> IonResult<&str, &str> {
    preceded(tag("/*"), cut(terminated(take_until("*/"), tag("*/"))))(i)
}

///
/// Encoding Section: Ion Null
///

/// NULL
///     : 'null'
///     ;
fn take_null(i: &str) -> IonResult<&str, &str> {
    tag("null")(i)
}

/// Note: the encoding spec doesn't include null here, it was simpler to do so.
/// TYPE
///     : 'bool'
///     | 'int'
///     | 'float'
///     | 'decimal'
///     | 'timestamp'
///     | 'symbol'
///     | 'string'
///     | 'clob'
///     | 'blob'
///     | 'list'
///     | 'sexp'
///     | 'struct'
///     ;
fn take_type(i: &str) -> IonResult<&str, &str> {
    alt((
        tag("null"),
        tag("bool"),
        tag("int"),
        tag("float"),
        tag("decimal"),
        tag("timestamp"),
        tag("string"),
        tag("symbol"),
        tag("blob"),
        tag("clob"),
        tag("struct"),
        tag("list"),
        tag("sexp"),
    ))(i)
}

///
/// Encoding Section: Ion Bool
///

/// Encoding: BOOL
/// BOOL
///     : 'true'
///     | 'false'
///     ;
fn take_bool(i: &str) -> IonResult<&str, bool> {
    alt((value(true, tag("true")), value(false, tag("false"))))(i)
}

///
/// Encoding Section: Ion TextTimestamp
///

/// Encoding: fragment TIMESTAMP
/// TIMESTAMP
///     : DATE ('T' TIME?)?
///     | YEAR '-' MONTH 'T'
///     | YEAR 'T'
///     ;
fn take_timestamp(i: &str) -> IonResult<&str, ion::Timestamp> {
    let (i, timestamp) = map(
        alt((
            map_res(
                pair(take_date, opt(preceded(one_of("tT"), opt(take_time)))),
                |((year, month, day), maybe_time)| match ion::Date::day(year, month, day) {
                    Ok(date) => {
                        if let Some(time) = maybe_time {
                            Ok(ion::TextTimestamp::new(date, time))
                        } else {
                            Ok(ion::TextTimestamp::new(date, None))
                        }
                    }
                    Err(e) => Err(e),
                },
            ),
            // Timestamps which consist of only a year or year and month must be terminated by t or T.
            map(
                terminated(
                    separated_pair(take_year, char('-'), take_month),
                    one_of("tT"),
                ),
                |(year, month)| ion::TextTimestamp::new(ion::Date::Month { year, month }, None),
            ),
            map(terminated(take_year, one_of("tT")), |year| {
                ion::TextTimestamp::new(ion::Date::Year { year }, None)
            }),
        )),
        ion::Timestamp::Text,
    )(i)?;

    Ok((i, timestamp))
}

/// fragment
/// DATE
///     : YEAR '-' MONTH '-' DAY
///     ;
fn take_date(i: &str) -> IonResult<&str, (i32, u8, u8)> {
    let (i, (y, (m, d))) = pair(
        take_year,
        pair(
            preceded(char('-'), take_month),
            preceded(char('-'), take_day),
        ),
    )(i)?;

    Ok((i, (y, m, d)))
}

/// fragment
/// YEAR
///     : '000'                     [1-9]
///     | '00'            [1-9]     DEC_DIGIT
///     | '0'   [1-9]     DEC_DIGIT DEC_DIGIT
///     | [1-9] DEC_DIGIT DEC_DIGIT DEC_DIGIT
///     ;
fn take_year(i: &str) -> IonResult<&str, i32> {
    map(
        alt((
            recognize(pair(tag("000"), one_of("123456789"))),
            recognize(tuple((
                tag("00"),
                one_of("123456789"),
                one_if(is_dec_digit),
            ))),
            recognize(tuple((
                tag("0"),
                one_of("123456789"),
                take_while_m_n(2, 2, is_dec_digit),
            ))),
            recognize(tuple((
                one_of("123456789"),
                take_while_m_n(3, 3, is_dec_digit),
            ))),
        )),
        |s: &str| {
            s.parse::<i32>()
                .expect("the parser ensures it will be within range")
        },
    )(i)
}

/// fragment
/// MONTH
///     : '0' [1-9]
///     | '1' [0-2]
///     ;
fn take_month(i: &str) -> IonResult<&str, u8> {
    map(
        alt((
            recognize(pair(char('0'), one_of("123456789"))),
            recognize(pair(char('1'), one_of("012"))),
        )),
        |s: &str| {
            s.parse::<u8>()
                .expect("the parser ensures it will be within range")
        },
    )(i)
}

/// fragment
/// DAY
///     : '0'   [1-9]
///     | [1-2] DEC_DIGIT
///     | '3'   [0-1]
///     ;
fn take_day(i: &str) -> IonResult<&str, u8> {
    map(
        alt((
            recognize(pair(char('0'), one_of("123456789"))),
            recognize(pair(one_of("12"), one_if(is_dec_digit))),
            recognize(pair(char('3'), one_of("01"))),
        )),
        |s: &str| {
            s.parse::<u8>()
                .expect("the parser ensures it will be within range")
        },
    )(i)
}

fn take_hour_and_minute(i: &str) -> IonResult<&str, (u32, u32)> {
    separated_pair(take_hour, char(COLON), take_minute)(i)
}

type ParsedTime = ((u32, u32), Option<(u32, Option<BigInt>)>, UtcOffset);

fn assemble_time(((hour, minute), maybe_second, offset): ParsedTime) -> ion::Time {
    let (second, maybe_fractional) = if maybe_second.is_none() {
        return ion::Time::Minute {
            hour,
            minute,
            offset,
        };
    } else {
        maybe_second.expect("is not none")
    };

    let fractional = if maybe_fractional.is_none() {
        return ion::Time::Second {
            hour,
            minute,
            second,
            offset,
        };
    } else {
        maybe_fractional.expect("is not none")
    };

    ion::Time::FractionalSecond {
        hour,
        minute,
        second,
        fractional,
        offset,
    }
}

/// fragment
/// TIME
///     : HOUR ':' MINUTE (':' SECOND)? OFFSET
///     ;
fn take_time(i: &str) -> IonResult<&str, ion::Time> {
    map(
        tuple((
            take_hour_and_minute,
            opt(preceded(char(COLON), take_second)),
            take_offset,
        )),
        assemble_time,
    )(i)
}

/// fragment
/// OFFSET
///     : 'Z'
///     | PLUS_OR_MINUS HOUR ':' MINUTE
///     ;
fn take_offset(i: &str) -> IonResult<&str, UtcOffset> {
    alt((
        map(char('Z'), |_| UtcOffset::UTC),
        map(
            pair(take_plus_or_minus, take_hour_and_minute),
            |(sign, (hour, minutes))| {
                let minutes: i16 = ((hour as i16) * 60) + (minutes as i16);
                let signed_minutes = if sign == '-' { -minutes } else { minutes };
                UtcOffset::minutes(signed_minutes)
            },
        ),
    ))(i)
}

/// fragment
/// HOUR
///     : [01] DEC_DIGIT
///     | '2' [0-3]
///     ;
fn take_hour(i: &str) -> IonResult<&str, u32> {
    map(
        alt((
            recognize(pair(one_of("01"), one_if(is_dec_digit))),
            recognize(pair(char('2'), one_of("0123"))),
        )),
        |s: &str| {
            s.parse::<u32>()
                .expect("parser verified hour should be valid u32")
        },
    )(i)
}

/// fragment
/// MINUTE
///     : [0-5] DEC_DIGIT
///     ;
fn take_minute(i: &str) -> IonResult<&str, u32> {
    map(
        recognize(pair(one_of("012345"), one_if(is_dec_digit))),
        |s: &str| {
            s.parse::<u32>()
                .expect("parser verified minute should be valid u32")
        },
    )(i)
}

/// note that W3C spec requires a digit after the '.'
/// fragment
/// SECOND
///     : [0-5] DEC_DIGIT ('.' DEC_DIGIT+)?
///     ;
fn take_second(i: &str) -> IonResult<&str, (u32, Option<BigInt>)> {
    let (i, s) = recognize(pair(one_of("012345"), one_if(is_dec_digit)))(i)?;
    let (i, f) = opt(preceded(char('.'), take_while1(is_dec_digit)))(i)?;
    let seconds = s
        .parse::<u32>()
        .expect("parser verified seconds should be valid u32");
    if let Some(f) = f {
        let fractional = str_to_bigint(f, 10)?;
        Ok((i, (seconds, Some(fractional))))
    } else {
        Ok((i, (seconds, None)))
    }
}

///
/// Encoding Section: Ion Int
///

/// Helper for turning Vec<&str>s into BigInts. Or failing miserably.
fn str_to_bigint<'a, T: AsRef<str>>(
    digits: T,
    radix: u32,
) -> Result<BigInt, Err<IonError<&'a str>>> {
    match BigInt::from_str_radix(digits.as_ref(), radix) {
        Ok(bigint) => Ok(bigint),
        Err(e) => Err(Err::Failure(IonError::from_format_error(
            "",
            FormatError::Text(TextFormatError::BigInt(e, digits.as_ref().to_string())),
        ))),
    }
}

/// Helper for turning Vec<&str>s into BigUints. Or failing miserably.
fn str_vec_to_bigint(vec: Vec<&str>, radix: u32) -> Result<BigInt, Err<IonError<&str>>> {
    let digits: String = vec.concat();
    Ok(str_to_bigint(digits, radix)?)
}

/// BIN_INTEGER
///     : '-'? '0' [bB] BINARY_DIGIT (UNDERSCORE? BINARY_DIGIT)*
///     ;
fn take_bin_integer(i: &str) -> IonResult<&str, BigInt> {
    let (i, negate) = opt(char('-'))(i)?;
    let (i, segments) = preceded(
        tag_no_case("0b"),
        separated_nonempty_list(char(UNDERSCORE), take_while1(is_binary_digit)),
    )(i)?;

    let mut number =
        String::with_capacity(segments.iter().fold(0, |sum, s| sum + s.chars().count()) + 1);

    if let Some(negate) = negate {
        number.push(negate)
    }

    segments.into_iter().for_each(|s| number.push_str(s));

    Ok((i, str_to_bigint(number, 2)?))
}

/// Encoding: DEC_INTEGER
/// DEC_INTEGER
///     : '-'? DEC_UNSIGNED_INTEGER
///     ;
fn take_dec_integer(i: &str) -> IonResult<&str, String> {
    let (i, (negate, segments)) = pair(opt(char('-')), take_dec_unsigned_integer)(i)?;

    let negated_char = if negate.is_some() { 1 } else { 0 };
    let mut number = String::with_capacity(
        segments.iter().fold(0, |sum, s| sum + s.chars().count()) + negated_char,
    );

    if let Some(negate) = negate {
        number.push(negate)
    }

    segments.iter().for_each(|s| number.push_str(s));

    Ok((i, number))
}

/// HEX_INTEGER
///     : '-'? '0' [xX] HEX_DIGIT (UNDERSCORE? HEX_DIGIT)*
///     ;
fn take_hex_integer(i: &str) -> IonResult<&str, BigInt> {
    let (i, negate) = opt(char('-'))(i)?;
    let (i, segments) = preceded(
        tag_no_case("0x"),
        separated_nonempty_list(char(UNDERSCORE), take_while1(is_hex_digit)),
    )(i)?;

    let mut number =
        String::with_capacity(segments.iter().fold(0, |sum, s| sum + s.chars().count()) + 1);

    if let Some(negate) = negate {
        number.push(negate)
    }

    segments.iter().for_each(|s| number.push_str(s));

    Ok((i, str_to_bigint(number, 16)?))
}

///
/// Encoding Section: Ion Float
///

/// SPECIAL_FLOAT
///     : PLUS_OR_MINUS 'inf'
///     | 'nan'
///     ;
fn take_special_float(i: &str) -> IonResult<&str, f64> {
    alt((
        value(INFINITY, tag("+inf")),
        value(NEG_INFINITY, tag("-inf")),
        value(NAN, tag("nan")),
    ))(i)
}

enum FloatOrDecimal {
    Float(String),
    Decimal(String),
}

fn take_float_or_decimal(i: &str) -> IonResult<&str, ion::Data> {
    let (i, integer) = take_dec_integer(i)?;
    let (i, fractional) = opt(take_dec_frac)(i)?;
    let (i, exponent) = opt(alt((
        map(take_float_exp, FloatOrDecimal::Float),
        map(take_decimal_exp, FloatOrDecimal::Decimal),
    )))(i)?;

    // If there is no fractional and no exponent part then this is an integer.
    if fractional.is_none() && exponent.is_none() {
        return Err(Err::Error(IonError::from_error_kind(i, ErrorKind::Digit)));
    }

    match exponent {
        Some(FloatOrDecimal::Float(exponent)) => assemble_float(i, integer, fractional, exponent),
        Some(FloatOrDecimal::Decimal(exponent)) => {
            assemble_decimal(i, integer, fractional, Some(exponent))
        }
        None => assemble_decimal(i, integer, fractional, None),
    }
}

/// Note: number parsing consolidated to avoid parsing DEC_INTEGER multiple times when deciding
/// the numeric type to apply.
///
/// FLOAT
///     : DEC_INTEGER DEC_FRAC? FLOAT_EXP
///     ;
fn assemble_float<'a>(
    i: &'a str,
    integer: String,
    fractional: Option<Vec<&'a str>>,
    exponent: String,
) -> IonResult<&'a str, ion::Data> {
    let mut float = integer;

    if let Some(fractional) = fractional {
        float.push('.');
        fractional.iter().for_each(|s| float.push_str(s));
    }

    float.push('e');
    float.push_str(&exponent);

    match float.parse::<f64>() {
        Ok(f) => Ok((i, ion::Data::Float(Some(f)))),
        Err(e) => Err(Err::Failure(IonError::from_format_error(
            i,
            FormatError::Text(TextFormatError::FloatParse(float, e)),
        ))),
    }
}

/// fragment
/// FLOAT_EXP
///     : [Ee] PLUS_OR_MINUS? DEC_DIGIT+
///     ;
fn take_float_exp(i: &str) -> IonResult<&str, String> {
    take_exp("eE")(i)
}

fn take_exp(exponent_delimiters: &'static str) -> impl Fn(&str) -> IonResult<&str, String> {
    move |i: &str| {
        let (i, _) = one_of(exponent_delimiters)(i)?;
        let (i, sign) = opt(take_plus_or_minus)(i)?;
        let (i, digits) = take_while1(|c: char| c.is_ascii_digit())(i)?;

        let sign_char = if sign.is_some() { 1 } else { 0 };

        let mut exponent = String::with_capacity(digits.len() + sign_char);
        if let Some(sign) = sign {
            exponent.push(sign);
        }
        exponent.push_str(digits);

        Ok((i, exponent))
    }
}

///
/// Encoding Section: Ion Decimal
///

/// Note: number parsing consolidated to avoid parsing DEC_INTEGER multiple times when deciding
/// the numeric type to apply.
///
/// DECIMAL
///     : DEC_INTEGER DEC_FRAC? DECIMAL_EXP?
///     ;
fn assemble_decimal<'a>(
    i: &'a str,
    integer: String,
    fractional: Option<Vec<&'a str>>,
    exponent: Option<String>,
) -> IonResult<&'a str, ion::Data> {
    // coefficient drops -0
    let sign = if integer.starts_with('-') {
        Sign::Minus
    } else {
        Sign::Plus
    };
    let mut coefficient = str_to_bigint(integer, 10)?;

    // If we have a fractional value we have to normalize it so the value is an integer so
    // the values can be represented with the BigInt library via two integers like so:
    //   coefficient * pow(10, exponent)
    // This involves shifting the decimal some number of digits to the right and keeping
    // track of the shift for incorporation into any exponent value.
    let exponent_shift: usize = if let Some(fractional) = fractional {
        let shift_digits = fractional.iter().fold(0, |sum, s| sum + s.chars().count());
        if shift_digits > 0 {
            let fractional = str_vec_to_bigint(fractional, 10)?;

            coefficient *= pow(BigInt::one() * 10, shift_digits);
            match sign {
                Sign::Plus | Sign::NoSign => coefficient += fractional,
                Sign::Minus => coefficient -= fractional,
            }
        }

        shift_digits
    } else {
        0
    };

    let mut exponent = if let Some(exp_str) = exponent {
        str_to_bigint(&exp_str, 10)?
    } else {
        BigInt::zero()
    };

    if exponent_shift > 0 {
        exponent -= exponent_shift
    }

    Ok((
        i,
        ion::Data::Decimal(Some(ion::Decimal {
            coefficient,
            exponent,
        })),
    ))
}

/// fragment
/// DECIMAL_EXP
///     : [Dd] PLUS_OR_MINUS? DEC_DIGIT+
///     ;
fn take_decimal_exp(i: &str) -> IonResult<&str, String> {
    take_exp("dD")(i)
}

///
/// Encoding Section: Ion Symbol
///

/// QUOTED_SYMBOL
///     : SYMBOL_QUOTE SYMBOL_TEXT SYMBOL_QUOTE
///     ;
fn take_quoted_symbol(i: &str) -> IonResult<&str, String> {
    preceded(
        not(tag(LONG_QUOTE)),
        take_delimited_input(SYMBOL_QUOTE, |i| {
            escaped_transform(
                take_symbol_text,
                COMMON_ESCAPE,
                map(take_text_escape, |esc| esc.into_utf_8_escape()),
            )(i)
        }),
    )(i)
}

/// Note: escape parsing is handled in take_quoted_symbol.
/// fragment
/// SYMBOL_TEXT
///     : (TEXT_ESCAPE | SYMBOL_TEXT_ALLOWED)*
///     ;
fn take_symbol_text(i: &str) -> IonResult<&str, &str> {
    take_while1(is_symbol_text_allowed)(i)
}

/// non-control Unicode and not single quote or backslash
/// fragment
/// SYMBOL_TEXT_ALLOWED
///     : '\u0020'..'\u0026' // no C1 control characters and no U+0027 single quote
///     | '\u0028'..'\u005B' // no U+005C backslash
///     | '\u005D'..'\uFFFF' // should be up to U+10FFFF
///     | WS_NOT_NL
///     ;
fn is_symbol_text_allowed(c: char) -> bool {
    let scalar_value: u32 = c.into();

    is_ws_not_nl(c) || (scalar_value >= 0x20 && c != '\'' && c != '\\')
}

/// Identifies Ion values which should not be allowed to be keywords.
fn not_keyword(i: &str) -> bool {
    let result: IonResult<&str, ()> = not(all_consuming(alt((
        tag("null"),
        tag("nan"),
        tag("false"),
        tag("true"),
    ))))(i);

    result.is_ok()
}

/// Certain keywords are excluded from being identifier symbols as they need to be translated
/// into other Ion values.
///
/// IDENTIFIER_SYMBOL
///     : [$_a-zA-Z] ([$_a-zA-Z] | DEC_DIGIT)*
///     ;
fn take_identifier_symbol(i: &str) -> IonResult<&str, &str> {
    verify(
        recognize(pair(
            one_if(|c: char| c.is_ascii_alphabetic() || c == '_' || c == '$'),
            take_while(|c: char| {
                c.is_ascii_alphabetic() || c.is_ascii_digit() || c == '_' || c == '$'
            }),
        )),
        not_keyword,
    )(i)
}

/// Parses a delimiter, then the input body, then another delimiter.
/// Any error past the first delimiter is considered a failure.
fn take_delimited_input<Input, F, ExtendItem, Output>(
    delimiter: &'static str,
    body_parser: F,
) -> impl Fn(Input) -> IonResult<Input, Output>
where
    Input: Clone + Compare<&'static str> + InputTake + Slice<RangeTo<usize>> + InputLength,
    Input: ExtendInto<Item = ExtendItem, Extender = Output>,
    Output: Extend<<Input as ExtendInto>::Item>,
    F: Fn(Input) -> IonResult<Input, Output>,
{
    move |i: Input| {
        map(
            preceded(
                tag(delimiter),
                cut(terminated(opt(&body_parser), tag(delimiter))),
            ),
            |s| s.unwrap_or_else(|| i.new_builder()),
        )(i.clone())
    }
}

/// Checks for a long quote at each char while consuming allowed text.
/// Helper for STRING_LONG_TEXT_ALLOWED and CLOB_LONG_TEXT
fn take_long_quoted_string_text_allowed<Input, Output, ExtendItem, A>(
    is_allowed_char: A,
) -> impl Fn(Input) -> IonResult<Input, Input>
where
    Input: Clone
        + Compare<&'static str>
        + InputTake
        + Slice<RangeFrom<usize>>
        + Slice<RangeTo<usize>>
        + InputLength
        + InputIter
        + ExtendInto<Item = ExtendItem, Extender = Output>,
    <Input as InputIter>::Item: Clone + AsChar,
    Output: Extend<<Input as ExtendInto>::Item>,
    A: Fn(<Input as InputIter>::Item) -> bool,
{
    move |i: Input| {
        for (idx, c) in i.iter_indices() {
            if let (_, Some(_)) = opt(peek(tag(LONG_QUOTE)))(i.slice(idx..))? {
                return if idx == 0 {
                    Err(Err::Error(IonError::from_error_kind(
                        i,
                        ErrorKind::EscapedTransform,
                    )))
                } else {
                    Ok((i.slice(idx..), i.slice(..idx)))
                };
            };

            if !is_allowed_char(c.clone()) && c.as_char() != '\'' {
                return if idx == 0 {
                    Err(Err::Error(IonError::from_error_kind(
                        i,
                        ErrorKind::EscapedTransform,
                    )))
                } else {
                    Ok((i.slice(idx..), i.slice(..idx)))
                };
            }
        }
        Err(Err::Failure(IonError::from_format_error(
            i,
            FormatError::Text(TextFormatError::OpenLongString),
        )))
    }
}

///
/// Encoding Section: Ion String
///

/// SHORT_QUOTED_STRING
///     : SHORT_QUOTE STRING_SHORT_TEXT SHORT_QUOTE
///     ;
fn take_short_quoted_string(i: &str) -> IonResult<&str, String> {
    take_delimited_input("\"", |i| {
        escaped_transform(
            take_short_quoted_string_segment,
            COMMON_ESCAPE,
            map(take_text_escape, |esc| esc.into_utf_8_escape()),
        )(i)
    })(i)
}

/// LONG_QUOTED_STRING
///     : LONG_QUOTE STRING_LONG_TEXT LONG_QUOTE
///     ;
fn take_long_quoted_string(i: &str) -> IonResult<&str, String> {
    take_delimited_input(LONG_QUOTE, |i| {
        escaped_transform(
            take_long_quoted_string_segment,
            COMMON_ESCAPE,
            map(take_text_escape, |esc| esc.into_utf_8_escape()),
        )(i)
    })(i)
}

/// fragment
/// STRING_SHORT_TEXT
///     : (TEXT_ESCAPE | STRING_SHORT_TEXT_ALLOWED)*
///     ;
fn take_short_quoted_string_segment(i: &str) -> IonResult<&str, &str> {
    take_while1(is_string_short_text_allowed)(i)
}

/// Encoding: STRING_LONG_TEXT
/// fragment
/// STRING_LONG_TEXT
///     : (TEXT_ESCAPE | STRING_LONG_TEXT_ALLOWED)*?
///     ;
fn take_long_quoted_string_segment(i: &str) -> IonResult<&str, &str> {
    take_long_quoted_string_text_allowed(is_string_long_text_allowed)(i)
}

///  non-control Unicode and not double quote or backslash
/// fragment
/// STRING_SHORT_TEXT_ALLOWED
///     : '\u0020'..'\u0021' // no C1 control characters and no U+0022 double quote
///     | '\u0023'..'\u005B' // no U+005C backslash
///     | '\u005D'..'\uFFFF' // should be up to U+10FFFF
///     | WS_NOT_NL
///     ;
fn is_string_short_text_allowed(c: char) -> bool {
    let scalar_value: u32 = c.into();

    is_ws_not_nl(c) || (scalar_value >= 0x20 && c != '"' && c != '\\')
}

/// non-control Unicode (newlines are OK)
/// fragment
/// STRING_LONG_TEXT_ALLOWED
///     : '\u0020'..'\u005B' // no C1 control characters and no U+005C blackslash
///     | '\u005D'..'\uFFFF' // should be up to U+10FFFF
///     | WS
///     ;
fn is_string_long_text_allowed(c: char) -> bool {
    let scalar_value: u32 = c.into();

    is_ws(c) || (scalar_value >= 0x20 && c != '\\')
}

/// fragment
/// TEXT_ESCAPE
///     : COMMON_ESCAPE | HEX_ESCAPE | UNICODE_ESCAPE
///     ;
fn take_text_escape(i: &str) -> IonResult<&str, Escape> {
    cut(alt((
        take_common_escape_code,
        take_hex_escape,
        take_unicode_escape,
    )))(i)
}

///
/// Encoding Section: Ion CLOB
///

/// Note: quoting lowered to take_clob_short_text via take_delimited_input.
/// SHORT_QUOTED_CLOB
///     : LOB_START WS* SHORT_QUOTE CLOB_SHORT_TEXT SHORT_QUOTE WS* LOB_END
///     ;
fn take_short_quoted_clob(i: &[u8]) -> IonResult<&[u8], ion::Clob> {
    map(take_clob_short_text, |data| ion::Clob { data })(i)
}

/// Note: quoting lowered to take_clob_short_text via take_delimited_input.
/// LONG_QUOTED_CLOB
///     : LOB_START (WS* LONG_QUOTE CLOB_LONG_TEXT*? LONG_QUOTE)+ WS* LOB_END
///     ;
fn take_long_quoted_clob(i: &[u8]) -> IonResult<&[u8], ion::Clob> {
    let (i, vec) = many1(preceded(eat_whitespace, take_clob_long_text))(i)?;
    let data = vec.concat();
    Ok((i, ion::Clob { data }))
}

/// fragment
/// CLOB_SHORT_TEXT
///     : (CLOB_ESCAPE | CLOB_SHORT_TEXT_ALLOWED)*
///     ;
fn take_clob_short_text(i: &[u8]) -> IonResult<&[u8], Vec<u8>> {
    take_delimited_input(SHORT_QUOTE, |i| {
        escaped_transform(
            take_clob_short_text_allowed,
            COMMON_ESCAPE,
            map(take_clob_escape, |esc| esc.into_ascii_escape()),
        )(i)
    })(i)
}

/// The quoting strategy is taken care of by a long quoted string helper.
///
/// Encoding: CLOB_LONG_TEXT
/// fragment
/// CLOB_LONG_TEXT
///     : CLOB_LONG_TEXT_NO_QUOTE
///     | '\'' CLOB_LONG_TEXT_NO_QUOTE
///     | '\'\'' CLOB_LONG_TEXT_NO_QUOTE
///     ;
fn take_clob_long_text(i: &[u8]) -> IonResult<&[u8], Vec<u8>> {
    take_delimited_input(LONG_QUOTE, |i| {
        escaped_transform(
            take_clob_long_text_allowed,
            COMMON_ESCAPE,
            map(take_clob_escape, |esc| esc.into_ascii_escape()),
        )(i)
    })(i)
}

/// Omitted - using a shared long quoted string parser which can be specialized for bodies.
/// See take_long_quoted_string_text_allowed
///
/// fragment
/// CLOB_LONG_TEXT_NO_QUOTE
///     : (CLOB_ESCAPE | CLOB_LONG_TEXT_ALLOWED)
///     ;

/// Parser of CLOB_SHORT_TEXT_ALLOWED
fn take_clob_short_text_allowed(i: &[u8]) -> IonResult<&[u8], &[u8]> {
    take_while1(is_clob_short_text_allowed)(i)
}

/// non-control ASCII and not double quote or backslash
/// fragment
/// CLOB_SHORT_TEXT_ALLOWED
///     : '\u0020'..'\u0021' // no U+0022 double quote
///     | '\u0023'..'\u005B' // no U+005C backslash
///     | '\u005D'..'\u007F'
///     | WS_NOT_NL
///     ;
fn is_clob_short_text_allowed(b: u8) -> bool {
    is_ws_not_nl(b as char) || (b >= 0x20 && b.as_char() != '"' && b.as_char() != '\\' && b <= 0x7f)
}

/// Parser of CLOB_LONG_TEXT_ALLOWED
fn take_clob_long_text_allowed(i: &[u8]) -> IonResult<&[u8], &[u8]> {
    take_long_quoted_string_text_allowed(is_clob_long_text_allowed)(i)
}

/// non-control ASCII (newlines are OK)
/// fragment
/// CLOB_LONG_TEXT_ALLOWED
///     : '\u0020'..'\u0026' // no U+0027 single quote
///     | '\u0028'..'\u005B' // no U+005C blackslash
///     | '\u005D'..'\u007F'
///     | WS
///     ;
fn is_clob_long_text_allowed(b: u8) -> bool {
    // Note: we allow the single quote and explicitly lookahead for the triple quote while iterating.
    is_ws(b.as_char()) || (b >= 0x20 && b.as_char() != '\\' && b.as_char() != '\'' && b <= 0x7f)
}

/// fragment
/// CLOB_ESCAPE
///     : COMMON_ESCAPE | HEX_ESCAPE
///     ;
fn take_clob_escape<Input>(i: Input) -> IonResult<Input, Escape>
where
    Input: Clone
        + AsBytes
        + InputIter
        + InputLength
        + InputTake
        + Compare<&'static str>
        + Slice<Range<usize>>
        + Slice<RangeFrom<usize>>
        + Slice<RangeTo<usize>>,
    <Input as InputIter>::Item: AsChar,
{
    cut(alt((take_common_escape_code, take_hex_escape)))(i)
}

///
/// Encoding Section: Ion BLOB
///

/// LOB_START * LOB_END is handled by take_lob above which can be
/// specialized with a body parser.
///
/// BLOB
///     : LOB_START (BASE_64_QUARTET | WS)* BASE_64_PAD? WS* LOB_END
///     ;
fn take_blob_body(i: &[u8]) -> IonResult<&[u8], ion::Blob> {
    let (i, vec) = take_base_64(i)?;
    match base64::decode(&vec) {
        Ok(data) => Ok((i, ion::Blob { data })),
        Err(_) => Err(Err::Failure(IonError::from_format_error(
            i,
            FormatError::Text(TextFormatError::Base64Decode),
        ))),
    }
}

// Note: it is allowed for this body to be empty. This is the default "lob" type.
fn take_base_64(i: &[u8]) -> IonResult<&[u8], Vec<u8>> {
    let (i, (quartets, pad)) = pair(
        many0(delimited(
            eat_whitespace,
            take_base_64_quartet,
            eat_whitespace,
        )),
        terminated(opt(take_base_64_pad), eat_whitespace),
    )(i)?;

    let mut bytes = quartets.iter().fold(0, |acc, _| acc + 4);

    if pad.is_some() {
        bytes += 4;
    }

    let mut base_64_bytes =
        quartets
            .iter()
            .fold(Vec::with_capacity(bytes), |mut s, (s1, s2, s3, s4)| {
                s.extend_from_slice(&[*s1, *s2, *s3, *s4]);
                s
            });

    if let Some((s1, s2, s3)) = pad {
        base_64_bytes.extend_from_slice(&[s1, s2]);
        if let Some(s3) = s3 {
            base_64_bytes.push(s3);
        }
    }

    Ok((i, base_64_bytes))
}

/// fragment
/// BASE_64_PAD
///     : BASE_64_PAD1
///     | BASE_64_PAD2
///     ;
fn take_base_64_pad(i: &[u8]) -> IonResult<&[u8], (u8, u8, Option<u8>)> {
    if let Ok((i, (s1, s2, s3))) = take_base_64_pad1(i) {
        return Ok((i, (s1, s2, Some(s3))));
    }
    let (i, (s1, s2)) = take_base_64_pad2(i)?;
    Ok((i, (s1, s2, None)))
}

/// fragment
/// BASE_64_QUARTET
///     : BASE_64_CHAR WS* BASE_64_CHAR WS* BASE_64_CHAR WS* BASE_64_CHAR
///     ;
fn take_base_64_quartet(i: &[u8]) -> IonResult<&[u8], (u8, u8, u8, u8)> {
    let (i, ((s1, s2), (s3, s4))) = separated_pair(
        separated_pair(take_base_64_char, eat_whitespace, take_base_64_char),
        eat_whitespace,
        separated_pair(take_base_64_char, eat_whitespace, take_base_64_char),
    )(i)?;
    Ok((i, (s1, s2, s3, s4)))
}

/// fragment
/// BASE_64_PAD1
///     : BASE_64_CHAR WS* BASE_64_CHAR WS* BASE_64_CHAR WS* '='
///     ;
fn take_base_64_pad1(i: &[u8]) -> IonResult<&[u8], (u8, u8, u8)> {
    let (i, ((s1, s2), (s3, _))) = separated_pair(
        separated_pair(take_base_64_char, eat_whitespace, take_base_64_char),
        eat_whitespace,
        separated_pair(take_base_64_char, eat_whitespace, tag(b"=")),
    )(i)?;
    Ok((i, (s1, s2, s3)))
}

/// fragment
/// BASE_64_PAD2
///     : BASE_64_CHAR WS* BASE_64_CHAR WS* '=' WS* '='
///     ;
fn take_base_64_pad2(i: &[u8]) -> IonResult<&[u8], (u8, u8)> {
    let (i, ((s1, s2), _)) = separated_pair(
        separated_pair(take_base_64_char, eat_whitespace, take_base_64_char),
        eat_whitespace,
        separated_pair(tag(b"="), eat_whitespace, tag(b"=")),
    )(i)?;
    Ok((i, (s1, s2)))
}

/// fragment
/// BASE_64_CHAR
///     : [0-9a-zA-Z+/]
///     ;
fn take_base_64_char(i: &[u8]) -> IonResult<&[u8], u8> {
    one_if(is_base_64_char)(i)
}

fn is_base_64_char(b: u8) -> bool {
    (b as char).is_ascii_alphanumeric() || b == b'+' || b == b'/'
}

///
/// Encoding Section: Common Lexer Primitives
///

/// The encoding spec does not correctly define LOB_START and LOB_END.

/// fragment LOB_START    : ';
const LOB_START: &str = "{{";

/// fragment LOB_END      : ';
const LOB_END: &str = "}}";

/// fragment SYMBOL_QUOTE : '\'';
const SYMBOL_QUOTE: &str = "\'";

/// fragment SHORT_QUOTE  : '"';
const SHORT_QUOTE: &str = "\"";

/// fragment LONG_QUOTE   : '\'\'\'';
const LONG_QUOTE: &str = "'''";

/// fragment
/// DEC_UNSIGNED_INTEGER
///     : '0'
///     | [1-9] (UNDERSCORE? DEC_DIGIT)*
///     ;
fn take_dec_unsigned_integer(i: &str) -> IonResult<&str, Vec<&str>> {
    // Ion does not allow leading zeroes, hence the split here.
    let (i, first) = take_while_m_n(1, 1, |c: char| c.is_ascii_digit())(i)?;
    let mut digits = vec![first];

    if first == "0" {
        return Ok((i, digits));
    }

    let (i, rest) = many0(preceded(
        opt(char(UNDERSCORE)),
        take_while1(|c: char| c.is_ascii_digit()),
    ))(i)?;

    digits.extend_from_slice(&rest);

    Ok((i, digits))
}

/// fragment
/// DEC_FRAC
///     : '.'
///     | '.' DEC_DIGIT (UNDERSCORE? DEC_DIGIT)*
///     ;
fn take_dec_frac(i: &str) -> IonResult<&str, Vec<&str>> {
    preceded(
        char('.'),
        separated_list(char(UNDERSCORE), take_while1(|c: char| c.is_ascii_digit())),
    )(i)
}

/// fragment
/// DEC_DIGIT
///     : [0-9]
///     ;
fn is_dec_digit(c: char) -> bool {
    c.is_ascii_digit()
}

/// fragment
/// HEX_DIGIT
///     : [0-9a-fA-F]
///     ;
fn is_hex_digit<Input>(c: Input) -> bool
where
    Input: AsChar,
{
    c.as_char().is_ascii_hexdigit()
}

/// fragment
/// BINARY_DIGIT
///     : [01]
///     ;
fn is_binary_digit(c: char) -> bool {
    c == '0' || c == '1'
}

/// fragment
/// PLUS_OR_MINUS
///     : [+\-]
///     ;
fn take_plus_or_minus(i: &str) -> IonResult<&str, char> {
    one_of("+-")(i)
}

enum Escape {
    None,
    Char(char),
    Digits(String),
}

impl Escape {
    // Converts an escape
    fn into_ascii_escape(self) -> ByteEscape {
        match self {
            Escape::None => ByteEscape(None),
            Escape::Char(c) => {
                assert!(c as u32 <= u8::max_value() as u32);
                ByteEscape(Some(c as u8))
            }
            Escape::Digits(digits) => {
                assert!(digits.len() <= 2);
                if let Ok(b) = u8::from_str_radix(&digits, 16) {
                    return ByteEscape(Some(b));
                }
                unreachable!("ASCII character range confirmed by parser");
            }
        }
    }

    fn into_utf_8_escape(self) -> CharEscape {
        match self {
            Escape::None => CharEscape(None),
            Escape::Char(c) => CharEscape(Some(c)),
            Escape::Digits(digits) => {
                if let Ok(int) = u32::from_str_radix(&digits, 16) {
                    if let Ok(c) = char::try_from(int) {
                        return CharEscape(Some(c));
                    }
                }
                unreachable!("UTF-8 range confirmed by parser");
            }
        }
    }
}

struct CharEscape(Option<char>);
struct ByteEscape(Option<u8>);

impl ExtendInto for CharEscape {
    type Item = char;
    type Extender = String;

    #[inline]
    fn new_builder(&self) -> String {
        String::new()
    }
    #[inline]
    fn extend_into(&self, acc: &mut String) {
        if let Some(v) = self.0 {
            acc.push(v);
        }
    }
}

impl ExtendInto for ByteEscape {
    type Item = u8;
    type Extender = Vec<u8>;

    #[inline]
    fn new_builder(&self) -> Vec<u8> {
        vec![]
    }
    #[inline]
    fn extend_into(&self, acc: &mut Vec<u8>) {
        if let Some(v) = self.0 {
            acc.push(v);
        }
    }
}

/// fragment
/// COMMON_ESCAPE
///     : '\\' COMMON_ESCAPE_CODE
///     ;
const COMMON_ESCAPE: char = '\\';

/// fragment
/// COMMON_ESCAPE_CODE
///     : 'a'
///     | 'b'
///     | 't'
///     | 'n'
///     | 'f'
///     | 'r'
///     | 'v'
///     | '?'
///     | '0'
///     | '\''
///     | '"'
///     | '/'
///     | '\\'
///     | NL
///     ;
fn take_common_escape_code<Input>(i: Input) -> IonResult<Input, Escape>
where
    Input: Clone
        + InputIter
        + Compare<&'static str>
        + Slice<Range<usize>>
        + Slice<RangeFrom<usize>>
        + InputTake
        + Slice<RangeTo<usize>>,
    <Input as InputIter>::Item: AsChar,
{
    alt((
        map(
            alt((
                value('\x07', char('a')), // alarm (BEL)
                value('\x08', char('b')), // backspace (BS)
                value('\t', char('t')),   // horizontal tab (HT)
                value('\n', char('n')),   // linefeed (LF)
                value('\x0c', char('f')), // form feed (FF)
                value('\r', char('r')),   // carriage return (CR)
                value('\x0b', char('v')), // vertical tab (VT)
                value('?', char('?')),    // question mark
                value('\x00', char('0')), // null (NUL)
                value('\'', char('\'')),  // single quote
                value('"', char('"')),    // double quote
                value('/', char('/')),    // forward slash
                value('\\', char('\\')),  // backslash
            )),
            Escape::Char,
        ),
        // escaped NL expands to nothing
        map(nl, |_| Escape::None),
    ))(i)
}

/// Helper for mapping a successfully parsed hex string to an Escape
///
/// Note: unsafe to call from anywhere but map_parser - does not properly consume
fn hex_digits_to_escape<Input>(i: Input) -> IonResult<Input, Escape>
where
    Input: Clone + AsBytes,
{
    let i2 = i.clone();
    let b = i2.as_bytes();
    if let Ok(int) = u32::from_str_radix(from_utf8(b).unwrap(), 16) {
        if char::try_from(int).is_ok() {
            return Ok((
                i.clone(),
                Escape::Digits(from_utf8(i.as_bytes()).unwrap().to_string()),
            ));
        }
    };
    Err(Err::Failure(IonError::from_format_error(
        i,
        FormatError::Text(TextFormatError::HexEscape(
            from_utf8(b).unwrap().to_string(),
        )),
    )))
}

/// Note: escape character detected by Nom in escaped_transform
///
/// fragment
/// HEX_ESCAPE
///     : '\\x' HEX_DIGIT HEX_DIGIT
///     ;
fn take_hex_escape<Input>(i: Input) -> IonResult<Input, Escape>
where
    Input: Clone
        + AsBytes
        + InputLength
        + InputTake
        + InputIter
        + Compare<&'static str>
        + Slice<Range<usize>>
        + Slice<RangeFrom<usize>>
        + Slice<RangeTo<usize>>,
    <Input as InputIter>::Item: AsChar,
{
    map_parser(
        preceded(char('x'), take_while_m_n(2, 2, is_hex_digit)),
        hex_digits_to_escape,
    )(i)
}

/// Note: escape character detected by Nom in escaped_transform
///
/// fragment
/// UNICODE_ESCAPE
///     : '\\u'     HEX_DIGIT_QUARTET
///     | '\\U000'  HEX_DIGIT_QUARTET HEX_DIGIT
///     | '\\U0010' HEX_DIGIT_QUARTET
///     ;
fn take_unicode_escape(i: &str) -> IonResult<&str, Escape> {
    map_parser(
        alt((
            preceded(char('u'), take_while_m_n(4, 4, is_hex_digit)),
            preceded(tag("U000"), take_while_m_n(5, 5, is_hex_digit)),
            preceded(
                tag("U00"),
                recognize(pair(tag("10"), take_while_m_n(4, 4, is_hex_digit))),
            ),
        )),
        hex_digits_to_escape,
    )(i)
}

/// Omitted - see above.
/// fragment
/// HEX_DIGIT_QUARTET
///     : HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT
///     ;

/// fragment
/// WS
///     : WS_NOT_NL
///     | '\u000A' // line feed
///     | '\u000D' // carriage return
///     ;
pub fn is_ws<Input>(c: Input) -> bool
where
    Input: AsChar,
{
    let c = c.as_char();

    is_ws_not_nl(c)
    || c == '\x0d' // Carriage Return
    || c == '\x0a' // Line Feed
}

/// fragment
/// NL
///     : '\u000D\u000A'  // carriage return + line feed
///     | '\u000D'        // carriage return
///     | '\u000A'        // line feed
///     ;
fn nl<Input>(i: Input) -> IonResult<Input, Input>
where
    Input: Clone
        + Compare<&'static str>
        + Slice<Range<usize>>
        + InputIter
        + Slice<RangeFrom<usize>>
        + Slice<RangeTo<usize>>
        + InputTake,
{
    alt((crlf, tag("\r"), tag("\n")))(i)
}

/// fragment
/// WS_NOT_NL
///     : '\u0009' // tab
///     | '\u000B' // vertical tab
///     | '\u000C' // form feed
///     | '\u0020' // space
///     ;
pub fn is_ws_not_nl(c: char) -> bool {
    c == '\x09' // Tab
  || c == '\x0b' // Vertical Tab
  || c == '\x0c' // Form Feed
  || c == '\x20' // Space
}

/// fragment
/// UNDERSCORE
///     : '_'
///     ;
const UNDERSCORE: char = '_';
