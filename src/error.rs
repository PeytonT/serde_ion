// Copyright 2018 Serde Developers
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use std;
use std::fmt::{self, Display};
use serde::{de, ser};

pub type Result<T> = std::result::Result<T, Error>;

// TODO Errors should contain more information
#[derive(Clone, Debug, PartialEq)]
pub enum Error {
    Message(String),
    Eof,
    Syntax,
    ExpectedBoolean,
    ExpectedInteger,
    ExpectedString,
    ExpectedNull,
    ExpectedArray,
    ExpectedArrayComma,
    ExpectedArrayEnd,
    ExpectedMap,
    ExpectedMapColon,
    ExpectedMapComma,
    ExpectedMapEnd,
    ExpectedEnum,
    TrailingCharacters,
}

impl ser::Error for Error {
    fn custom<T: Display>(msg: T) -> Self {
        Error::Message(msg.to_string())
    }
}

impl de::Error for Error {
    fn custom<T: Display>(msg: T) -> Self {
        Error::Message(msg.to_string())
    }
}

impl Display for Error {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str(std::error::Error::description(self))
    }
}

impl std::error::Error for Error {
    fn description(&self) -> &str {
        match self {
            Error::Message(msg) => &msg,
            Error::Eof => unimplemented!(),
            Error::Syntax => unimplemented!(),
            Error::ExpectedBoolean => unimplemented!(),
            Error::ExpectedInteger => unimplemented!(),
            Error::ExpectedString => unimplemented!(),
            Error::ExpectedNull => unimplemented!(),
            Error::ExpectedArray => unimplemented!(),
            Error::ExpectedArrayComma => unimplemented!(),
            Error::ExpectedArrayEnd => unimplemented!(),
            Error::ExpectedMap => unimplemented!(),
            Error::ExpectedMapColon => unimplemented!(),
            Error::ExpectedMapComma => unimplemented!(),
            Error::ExpectedMapEnd => unimplemented!(),
            Error::ExpectedEnum => unimplemented!(),
            Error::TrailingCharacters => unimplemented!(),
        }
    }
}
