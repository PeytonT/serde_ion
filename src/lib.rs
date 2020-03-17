// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.
#![allow(dead_code, unused_variables)]
mod de;
pub mod error;
pub mod parser;
mod ser;
pub mod symbols;
pub mod value;

extern crate bit_vec;
extern crate serde;

#[macro_use]
extern crate num_derive;

#[macro_use]
extern crate lazy_static;

#[allow(non_camel_case_types)]
#[derive(Clone, Debug, PartialEq)]
pub enum Version {
    Ion_1_0,
}
