// Some content Copyright 2018 Serde Developers
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

mod ion_types;
mod error;
mod ser;
extern crate serde;

// #[cfg(test)]
// #[macro_use]
// extern crate serde_derive;

pub use crate::error::{Error, Result};
pub use ser::{to_string, to_bytes};
