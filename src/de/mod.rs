pub mod ion_1_0;

use crate::error::{Error, Result};
use serde::de::Visitor;
use serde::forward_to_deserialize_any;

//////////////////////////////////////////////////////////////////////////////

/// A structure that deserializes binary Ion into Rust values.
#[derive(Debug)]
pub struct BinaryDeserializer<'de> {
    input: &'de [u8],
}

impl<'de> BinaryDeserializer<'de> {
    /// Create a Deserializer from a slice of bytes.
    pub fn from_slice(input: &'de [u8]) -> Self {
        BinaryDeserializer { input }
    }

    /// The `Deserializer::end` method should be called after a value has been fully deserialized.
    /// This allows the `Deserializer` to validate that the input stream is at the end or that it
    /// only has trailing padding.
    pub fn end(&mut self) -> Result<()> {
        todo!()
    }
}

impl<'de> serde::de::Deserializer<'de> for BinaryDeserializer<'de> {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<<V as Visitor<'de>>::Value>
    where
        V: Visitor<'de>,
    {
        todo!()
    }

    forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct seq tuple
        tuple_struct map struct enum identifier ignored_any
    }
}

/// A structure that deserializes text Ion into Rust values.
#[derive(Debug)]
pub struct TextDeserializer<'de> {
    input: &'de str,
}

impl<'de> TextDeserializer<'de> {
    /// Create a Deserializer from a str.
    pub fn from_str(input: &'de str) -> Self {
        TextDeserializer { input }
    }

    /// The `Deserializer::end` method should be called after a value has been fully deserialized.
    /// This allows the `Deserializer` to validate that the input stream is at the end or that it
    /// only has trailing whitespace.
    pub fn end(&mut self) -> Result<()> {
        todo!()
    }
}

impl<'de> serde::de::Deserializer<'de> for TextDeserializer<'de> {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<<V as Visitor<'de>>::Value>
    where
        V: Visitor<'de>,
    {
        todo!()
    }

    forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct seq tuple
        tuple_struct map struct enum identifier ignored_any
    }
}
