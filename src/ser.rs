// Copyright 2018 Serde Developers
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use serde::ser::{self, Serialize};
use crate::error::{Error, Result};
use bytes::{BytesMut, BufMut};

pub fn to_string<T>(value: &T) -> Result<String>
where
    T: Serialize,
{
    let mut serializer = text::TextSerializer {
        output: String::new(),
    };
    value.serialize(&mut serializer)?;
    Ok(serializer.output)
}

pub fn to_bytes<T>(value: &T) -> Result<BytesMut>
where
    T: Serialize,
{
    let mut serializer = binary::BinarySerializer {
        output: BytesMut::new(),
    };
    value.serialize(&mut serializer)?;
    Ok(serializer.output)
}

mod text {
    use super::{ser, Serialize, Error, Result};

    pub struct TextSerializer {
        pub output: String,
    }

    impl<'a> ser::Serializer for &'a mut TextSerializer {
        type Ok = ();
        type Error = Error;

        type SerializeSeq = Self;
        type SerializeTuple = Self;
        type SerializeTupleStruct = Self;
        type SerializeTupleVariant = Self;
        type SerializeMap = Self;
        type SerializeStruct = Self;
        type SerializeStructVariant = Self;

        fn serialize_bool(self, v: bool) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }

        fn serialize_i8(self, v: i8) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }

        fn serialize_i16(self, v: i16) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }

        fn serialize_i32(self, v: i32) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }

        fn serialize_i64(self, v: i64) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }

        fn serialize_u8(self, v: u8) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }

        fn serialize_u16(self, v: u16) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }

        fn serialize_u32(self, v: u32) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }

        fn serialize_u64(self, v: u64) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }

        fn serialize_f32(self, v: f32) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }

        fn serialize_f64(self, v: f64) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }

        fn serialize_char(self, v: char) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }

        fn serialize_str(self, v: &str) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }

        fn serialize_bytes(self, v: &[u8]) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }

        fn serialize_none(self) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }

        fn serialize_some<T>(self, value: &T) -> Result<()>
        where
            T: ?Sized + Serialize,
        {
            // TODO add logic
            unimplemented!()
        }

        fn serialize_unit(self) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }

        fn serialize_unit_struct(self, _name: &'static str) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }

        // When serializing a unit variant (or any other kind of variant), formats
        // can choose whether to keep track of it by index or by name. Binary
        // formats typically use the index of the variant and human-readable formats
        // typically use the name.
        fn serialize_unit_variant(
            self,
            _name: &'static str,
            _variant_index: u32,
            variant: &'static str,
        ) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }

        // As is done here, serializers are encouraged to treat newtype structs as
        // insignificant wrappers around the data they contain.
        fn serialize_newtype_struct<T>(
            self,
            _name: &'static str,
            value: &T,
        ) -> Result<()>
        where
            T: ?Sized + Serialize,
        {
            // TODO add logic
            unimplemented!()
        }

        // Note that newtype variant (and all of the other variant serialization
        // methods) refer exclusively to the "externally tagged" enum
        // representation.
        fn serialize_newtype_variant<T>(
            self,
            _name: &'static str,
            _variant_index: u32,
            variant: &'static str,
            value: &T,
        ) -> Result<()>
        where
            T: ?Sized + Serialize,
        {
            // TODO add logic
            unimplemented!()
        }

        // COMPOUND TYPES
        //
        // The start of the sequence, each value, and the end are three separate
        // method calls. The length of the sequence may or may not be known ahead of time.
        // Some serializers may only be able to support sequences for which the length is
        // known up front.
        fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq> {
            // TODO add logic
            unimplemented!()
        }

        // Some formats may be able to represent tuples more efficiently by omitting
        // the length, since tuple means that the corresponding `Deserialize
        // implementation will know the length without needing to look at the serialized data.
        fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple> {
            // TODO add logic
            unimplemented!()
        }

        fn serialize_tuple_struct(
            self,
            _name: &'static str,
            len: usize,
        ) -> Result<Self::SerializeTupleStruct> {
            // TODO add logic
            unimplemented!()
        }

        fn serialize_tuple_variant(
            self,
            _name: &'static str,
            _variant_index: u32,
            variant: &'static str,
            _len: usize,
        ) -> Result<Self::SerializeTupleVariant> {
            // TODO add logic
            unimplemented!()
        }

        fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap> {
            // TODO add logic
            unimplemented!()
        }

        fn serialize_struct(
            self,
            _name: &'static str,
            len: usize,
        ) -> Result<Self::SerializeStruct> {
            // TODO add logic
            unimplemented!()
        }

        // This is the externally tagged representation.
        fn serialize_struct_variant(
            self,
            _name: &'static str,
            _variant_index: u32,
            variant: &'static str,
            _len: usize,
        ) -> Result<Self::SerializeStructVariant> {
            // TODO add logic
            unimplemented!()
        }
    }

    // The following 7 impls deal with the serialization of compound types like
    // sequences and maps. Serialization of such types is begun by a Serializer
    // method and followed by zero or more calls to serialize individual elements of
    // the compound type and one call to end the compound type.
    //
    // This impl is SerializeSeq so these methods are called after `serialize_seq`
    // is called on the Serializer.
    impl<'a> ser::SerializeSeq for &'a mut TextSerializer {
        // Must match the `Ok` and `Error` types of the serializer.
        type Ok = ();
        type Error = Error;

        // Serialize a single element of the sequence.
        fn serialize_element<T>(&mut self, value: &T) -> Result<()>
        where
            T: ?Sized + Serialize,
        {
            // TODO add logic
            unimplemented!()
        }

        // Close the sequence.
        fn end(self) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }
    }

    // Same thing but for tuples.
    impl<'a> ser::SerializeTuple for &'a mut TextSerializer {
        type Ok = ();
        type Error = Error;

        fn serialize_element<T>(&mut self, value: &T) -> Result<()>
        where
            T: ?Sized + Serialize,
        {
            // TODO add logic
            unimplemented!()
        }

        fn end(self) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }
    }

    // Same thing but for tuple structs.
    impl<'a> ser::SerializeTupleStruct for &'a mut TextSerializer {
        type Ok = ();
        type Error = Error;

        fn serialize_field<T>(&mut self, value: &T) -> Result<()>
        where
            T: ?Sized + Serialize,
        {
            // TODO add logic
            unimplemented!()
        }

        fn end(self) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }
    }

    // Tuple variants are a little different. Refer back to the
    // `serialize_tuple_variant` method above:
    //
    //    self.output += "{";
    //    variant.serialize(&mut *self)?;
    //    self.output += ":[";
    //
    // So the `end` method in this impl is responsible for closing both the `]` and
    // the `}`.
    impl<'a> ser::SerializeTupleVariant for &'a mut TextSerializer {
        type Ok = ();
        type Error = Error;

        fn serialize_field<T>(&mut self, value: &T) -> Result<()>
        where
            T: ?Sized + Serialize,
        {
            // TODO add logic
            unimplemented!()
        }

        fn end(self) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }
    }

    // Some `Serialize` types are not able to hold a key and value in memory at the
    // same time so `SerializeMap` implementations are required to support
    // `serialize_key` and `serialize_value` individually.
    //
    // There is a third optional method on the `SerializeMap` trait. The
    // `serialize_entry` method allows serializers to optimize for the case where
    // key and value are both available simultaneously.
    impl<'a> ser::SerializeMap for &'a mut TextSerializer {
        type Ok = ();
        type Error = Error;

        // A serializer needs to validate that map keys are strings.
        // This can be done by using a different Serializer to serialize the key
        // (instead of `&mut **self`) and having that other serializer only
        // implement `serialize_str` and return an error on any other data type.
        fn serialize_key<T>(&mut self, key: &T) -> Result<()>
        where
            T: ?Sized + Serialize,
        {
            // TODO add logic
            unimplemented!()
        }

        fn serialize_value<T>(&mut self, value: &T) -> Result<()>
        where
            T: ?Sized + Serialize,
        {
            // TODO add logic
            unimplemented!()
        }

        fn end(self) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }
    }

    // Structs are like maps in which the keys are constrained to be compile-time
    // constant strings.
    impl<'a> ser::SerializeStruct for &'a mut TextSerializer {
        type Ok = ();
        type Error = Error;

        fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
        where
            T: ?Sized + Serialize,
        {
            // TODO add logic
            unimplemented!()
        }

        fn end(self) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }
    }

    // Similar to `SerializeTupleVariant`, here the `end` method is responsible for
    // closing both of the curly braces opened by `serialize_struct_variant`.
    impl<'a> ser::SerializeStructVariant for &'a mut TextSerializer {
        type Ok = ();
        type Error = Error;

        fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
        where
            T: ?Sized + Serialize,
        {
            // TODO add logic
            unimplemented!()
        }

        fn end(self) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }
    }
}

mod binary {
    use super::{ser, Serialize, Error, Result, BytesMut, BufMut};

    pub struct BinarySerializer {
        pub output: BytesMut,
    }

    impl<'a> ser::Serializer for &'a mut BinarySerializer {
        type Ok = ();
        type Error = Error;

        type SerializeSeq = Self;
        type SerializeTuple = Self;
        type SerializeTupleStruct = Self;
        type SerializeTupleVariant = Self;
        type SerializeMap = Self;
        type SerializeStruct = Self;
        type SerializeStructVariant = Self;

        fn serialize_bool(self, v: bool) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }

        fn serialize_i8(self, v: i8) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }

        fn serialize_i16(self, v: i16) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }

        fn serialize_i32(self, v: i32) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }

        fn serialize_i64(self, v: i64) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }

        fn serialize_u8(self, v: u8) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }

        fn serialize_u16(self, v: u16) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }

        fn serialize_u32(self, v: u32) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }

        fn serialize_u64(self, v: u64) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }

        fn serialize_f32(self, v: f32) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }

        fn serialize_f64(self, v: f64) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }

        fn serialize_char(self, v: char) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }

        fn serialize_str(self, v: &str) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }

        fn serialize_bytes(self, v: &[u8]) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }

        fn serialize_none(self) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }

        fn serialize_some<T>(self, value: &T) -> Result<()>
        where
            T: ?Sized + Serialize,
        {
            // TODO add logic
            unimplemented!()
        }

        fn serialize_unit(self) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }

        fn serialize_unit_struct(self, _name: &'static str) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }

        // When serializing a unit variant (or any other kind of variant), formats
        // can choose whether to keep track of it by index or by name. Binary
        // formats typically use the index of the variant and human-readable formats
        // typically use the name.
        fn serialize_unit_variant(
            self,
            _name: &'static str,
            _variant_index: u32,
            variant: &'static str,
        ) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }

        // As is done here, serializers are encouraged to treat newtype structs as
        // insignificant wrappers around the data they contain.
        fn serialize_newtype_struct<T>(
            self,
            _name: &'static str,
            value: &T,
        ) -> Result<()>
        where
            T: ?Sized + Serialize,
        {
            // TODO add logic
            unimplemented!()
        }

        // Note that newtype variant (and all of the other variant serialization
        // methods) refer exclusively to the "externally tagged" enum
        // representation.
        fn serialize_newtype_variant<T>(
            self,
            _name: &'static str,
            _variant_index: u32,
            variant: &'static str,
            value: &T,
        ) -> Result<()>
        where
            T: ?Sized + Serialize,
        {
            // TODO add logic
            unimplemented!()
        }

        // COMPOUND TYPES
        //
        // The start of the sequence, each value, and the end are three separate
        // method calls. The length of the sequence may or may not be known ahead of time.
        // Some serializers may only be able to support sequences for which the length is
        // known up front.
        fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq> {
            // TODO add logic
            unimplemented!()
        }

        // Some formats may be able to represent tuples more efficiently by omitting
        // the length, since tuple means that the corresponding `Deserialize
        // implementation will know the length without needing to look at the serialized data.
        fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple> {
            // TODO add logic
            unimplemented!()
        }

        fn serialize_tuple_struct(
            self,
            _name: &'static str,
            len: usize,
        ) -> Result<Self::SerializeTupleStruct> {
            // TODO add logic
            unimplemented!()
        }

        fn serialize_tuple_variant(
            self,
            _name: &'static str,
            _variant_index: u32,
            variant: &'static str,
            _len: usize,
        ) -> Result<Self::SerializeTupleVariant> {
            // TODO add logic
            unimplemented!()
        }

        fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap> {
            // TODO add logic
            unimplemented!()
        }

        fn serialize_struct(
            self,
            _name: &'static str,
            len: usize,
        ) -> Result<Self::SerializeStruct> {
            // TODO add logic
            unimplemented!()
        }

        // This is the externally tagged representation.
        fn serialize_struct_variant(
            self,
            _name: &'static str,
            _variant_index: u32,
            variant: &'static str,
            _len: usize,
        ) -> Result<Self::SerializeStructVariant> {
            // TODO add logic
            unimplemented!()
        }
    }

    // The following 7 impls deal with the serialization of compound types like
    // sequences and maps. Serialization of such types is begun by a Serializer
    // method and followed by zero or more calls to serialize individual elements of
    // the compound type and one call to end the compound type.
    //
    // This impl is SerializeSeq so these methods are called after `serialize_seq`
    // is called on the Serializer.
    impl<'a> ser::SerializeSeq for &'a mut BinarySerializer {
        // Must match the `Ok` and `Error` types of the serializer.
        type Ok = ();
        type Error = Error;

        // Serialize a single element of the sequence.
        fn serialize_element<T>(&mut self, value: &T) -> Result<()>
        where
            T: ?Sized + Serialize,
        {
            // TODO add logic
            unimplemented!()
        }

        // Close the sequence.
        fn end(self) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }
    }

    // Same thing but for tuples.
    impl<'a> ser::SerializeTuple for &'a mut BinarySerializer {
        type Ok = ();
        type Error = Error;

        fn serialize_element<T>(&mut self, value: &T) -> Result<()>
        where
            T: ?Sized + Serialize,
        {
            // TODO add logic
            unimplemented!()
        }

        fn end(self) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }
    }

    // Same thing but for tuple structs.
    impl<'a> ser::SerializeTupleStruct for &'a mut BinarySerializer {
        type Ok = ();
        type Error = Error;

        fn serialize_field<T>(&mut self, value: &T) -> Result<()>
        where
            T: ?Sized + Serialize,
        {
            // TODO add logic
            unimplemented!()
        }

        fn end(self) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }
    }

    // Tuple variants are a little different. Refer back to the
    // `serialize_tuple_variant` method above:
    //
    //    self.output += "{";
    //    variant.serialize(&mut *self)?;
    //    self.output += ":[";
    //
    // So the `end` method in this impl is responsible for closing both the `]` and
    // the `}`.
    impl<'a> ser::SerializeTupleVariant for &'a mut BinarySerializer {
        type Ok = ();
        type Error = Error;

        fn serialize_field<T>(&mut self, value: &T) -> Result<()>
        where
            T: ?Sized + Serialize,
        {
            // TODO add logic
            unimplemented!()
        }

        fn end(self) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }
    }

    // Some `Serialize` types are not able to hold a key and value in memory at the
    // same time so `SerializeMap` implementations are required to support
    // `serialize_key` and `serialize_value` individually.
    //
    // There is a third optional method on the `SerializeMap` trait. The
    // `serialize_entry` method allows serializers to optimize for the case where
    // key and value are both available simultaneously.
    impl<'a> ser::SerializeMap for &'a mut BinarySerializer {
        type Ok = ();
        type Error = Error;

        // A serializer needs to validate that map keys are strings.
        // This can be done by using a different Serializer to serialize the key
        // (instead of `&mut **self`) and having that other serializer only
        // implement `serialize_str` and return an error on any other data type.
        fn serialize_key<T>(&mut self, key: &T) -> Result<()>
        where
            T: ?Sized + Serialize,
        {
            // TODO add logic
            unimplemented!()
        }

        fn serialize_value<T>(&mut self, value: &T) -> Result<()>
        where
            T: ?Sized + Serialize,
        {
            // TODO add logic
            unimplemented!()
        }

        fn end(self) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }
    }

    // Structs are like maps in which the keys are constrained to be compile-time
    // constant strings.
    impl<'a> ser::SerializeStruct for &'a mut BinarySerializer {
        type Ok = ();
        type Error = Error;

        fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
        where
            T: ?Sized + Serialize,
        {
            // TODO add logic
            unimplemented!()
        }

        fn end(self) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }
    }

    // Similar to `SerializeTupleVariant`, here the `end` method is responsible for
    // closing both of the curly braces opened by `serialize_struct_variant`.
    impl<'a> ser::SerializeStructVariant for &'a mut BinarySerializer {
        type Ok = ();
        type Error = Error;

        fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
        where
            T: ?Sized + Serialize,
        {
            // TODO add logic
            unimplemented!()
        }

        fn end(self) -> Result<()> {
            // TODO add logic
            unimplemented!()
        }
    }
}
