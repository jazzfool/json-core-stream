use crate::ser::{Error, Result, Serializer, Write};
use serde::ser;

pub struct SerializeStruct<'a, W> {
    ser: &'a mut Serializer<W>,
    first: bool,
}

impl<'a, W> SerializeStruct<'a, W>
where
    W: Write,
{
    pub(crate) fn new(ser: &'a mut Serializer<W>) -> Self {
        SerializeStruct { ser, first: true }
    }
}

impl<'a, W> ser::SerializeStruct for SerializeStruct<'a, W>
where
    W: Write,
{
    type Ok = ();
    type Error = Error;

    fn serialize_field<T: ?Sized>(&mut self, key: &'static str, value: &T) -> Result<()>
    where
        T: ser::Serialize,
    {
        // XXX if `value` is `None` we not produce any output for this field
        if !self.first {
            self.ser.push(b',')?;
        }
        self.first = false;

        self.ser.push(b'"')?;
        self.ser.extend_from_slice(key.as_bytes())?;
        self.ser.extend_from_slice(b"\":")?;

        value.serialize(&mut *self.ser)?;

        Ok(())
    }

    fn end(self) -> Result<Self::Ok> {
        self.ser.push(b'}')?;
        Ok(())
    }
}

pub struct SerializeStructVariant<'a, W> {
    ser: &'a mut Serializer<W>,
    first: bool,
}

impl<'a, W> SerializeStructVariant<'a, W>
where
    W: Write,
{
    pub(crate) fn new(ser: &'a mut Serializer<W>) -> Self {
        SerializeStructVariant { ser, first: true }
    }
}

impl<'a, W> ser::SerializeStructVariant for SerializeStructVariant<'a, W>
where
    W: Write,
{
    type Ok = ();
    type Error = Error;

    fn serialize_field<T: ?Sized>(&mut self, key: &'static str, value: &T) -> Result<()>
    where
        T: ser::Serialize,
    {
        // XXX if `value` is `None` we not produce any output for this field
        if !self.first {
            self.ser.push(b',')?;
        }
        self.first = false;

        self.ser.push(b'"')?;
        self.ser.extend_from_slice(key.as_bytes())?;
        self.ser.extend_from_slice(b"\":")?;

        value.serialize(&mut *self.ser)?;

        Ok(())
    }

    fn end(self) -> Result<Self::Ok> {
        self.ser.extend_from_slice(b"}}")?;
        Ok(())
    }
}
