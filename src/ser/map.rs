use crate::ser::{Error, Result, Serializer, Write};
use serde::ser;

pub struct SerializeMap<'a, W> {
    ser: &'a mut Serializer<W>,
    first: bool,
}

impl<'a, W> SerializeMap<'a, W>
where
    W: Write,
{
    pub(crate) fn new(ser: &'a mut Serializer<W>) -> Self {
        SerializeMap { ser, first: true }
    }
}

impl<'a, W> ser::SerializeMap for SerializeMap<'a, W>
where
    W: Write,
{
    type Ok = ();
    type Error = Error;

    fn end(self) -> Result<Self::Ok> {
        self.ser.push(b'}')?;
        Ok(())
    }

    fn serialize_key<T: ?Sized>(&mut self, key: &T) -> Result<()>
    where
        T: ser::Serialize,
    {
        if !self.first {
            self.ser.push(b',')?;
        }
        self.first = false;
        key.serialize(&mut *self.ser)?;
        self.ser.extend_from_slice(b":")?;
        Ok(())
    }

    fn serialize_value<T: ?Sized>(&mut self, value: &T) -> Result<()>
    where
        T: ser::Serialize,
    {
        value.serialize(&mut *self.ser)?;
        Ok(())
    }
}
