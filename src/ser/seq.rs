use crate::ser::{Error, Result, Serializer, Write};
use serde::ser;

pub struct SerializeSeq<'a, W> {
    de: &'a mut Serializer<W>,
    first: bool,
}

impl<'a, W> SerializeSeq<'a, W>
where
    W: Write,
{
    pub(crate) fn new(de: &'a mut Serializer<W>) -> Self {
        SerializeSeq { de, first: true }
    }
}

impl<'a, W> ser::SerializeSeq for SerializeSeq<'a, W>
where
    W: Write,
{
    type Ok = ();
    type Error = Error;

    fn serialize_element<T: ?Sized>(&mut self, value: &T) -> Result<()>
    where
        T: ser::Serialize,
    {
        if !self.first {
            self.de.push(b',')?;
        }
        self.first = false;

        value.serialize(&mut *self.de)?;
        Ok(())
    }

    fn end(self) -> Result<Self::Ok> {
        self.de.push(b']')?;
        Ok(())
    }
}

impl<'a, W> ser::SerializeTuple for SerializeSeq<'a, W>
where
    W: Write,
{
    type Ok = ();
    type Error = Error;

    fn serialize_element<T: ?Sized>(&mut self, value: &T) -> Result<()>
    where
        T: ser::Serialize,
    {
        ser::SerializeSeq::serialize_element(self, value)
    }

    fn end(self) -> Result<Self::Ok> {
        ser::SerializeSeq::end(self)
    }
}

impl<'a, W> ser::SerializeTupleStruct for SerializeSeq<'a, W>
where
    W: Write,
{
    type Ok = ();
    type Error = Error;

    fn serialize_field<T: ?Sized>(&mut self, value: &T) -> Result<()>
    where
        T: ser::Serialize,
    {
        ser::SerializeSeq::serialize_element(self, value)
    }

    fn end(self) -> Result<Self::Ok> {
        ser::SerializeSeq::end(self)
    }
}
