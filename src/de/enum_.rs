use crate::de::{Deserializer, Error, Read, Result};
use serde::de;

pub(crate) struct UnitVariantAccess<'a, R, const N: usize> {
    de: &'a mut Deserializer<R, N>,
}

impl<'a, 'de, R, const N: usize> UnitVariantAccess<'a, R, N>
where
    R: Read<'de>,
{
    pub(crate) fn new(de: &'a mut Deserializer<R, N>) -> Self {
        UnitVariantAccess { de }
    }
}

impl<'a, 'de, R, const N: usize> de::EnumAccess<'de> for UnitVariantAccess<'a, R, N>
where
    R: Read<'de>,
{
    type Error = Error;
    type Variant = Self;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self)>
    where
        V: de::DeserializeSeed<'de>,
    {
        let variant = seed.deserialize(&mut *self.de)?;
        Ok((variant, self))
    }
}

impl<'a, 'de, R, const N: usize> de::VariantAccess<'de> for UnitVariantAccess<'a, R, N>
where
    R: Read<'de>,
{
    type Error = Error;

    fn unit_variant(self) -> Result<()> {
        Ok(())
    }

    fn newtype_variant_seed<T>(self, _seed: T) -> Result<T::Value>
    where
        T: de::DeserializeSeed<'de>,
    {
        Err(Error::InvalidType)
    }

    fn tuple_variant<V>(self, _len: usize, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        Err(Error::InvalidType)
    }

    fn struct_variant<V>(self, _fields: &'static [&'static str], _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        Err(Error::InvalidType)
    }
}

pub(crate) struct VariantAccess<'a, R, const N: usize> {
    de: &'a mut Deserializer<R, N>,
}

impl<'a, 'de, R, const N: usize> VariantAccess<'a, R, N>
where
    R: Read<'de>,
{
    pub(crate) fn new(de: &'a mut Deserializer<R, N>) -> Self {
        VariantAccess { de }
    }
}

impl<'a, 'de, R, const N: usize> de::EnumAccess<'de> for VariantAccess<'a, R, N>
where
    R: Read<'de>,
{
    type Error = Error;
    type Variant = Self;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self)>
    where
        V: de::DeserializeSeed<'de>,
    {
        let variant = seed.deserialize(&mut *self.de)?;
        self.de.parse_object_colon()?;
        Ok((variant, self))
    }
}

impl<'a, 'de, R, const N: usize> de::VariantAccess<'de> for VariantAccess<'a, R, N>
where
    R: Read<'de>,
{
    type Error = Error;

    fn unit_variant(self) -> Result<()> {
        de::Deserialize::deserialize(self.de)
    }

    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value>
    where
        T: de::DeserializeSeed<'de>,
    {
        seed.deserialize(self.de)
    }

    fn tuple_variant<V>(self, _len: usize, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        de::Deserializer::deserialize_seq(self.de, visitor)
    }

    fn struct_variant<V>(self, fields: &'static [&'static str], visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        de::Deserializer::deserialize_struct(self.de, "", fields, visitor)
    }
}
