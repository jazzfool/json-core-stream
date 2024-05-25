use serde::de;

use crate::de::{Deserializer, Error, Read, Result};

pub(crate) struct SeqAccess<'a, R, const N: usize> {
    first: bool,
    de: &'a mut Deserializer<R, N>,
}

impl<'a, 'de, R: Read<'de>, const N: usize> SeqAccess<'a, R, N> {
    pub fn new(de: &'a mut Deserializer<R, N>) -> Self {
        SeqAccess { de, first: true }
    }
}

impl<'a, 'de, R: Read<'de>, const N: usize> de::SeqAccess<'de> for SeqAccess<'a, R, N> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>>
    where
        T: de::DeserializeSeed<'de>,
    {
        let peek = match self
            .de
            .parse_whitespace()
            .ok_or(Error::EofWhileParsingList)?
        {
            b']' => return Ok(None),
            b',' => {
                self.de.eat_char();
                self.de
                    .parse_whitespace()
                    .ok_or(Error::EofWhileParsingValue)?
            }
            c => {
                if self.first {
                    self.first = false;
                    c
                } else {
                    return Err(Error::ExpectedListCommaOrEnd);
                }
            }
        };

        if peek == b']' {
            Err(Error::TrailingComma)
        } else {
            Ok(Some(seed.deserialize(&mut *self.de)?))
        }
    }
}
