# `json-core-stream`

no-std, no alloc

A fork of [`serde-json-core`](https://github.com/rust-embedded-community/serde-json-core) with changes to support de/serialization from/to a stream of bytes.

Things it does differently:

- Deserializer reads bytes from a `Read` trait instance.
- Deserializer has an internal buffer (size can be specified as a const generic).
- No longer supports deserializing into borrowed strings; can only deserialize strings into `heapless::String` now.

The existing methods to de/serialize to/from a slice/str remain,
however there is now a public `Read` trait that can be used with `from_stream`,
and a public `Write` trait that can be used with `to_stream`.

## License

Licensed under either of

- Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or
  http://www.apache.org/licenses/LICENSE-2.0)
- MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
