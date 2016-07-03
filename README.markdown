[![Build Status](https://travis-ci.org/ppelleti/normalization-insensitive.svg?branch=master)](https://travis-ci.org/ppelleti/normalization-insensitive)

The module `Data.Unicode.NormalizationInsensitive` provides the `NI`
type constructor which can be parameterized by a string-like type like:
`String`, `ByteString`, `Text`, etc.. Comparisons of values of the resulting
type will be insensitive to normalization.

This is very similar in spirit to the `case-insensitive` package, and is
in fact based on the same code.

This package uses NFC internally, although that shouldn't matter to
you, unless you use `unsafeMk`.
