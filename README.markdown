Latest:
[![Hackage](https://img.shields.io/hackage/v/normalization-insensitive.svg)](https://hackage.haskell.org/package/normalization-insensitive)
Linux:
[![Build Status](https://travis-ci.org/ppelleti/normalization-insensitive.svg?branch=master)](https://travis-ci.org/ppelleti/normalization-insensitive)
Windows:
[![Build status](https://ci.appveyor.com/api/projects/status/0qhwhc1cfsphf263/branch/master?svg=true)](https://ci.appveyor.com/project/ppelleti/normalization-insensitive/branch/master)

The module `Data.Unicode.NormalizationInsensitive` provides the `NI`
type constructor which can be parameterized by a string-like type like:
`String`, `ByteString`, `Text`, etc.. Comparisons of values of the resulting
type will be insensitive to normalization.

This is very similar in spirit to the [`case-insensitive`][1] package, and is
in fact based on the same code.

This package uses [NFC][2] internally, although that shouldn't matter to
you, unless you use `unsafeMk`.

[1]: https://hackage.haskell.org/package/case-insensitive
[2]: https://en.wikipedia.org/wiki/Unicode_equivalence#Normal_forms
