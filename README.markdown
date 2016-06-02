The module `Data.Unicode.NormalizationInsensitive` provides the `NI`
type constructor which can be parameterized by a string-like type like:
`String`, `ByteString`, `Text`, etc.. Comparisons of values of the resulting
type will be insensitive to normalization.
