# zlib fork for ITK

This branch contains changes required to embed zlib into ITK. This includes
changes made primarily to the build system to allow it to be embedded into
another source tree as well as a header to facilitate mangling of the symbols
to avoid conflicts with other copies of the library within a single process.

  * Ignore whitespace problems for ITK's commit checks.
  * Integration with ITK's module system.
  * Mangle all exported symbols to have a `itkzlib_` prefix.
