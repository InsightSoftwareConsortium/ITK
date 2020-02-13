# Eigen fork for ITK

This branch contains changes required to embed Eigen into ITK. This
 includes changes made primarily to the build system to allow it to be embedded
 into another source tree as well as a header to facilitate mangling of the
 symbols to avoid conflicts with other copies of the library within a single
 process.

* Add `.gitattributes` to avoid failures in ITK's content checks.
* Remove executable permissions from include files.
* Integrate the CMake build with ITK's module system.
<!-- * Mangle all exported symbols to live in a `itkeigen` namespace. -->
