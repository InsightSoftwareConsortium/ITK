HDF5
====

The `itkhdf5` subdirectory contains a reduced distribution of the hdf5 source
tree with only the library source code and [CMake] build system. It is not a
submodule; the actual content is part of our source tree and changes can be
made and committed directly.

Update hdf5 from upstream using the `UpdateFromUpstream.sh` script one directory up.

Finally, update symbol mangling according to the instructions in `itk_hdf5_mangle.h`.

[CMake]: https://cmake.org/
