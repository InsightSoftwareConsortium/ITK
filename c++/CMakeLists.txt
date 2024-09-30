cmake_minimum_required (VERSION 3.18)
project (HDF5_CPP CXX)

add_subdirectory (src)

#-----------------------------------------------------------------------------
# Build the CPP unit tests
#-----------------------------------------------------------------------------
if (NOT HDF5_EXTERNALLY_CONFIGURED AND BUILD_TESTING)
  add_subdirectory (test)
endif ()
