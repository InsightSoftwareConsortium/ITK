cmake_minimum_required (VERSION 3.10)
project (HDF5_CPP CXX)

add_subdirectory (src)

#-----------------------------------------------------------------------------
# Build the CPP Examples
#-----------------------------------------------------------------------------
if (HDF5_BUILD_EXAMPLES)
  add_subdirectory (examples)
endif ()

#-----------------------------------------------------------------------------
# Build the CPP unit tests
#-----------------------------------------------------------------------------
if (BUILD_TESTING)
  add_subdirectory (test)
endif ()
