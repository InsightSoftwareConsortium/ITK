cmake_minimum_required (VERSION 3.18)
project (HDF5_HL C)

#-----------------------------------------------------------------------------
# Option to use new-style references with dimension scale APIs
#-----------------------------------------------------------------------------
option (HDF5_DIMENSION_SCALES_NEW_REF  "Use new-style references with dimension scale APIs" OFF)
if (HDF5_DIMENSION_SCALES_NEW_REF)
  set (H5_DIMENSION_SCALES_WITH_NEW_REF 1)
endif ()

#-----------------------------------------------------------------------------
# List Source files
#-----------------------------------------------------------------------------

add_subdirectory (src)

# Build HDF5 Tools
if (HDF5_BUILD_TOOLS)
  add_subdirectory (tools)
endif ()

#-- Add High Level Examples
if (HDF5_BUILD_EXAMPLES)
  add_subdirectory (examples)
endif ()

#-- Build the Unit testing if requested
if (NOT HDF5_EXTERNALLY_CONFIGURED AND BUILD_TESTING AND HDF5_TEST_SERIAL)
  add_subdirectory (test)
endif ()
