#[[ ITK --start
cmake_minimum_required (VERSION 3.12)
# ITK --stop ]]
project (HDF5_HL C)

#-----------------------------------------------------------------------------
# List Source files
#-----------------------------------------------------------------------------

add_subdirectory (src)

#[[ ITK --start
#-- Build the High level Tools
if (HDF5_BUILD_TOOLS)
  add_subdirectory (tools)
endif ()

#-- Add High Level Examples
if (HDF5_BUILD_EXAMPLES)
  add_subdirectory (examples)
endif ()
#-- Build the Unit testing if requested
if (BUILD_TESTING AND HDF5_TEST_SERIAL)
  add_subdirectory (test)
endif ()
# ITK --stop ]]
