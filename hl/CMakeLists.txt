cmake_minimum_required (VERSION 3.12)
project (HDF5_HL C)

#-----------------------------------------------------------------------------
# List Source files
#-----------------------------------------------------------------------------

add_subdirectory (src)

# Build HDF5 Tools
if (HDF5_BUILD_TOOLS)
  #-----------------------------------------------------------------------------
  #-- Option to build the High level Tools
  #-----------------------------------------------------------------------------
  if (EXISTS "${HDF5_HL_SOURCE_DIR}/tools" AND IS_DIRECTORY "${HDF5_HL_SOURCE_DIR}/tools")
    option (HDF5_BUILD_HL_TOOLS  "Build HDF5 HL Tools" ON)
    if (HDF5_BUILD_HL_TOOLS)
      add_subdirectory (tools)
    endif ()
  endif ()
endif ()

#-- Add High Level Examples
if (HDF5_BUILD_EXAMPLES)
  add_subdirectory (examples)
endif ()

#-- Build the Unit testing if requested
if (BUILD_TESTING AND HDF5_TEST_SERIAL)
  add_subdirectory (test)
endif ()
