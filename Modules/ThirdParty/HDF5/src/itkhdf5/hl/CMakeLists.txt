PROJECT (HDF5_HL C CXX)

#-----------------------------------------------------------------------------
# Apply Definitions to compiler in this directory and below
#-----------------------------------------------------------------------------
add_definitions (${HDF_EXTRA_C_FLAGS})


#-----------------------------------------------------------------------------
# List Source files
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_HL_SOURCE_DIR}/src )

add_subdirectory (${HDF5_HL_SOURCE_DIR}/src ${HDF5_HL_BINARY_DIR}/src)

#-- Build the High level Tools
if (HDF5_BUILD_TOOLS)
  add_subdirectory (${HDF5_HL_SOURCE_DIR}/tools ${HDF5_HL_BINARY_DIR}/tools)
endif (HDF5_BUILD_TOOLS)

#-- Add High Level Examples
if (HDF5_BUILD_EXAMPLES)
  add_subdirectory (${HDF5_HL_SOURCE_DIR}/examples ${HDF5_HL_BINARY_DIR}/examples)
endif (HDF5_BUILD_EXAMPLES)

#-- Build the Unit testing if requested
if (NOT HDF5_EXTERNALLY_CONFIGURED)
  if (BUILD_TESTING)
    add_subdirectory (${HDF5_HL_SOURCE_DIR}/test ${HDF5_HL_BINARY_DIR}/test)
  endif (BUILD_TESTING)
endif (NOT HDF5_EXTERNALLY_CONFIGURED)
