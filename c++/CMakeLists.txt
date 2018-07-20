cmake_minimum_required (VERSION 3.10)
PROJECT (HDF5_CPP)

#-----------------------------------------------------------------------------
# Apply Definitions to compiler in this directory and below
#-----------------------------------------------------------------------------
add_definitions (${HDF_EXTRA_C_FLAGS})

#-----------------------------------------------------------------------------
# Generate configure file
#-----------------------------------------------------------------------------
configure_file (${HDF_RESOURCES_DIR}/H5cxx_config.h.in
                ${HDF5_BINARY_DIR}/H5cxx_pubconf.h
)

#-----------------------------------------------------------------------------
# Setup Include directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_CPP_SOURCE_DIR}/src)
INCLUDE_DIRECTORIES (${HDF5_BINARY_DIR})

#-----------------------------------------------------------------------------
# Parallel/MPI, prevent spurious cpp/cxx warnings
#-----------------------------------------------------------------------------
if (H5_HAVE_PARALLEL)
  add_definitions ("-DMPICH_SKIP_MPICXX")
  add_definitions ("-DMPICH_IGNORE_CXX_SEEK")
endif ()

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
