cmake_minimum_required (VERSION 3.10)
project (HDF5_HL_CPP CXX)

#-----------------------------------------------------------------------------
# Main HL lib is in /src
#-----------------------------------------------------------------------------
add_subdirectory (src)

# --------------------------------------------------------------------
#  Add in the examples for the Packet Table codes
# --------------------------------------------------------------------
if (HDF5_BUILD_EXAMPLES)
  add_subdirectory (examples)
endif ()

# --------------------------------------------------------------------
# Add in the unit tests for the packet table c++ wrapper
# --------------------------------------------------------------------

if (BUILD_TESTING)
  add_subdirectory (test)
endif ()
