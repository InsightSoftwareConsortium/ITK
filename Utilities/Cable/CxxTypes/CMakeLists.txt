# Get the source list for the library.
INCLUDE(${CABLE_SOURCE_DIR}/CxxTypes/CMakeLists_Sources.txt)

INCLUDE_DIRECTORIES(${CABLE_SOURCE_DIR}/CxxTypes)

# Create a static CxxTypes library.
ADD_LIBRARY(CxxTypes STATIC CxxTypes_SRCS)
ADD_DEFINITIONS(-D_cxx_STATIC)

IF (NOT CABLE_NO_INSTALL)
  INSTALL_FILES(/share/Cable/include/CxxTypes ^cxx.*\\.h$)
ENDIF (NOT CABLE_NO_INSTALL)
