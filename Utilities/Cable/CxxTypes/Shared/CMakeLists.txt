# Get the source list for the library.
INCLUDE(${CABLE_SOURCE_DIR}/CxxTypes/CMakeLists_Sources.txt)

# Create a shared CxxTypes library.
ADD_LIBRARY(CxxTypes SHARED CxxTypes_SRCS)
