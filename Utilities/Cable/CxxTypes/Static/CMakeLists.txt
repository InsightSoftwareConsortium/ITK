# Get the source list for the library.
INCLUDE(${CABLE_SOURCE_DIR}/CxxTypes/CMakeLists_Sources.txt)

# Create a static CxxTypes library.
ADD_LIBRARY(CxxTypesStatic STATIC CxxTypes_SRCS)
