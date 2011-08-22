# The header files
SET(HEADERS
  itkCuberilleImageToMeshFilter.h
)

# The implementation files
SET(SOURCES
  itkCuberilleImageToMeshFilter.txx
  examples.cxx
)

# Add this as include directory
INCLUDE_DIRECTORIES(
  ${CMAKE_SOURCE_DIR}
  ${SOURCE_PATH}
)

# Main library
ADD_EXECUTABLE(Examples ${HEADERS} ${SOURCES})
TARGET_LINK_LIBRARIES(Examples ${ITK_LIBRARIES} ITKQuadEdgeMesh)
