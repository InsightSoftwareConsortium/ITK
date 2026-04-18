# The header files
set(HEADERS
  itkCuberilleImageToMeshFilter.h
)

# The implementation files
set(SOURCES
  itkCuberilleImageToMeshFilter.txx
  examples.cxx
)

# Add this as include directory
include_directories(
  ${CMAKE_SOURCE_DIR}
  ${SOURCE_PATH}
)

# Main library
add_executable(Examples ${HEADERS} ${SOURCES})
target_link_libraries(Examples ${ITK_LIBRARIES} ITKQuadEdgeMesh)
