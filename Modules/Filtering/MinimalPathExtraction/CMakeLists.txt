project(MinimalPath)

set(
  LIBRARY_OUTPUT_PATH
  ${CMAKE_BINARY_DIR}/bin
  CACHE PATH
  "Single output directory for building all libraries."
  FORCE
)
set(
  EXECUTABLE_OUTPUT_PATH
  ${CMAKE_BINARY_DIR}/bin
  CACHE PATH
  "Single output directory for building all executables."
  FORCE
)

find_package(ITK REQUIRED)
include(${ITK_USE_FILE})

include_directories(${CMAKE_SOURCE_DIR}/include)

add_subdirectory(examples)

include(CTest)

if(BUILD_TESTING)
  add_subdirectory(test)
endif(BUILD_TESTING)
