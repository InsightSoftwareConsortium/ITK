cmake_minimum_required(VERSION 2.8)
if(COMMAND CMAKE_POLICY)
  cmake_policy(SET CMP0003 NEW)
endif(COMMAND CMAKE_POLICY)


project(SimpleImageReconstruction)

find_package(ITK REQUIRED)
include(${ITK_USE_FILE})

set(REQUIRED_ITK_VERSION "4.0.0")
if("${ITK_VERSION}" VERSION_LESS "${REQUIRED_ITK_VERSION}")
  message(FATAL_ERROR "ITK Version ${REQUIRED_ITK_VERSION} or greater is required.")
endif()

OPTION(BUILD_SHARED_LIBS "Build with shared libraries." ${ITK_BUILD_SHARED_LIBS})
IF(BUILD_SHARED_LIBS)
  SET(SimpleImageReconstruction_BUILD_SHARED_LIBS_CONFIG 1)
ELSE(BUILD_SHARED_LIBS)
  SET(SimpleImageReconstruction_BUILD_SHARED_LIBS_CONFIG 0)
ENDIF(BUILD_SHARED_LIBS)


include(CTest)
include(CPack)

include_directories(
  ${PROJECT_SOURCE_DIR}/Source/TileStitching
  ${PROJECT_BINARY_DIR}/Source/TileStitching
  )

add_subdirectory(Source)
