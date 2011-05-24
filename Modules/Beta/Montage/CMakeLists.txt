cmake_minimum_required(VERSION 2.8)
if(COMMAND CMAKE_POLICY)
  cmake_policy(SET CMP0003 NEW)
endif(COMMAND CMAKE_POLICY)


project(SimpleImageReconstruction)

find_package(ITK REQUIRED)
include(${ITK_USE_FILE})

# if( NOT USE_FFTWD)
#  message(FATAL_ERROR "ITK Must be configured with USE_FFTWD ON")
# endif()

find_package(VTK REQUIRED)
include(${VTK_USE_FILE})

include(CMake/PlatformSpecificChecks.cmake)


OPTION(BUILD_SHARED_LIBS "Build with shared libraries." ${VTK_BUILD_SHARED_LIBS})
IF(BUILD_SHARED_LIBS)
  SET(SimpleImageReconstruction_BUILD_SHARED_LIBS_CONFIG 1)
ELSE(BUILD_SHARED_LIBS)
  SET(SimpleImageReconstruction_BUILD_SHARED_LIBS_CONFIG 0)
ENDIF(BUILD_SHARED_LIBS)


include(CTest)
include(CPack)

include_directories(${PROJECT_SOURCE_DIR}/Source/TileStitching)

add_subdirectory(Source)
add_subdirectory(Testing)
