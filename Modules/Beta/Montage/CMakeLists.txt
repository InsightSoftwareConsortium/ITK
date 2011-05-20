cmake_minimum_required(VERSION 2.8)
if(COMMAND CMAKE_POLICY)
  cmake_policy(SET CMP0003 NEW)
endif(COMMAND CMAKE_POLICY)


project(SimpleImageReconstruction)

find_package(ITK REQUIRED)
include(${ITK_USE_FILE})

include(CMake/PlatformSpecificChecks.cmake)

include(CTest)
include(CPack)

add_subdirectory(Source)
add_subdirectory(Testing)
