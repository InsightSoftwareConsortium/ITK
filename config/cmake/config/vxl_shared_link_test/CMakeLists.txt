# vxl/config/cmake/config/vxl_shared_link_test/CMakeLists.txt
#
cmake_minimum_required(VERSION 2.8.9 FATAL_ERROR)
if(POLICY CMP0025)
  cmake_policy(SET CMP0025 NEW)
endif()
if(POLICY CMP0042)
  cmake_policy(SET CMP0042 NEW)
endif()
project(vxl_pic_compatible)

add_library(cmTryCompileStaticLib STATIC static_src.cxx)
add_library(cmTryCompileSharedLib SHARED shared_src.cxx)
target_link_libraries(cmTryCompileSharedLib cmTryCompileStaticLib ${LINK_LIBRARIES})
