# vxl/config/cmake/config/vxl_shared_link_test/CMakeLists.txt
#

cmake_minimum_required(VERSION 3.22.1...3.31.9)

project(vxl_pic_compatible)

add_library(cmTryCompileStaticLib STATIC static_src.cxx)
add_library(cmTryCompileSharedLib SHARED shared_src.cxx)
target_link_libraries(cmTryCompileSharedLib cmTryCompileStaticLib ${LINK_LIBRARIES})
