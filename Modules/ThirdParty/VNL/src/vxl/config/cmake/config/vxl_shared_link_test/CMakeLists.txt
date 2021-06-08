# vxl/config/cmake/config/vxl_shared_link_test/CMakeLists.txt
#

# Set policies consistent with newer versions of cmake
# to ease integration with projects that require newer
# cmake versions.
cmake_minimum_required(VERSION 3.16.3)
if(CMAKE_VERSION VERSION_LESS 3.12.0)
  cmake_policy(VERSION ${CMAKE_VERSION})
else()
  cmake_policy(VERSION 3.10.2...3.13.2)
endif()

project(vxl_pic_compatible)

add_library(cmTryCompileStaticLib STATIC static_src.cxx)
add_library(cmTryCompileSharedLib SHARED shared_src.cxx)
target_link_libraries(cmTryCompileSharedLib cmTryCompileStaticLib ${LINK_LIBRARIES})
