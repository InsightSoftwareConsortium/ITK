# vxl/config/cmake/config/vxl_shared_link_test/CMakeLists.txt
#
cmake_minimum_required(VERSION 3.10.2 FATAL_ERROR)
# Set policies consistent with newer versions of cmake
# to ease integration with projects that require newer
# cmake versions.

foreach(p
    ## Only policies introduced after the cmake_minimum_required
    ## version need to explicitly be set to NEW.

    ##----- Policies Introduced by CMake 3.10¶
    CMP0071  #: Let AUTOMOC and AUTOUIC process GENERATED files.
    CMP0070  #: Define file(GENERATE) behavior for relative paths.
    )
  if(POLICY ${p})
    cmake_policy(SET ${p} NEW)
  endif()
endforeach()
project(vxl_pic_compatible)

add_library(cmTryCompileStaticLib STATIC static_src.cxx)
add_library(cmTryCompileSharedLib SHARED shared_src.cxx)
target_link_libraries(cmTryCompileSharedLib cmTryCompileStaticLib ${LINK_LIBRARIES})
