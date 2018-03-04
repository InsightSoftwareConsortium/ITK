# vxl/config/cmake/config/vxl_shared_link_test/CMakeLists.txt
#
cmake_minimum_required(VERSION 3.3.0 FATAL_ERROR)
# Set policies consistent with newer versions of cmake
# to ease integration with projects that require newer
# cmake versions.

foreach(p
    ## Only policies introduced after the cmake_minimum_required
    ## version need to explicitly be set to NEW.

    ##----- Policies Introduced by CMake 3.10¶
    CMP0071  #: Let AUTOMOC and AUTOUIC process GENERATED files.
    CMP0070  #: Define file(GENERATE) behavior for relative paths.
    ##----- Policies Introduced by CMake 3.9
    CMP0069  #: INTERPROCEDURAL_OPTIMIZATION is enforced when enabled.
    CMP0068  #: RPATH settings on macOS do not affect install_name.
    ##----- Policies Introduced by CMake 3.8
    CMP0067  #: Honor language standard in try_compile() source-file signature.
    ##----- Policies Introduced by CMake 3.7
    CMP0066  #: Honor per-config flags in try_compile() source-file signature.
    ##----- Policies Introduced by CMake 3.4
    CMP0065  #: Do not add flags to export symbols from executables without the ENABLE_EXPORTS target property.
    CMP0064  #: Support new TEST if() operator.
    )
  if(POLICY ${p})
    cmake_policy(SET ${p} NEW)
  endif()
endforeach()
project(vxl_pic_compatible)

add_library(cmTryCompileStaticLib STATIC static_src.cxx)
add_library(cmTryCompileSharedLib SHARED shared_src.cxx)
target_link_libraries(cmTryCompileSharedLib cmTryCompileStaticLib ${LINK_LIBRARIES})
