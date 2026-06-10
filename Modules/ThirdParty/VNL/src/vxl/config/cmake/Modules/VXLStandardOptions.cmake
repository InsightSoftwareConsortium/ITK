# vxl/config/cmake/VXLStandardOptions.cmake
#
# This CMake module is included by vxl/CMakeLists.txt.  It adds
# several vxl-standard testing and build options to the project:
#
#  BUILD_SHARED_LIBS
#  BUILD_TESTING
#

include(CTest)

if( WIN32 )
  option( BUILD_SHARED_LIBS "Build shared libraries." OFF)
  if (BUILD_SHARED_LIBS)
    # On windows, we need to export symbols for shared builds
    set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS "TRUE")
  endif()
else()
  option( BUILD_SHARED_LIBS "Build shared libraries." OFF)
endif()
set( BUILD_SHARED_LIBS ${BUILD_SHARED_LIBS} )
mark_as_advanced(BUILD_SHARED_LIBS)

# Taken from ITK build environment
# On Visual Studio 8 MS deprecated C. This removes many security warnings
if(WIN32)
  if(NOT MINGW)
    add_compile_definitions(
      _CRT_FAR_MAPPINGS_NO_DEPRECATE
      _CRT_IS_WCTYPE_NO_DEPRECATE
      _CRT_MANAGED_FP_NO_DEPRECATE
      _CRT_NONSTDC_NO_DEPRECATE
      _CRT_SECURE_NO_DEPRECATE
      _CRT_SECURE_NO_DEPRECATE_GLOBALS
      _CRT_SETERRORMODE_BEEP_SLEEP_NO_DEPRECATE
      _CRT_TIME_FUNCTIONS_NO_DEPRECATE
      _CRT_VCCLRIT_NO_DEPRECATE
      _SCL_SECURE_NO_DEPRECATE
      )
  endif()
endif()
