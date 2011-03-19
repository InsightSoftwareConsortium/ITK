# On Visual Studio 8 MS deprecated C. This removes all 1.276E1265 security
# warnings
if(WIN32)
   if(NOT CYGWIN)
     if(NOT MINGW)
       if(NOT ITK_ENABLE_VISUAL_STUDIO_DEPRECATED_C_WARNINGS)
         add_definitions(
           -D_CRT_FAR_MAPPINGS_NO_DEPRECATE
           -D_CRT_IS_WCTYPE_NO_DEPRECATE
           -D_CRT_MANAGED_FP_NO_DEPRECATE
           -D_CRT_NONSTDC_NO_DEPRECATE
           -D_CRT_SECURE_NO_DEPRECATE
           -D_CRT_SECURE_NO_DEPRECATE_GLOBALS
           -D_CRT_SETERRORMODE_BEEP_SLEEP_NO_DEPRECATE
           -D_CRT_TIME_FUNCTIONS_NO_DEPRECATE
           -D_CRT_VCCLRIT_NO_DEPRECATE
           -D_SCL_SECURE_NO_DEPRECATE
           )
       endif(NOT ITK_ENABLE_VISUAL_STUDIO_DEPRECATED_C_WARNINGS)
     endif(NOT MINGW)
   endif(NOT CYGWIN)
endif(WIN32)

if(WIN32)
  # Some libraries (e.g. vxl libs) have no dllexport markup, so we can
  # build full shared libraries only with the GNU toolchain. For non
  # gnu compilers on windows, only Common is shared.  This allows for
  # plugin type applications to use a dll for ITKCommon which will contain
  # the static for Modified time.
  if(CMAKE_COMPILER_IS_GNUCXX)
    # CMake adds --enable-all-exports on Cygwin (since Cygwin is
    # supposed to be UNIX-like), but we need to add it explicitly for
    # a native windows build with the MinGW tools.
    if(MINGW)
      set(CMAKE_SHARED_LIBRARY_CREATE_C_FLAGS
        "-shared -Wl,--export-all-symbols -Wl,--enable-auto-import")
      set(CMAKE_SHARED_LIBRARY_CREATE_CXX_FLAGS
        "-shared -Wl,--export-all-symbols -Wl,--enable-auto-import")
      set(CMAKE_EXE_LINKER_FLAGS "-Wl,--enable-auto-import")
    endif(MINGW)
    if(CYGWIN)
      set(CMAKE_EXE_LINKER_FLAGS "-Wl,--enable-auto-import")
    endif(CYGWIN)
  else(CMAKE_COMPILER_IS_GNUCXX)
   if(BUILD_SHARED_LIBS)
     set(ITK_LIBRARY_BUILD_TYPE "SHARED")
   else(BUILD_SHARED_LIBS)
     set(ITK_LIBRARY_BUILD_TYPE "STATIC")
   endif(BUILD_SHARED_LIBS)
   set(BUILD_SHARED_LIBS OFF)
  endif(CMAKE_COMPILER_IS_GNUCXX)
endif(WIN32)

#-----------------------------------------------------------------------------
# Find platform-specific differences in the handling of IEEE floating point
# special values.
include(${ITK_SOURCE_DIR}/CMake/CheckBigBitfield.cmake)
CHECK_BIG_BITFIELD(BIGBITFIELD_VALUE ${ITK_SOURCE_DIR}/CMake)
if(BIGBITFIELD_VALUE)
   set(BIGBITFIELD 1 CACHE INTERNAL "System handles bit-fields larger than 32 bits.")
else(BIGBITFIELD_VALUE)
   set(BIGBITFIELD 0 CACHE INTERNAL "System handles bit-fields larger than 32 bits.")
endif(BIGBITFIELD_VALUE)

include(${ITK_SOURCE_DIR}/CMake/TestQnanhibit.cmake)
TEST_QNANHIBIT(QNANHIBIT_VALUE ${ITK_SOURCE_DIR}/CMake)
if(QNANHIBIT_VALUE)
   set(QNANHIBIT 1 CACHE INTERNAL "The 22nd bit of 32-bit floating-point quiet NaN.")
else(QNANHIBIT_VALUE)
   set(QNANHIBIT 0 CACHE INTERNAL "The 22nd bit of 32-bit floating-point quiet NaN.")
endif(QNANHIBIT_VALUE)



#-----------------------------------------------------------------------------
#ITK requires special compiler flags on some platforms.
if(CMAKE_COMPILER_IS_GNUCXX)
 set(ITK_REQUIRED_C_FLAGS "${ITK_REQUIRED_C_FLAGS} -Wall -Wno-uninitialized -Wno-unused-parameter")
 set(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -ftemplate-depth-50 -Wall")
 include(${ITK_SOURCE_DIR}/CMake/itkCheckCXXAcceptsFlags.cmake)
 itkCHECK_CXX_ACCEPTS_FLAGS("-Wno-deprecated" CXX_HAS_DEPRECATED_FLAG)
 if(CXX_HAS_DEPRECATED_FLAG)
   set(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -Wno-deprecated")
 endif(CXX_HAS_DEPRECATED_FLAG)
 if(APPLE)
   # -no-cpp-precomp and -Wno-long-double were compiler flags present
   # only in Apple's gcc and not in the FSF gcc. The flags are obsolete
   # and totally removed in gcc 4.2 and later. I believe they are only
   # needed with gcc 3.3 and earlier.
   execute_process(COMMAND "${CMAKE_C_COMPILER}" --version
     OUTPUT_VARIABLE _version ERROR_VARIABLE _version)
   if("${_version}" MATCHES "gcc.*3\\.3.*Apple")
     set(ITK_REQUIRED_C_FLAGS "${ITK_REQUIRED_C_FLAGS} -no-cpp-precomp")
     set(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -no-cpp-precomp")
   endif()
   itkCHECK_CXX_ACCEPTS_FLAGS("-Wno-long-double" CXX_HAS_LONGDOUBLE_FLAG)
   if(CXX_HAS_LONGDOUBLE_FLAG)
     set(ITK_REQUIRED_C_FLAGS "${ITK_REQUIRED_C_FLAGS} -Wno-long-double")
     set(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -Wno-long-double")
   endif(CXX_HAS_LONGDOUBLE_FLAG)

   option(ITK_USE_64BITS_APPLE_TRUNCATION_WARNING "Turn on warnings on 64bits to 32bits truncations." OFF)
   mark_as_advanced(ITK_USE_64BITS_APPLE_TRUNCATION_WARNING)
 endif(APPLE)

 # gcc must have -msse2 option to enable sse2 support
 if(VNL_CONFIG_ENABLE_SSE2 OR VNL_CONFIG_ENABLE_SSE2_ROUNDING)
   set(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -msse2")
 endif(VNL_CONFIG_ENABLE_SSE2 OR VNL_CONFIG_ENABLE_SSE2_ROUNDING)
endif(CMAKE_COMPILER_IS_GNUCXX)


#---------------------------------------------------------------
# run try compiles and tests for ITK
include(CMake/itkTestFriendTemplatedFunction.cmake)
