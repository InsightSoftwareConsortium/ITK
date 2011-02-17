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


#-----------------------------------------------------------------------------
#ITK requires special compiler flags on some platforms.
if(CMAKE_COMPILER_IS_GNUCXX)
 set(ITK_REQUIRED_C_FLAGS "${ITK_REQUIRED_C_FLAGS} -Wall -Wno-uninitialized -Wn o-unused-parameter")
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
   itkCHECK_CXX_ACCEPTS_FLAGS("-no-cpp-precomp" CXX_HAS_CPP_PRECOMP_FLAG)
   if(CXX_HAS_CPP_PRECOMP_FLAG)
     set(ITK_REQUIRED_C_FLAGS "${ITK_REQUIRED_C_FLAGS} -no-cpp-precomp")
     set(ITK_REQUIRED_CXX_FLAGS "${ITK_REQUIRED_CXX_FLAGS} -no-cpp-precomp")
   endif(CXX_HAS_CPP_PRECOMP_FLAG)
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
