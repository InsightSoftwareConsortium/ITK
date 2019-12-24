#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
#
#
# This file provides functions for C++ support.
#
#-------------------------------------------------------------------------------
ENABLE_LANGUAGE (CXX)
set (HDF_PREFIX "H5")

#-------------------------------------------------------------------------------
#  Fix CXX flags if we are compiling staticly on Windows using
#  Windows_MT.cmake from config/cmake/UserMacros
#-------------------------------------------------------------------------------
if (BUILD_STATIC_CRT_LIBS)
  TARGET_STATIC_CRT_FLAGS ()
endif ()

#-----------------------------------------------------------------------------
# Configure Checks which require CXX compilation must go in here
# not in the main ConfigureChecks.cmake files, because if the user has
# no CXX compiler, problems arise.
#-----------------------------------------------------------------------------
include (CheckIncludeFileCXX)
include (TestForSTDNamespace)

# IF the c compiler found stdint, check the C++ as well. On some systems this
# file will be found by C but not C++, only do this test IF the C++ compiler
# has been initialized (e.g. the project also includes some c++)
if (${HDF_PREFIX}_HAVE_STDINT_H AND CMAKE_CXX_COMPILER_LOADED)
  CHECK_INCLUDE_FILE_CXX ("stdint.h" ${HDF_PREFIX}_HAVE_STDINT_H_CXX)
  if (NOT ${HDF_PREFIX}_HAVE_STDINT_H_CXX)
    set (${HDF_PREFIX}_HAVE_STDINT_H "" CACHE INTERNAL "Have includes HAVE_STDINT_H")
    set (USE_INCLUDES ${USE_INCLUDES} "stdint.h")
  endif ()
endif ()

# For other CXX specific tests, use this MACRO.
macro (HDF_CXX_FUNCTION_TEST OTHER_TEST)
  if (NOT DEFINED ${OTHER_TEST})
    set (MACRO_CHECK_FUNCTION_DEFINITIONS "-D${OTHER_TEST} ${CMAKE_REQUIRED_FLAGS}")
    set (OTHER_TEST_ADD_LIBRARIES)
    if (HDF5_REQUIRED_LIBRARIES)
      set (OTHER_TEST_ADD_LIBRARIES "-DLINK_LIBRARIES:STRING=${HDF5_REQUIRED_LIBRARIES}")
    endif ()

    foreach (def
        HAVE_SYS_TIME_H
        HAVE_UNISTD_H
        HAVE_SYS_TYPES_H
        HAVE_SYS_SOCKET_H
        HAVE_SYS_FILE_H
    )
      if ("${${HDF_PREFIX}_${def}}")
        set (MACRO_CHECK_FUNCTION_DEFINITIONS "${MACRO_CHECK_FUNCTION_DEFINITIONS} -D${def}")
      endif ()
    endforeach ()

    if (LARGEFILE)
      set (MACRO_CHECK_FUNCTION_DEFINITIONS
          "${MACRO_CHECK_FUNCTION_DEFINITIONS} -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -D_LARGEFILE_SOURCE"
      )
    endif ()

    #message (STATUS "Performing ${OTHER_TEST}")
    TRY_COMPILE (${OTHER_TEST}
        ${CMAKE_BINARY_DIR}
        ${HDF_RESOURCES_EXT_DIR}/HDFCXXTests.cpp
        CMAKE_FLAGS -DCOMPILE_DEFINITIONS:STRING=${MACRO_CHECK_FUNCTION_DEFINITIONS}
        "${OTHER_TEST_ADD_LIBRARIES}"
        OUTPUT_VARIABLE OUTPUT
    )
    if (${OTHER_TEST} EQUAL 0)
      set (${OTHER_TEST} 1 CACHE INTERNAL "CXX test ${FUNCTION}")
      message (STATUS "Performing CXX Test ${OTHER_TEST} - Success")
    else ()
      message (STATUS "Performing CXX Test ${OTHER_TEST} - Failed")
      set (${OTHER_TEST} "" CACHE INTERNAL "CXX test ${FUNCTION}")
      file (APPEND ${CMAKE_BINARY_DIR}/CMakeFiles/CMakeError.log
          "Performing CXX Test ${OTHER_TEST} failed with the following output:\n"
          "${OUTPUT}\n"
      )
    endif ()
  endif ()
endmacro ()

#-----------------------------------------------------------------------------
# Check a bunch of cxx functions
#-----------------------------------------------------------------------------
if (CMAKE_CXX_COMPILER_LOADED)
  foreach (cxx_test
      OLD_HEADER_FILENAME
      HDF_NO_NAMESPACE
      HDF_NO_STD
      BOOL_NOTDEFINED
      NO_STATIC_CAST
      CXX_HAVE_OFFSETOF
  )
    HDF_CXX_FUNCTION_TEST (${cxx_test})
  endforeach ()
endif ()
