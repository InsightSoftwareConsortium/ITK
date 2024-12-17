#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://www.hdfgroup.org/licenses.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
#
ENABLE_LANGUAGE (CXX)

set (CMAKE_CXX_STANDARD 11)
set (CMAKE_CXX_STANDARD_REQUIRED TRUE)

set (CMAKE_CXX_EXTENSIONS OFF)

set (CMAKE_CXX_FLAGS "${CMAKE_CXX_SANITIZER_FLAGS} ${CMAKE_CXX_FLAGS}")
message (VERBOSE "Warnings Configuration: CXX default: ${CMAKE_CXX_FLAGS}")
#-----------------------------------------------------------------------------
# Compiler specific flags : Shouldn't there be compiler tests for these
#-----------------------------------------------------------------------------
if (WIN32 AND (CMAKE_CXX_COMPILER_ID STREQUAL "Intel" OR CMAKE_CXX_COMPILER_ID MATCHES "IntelLLVM"))
  set (_INTEL_WINDOWS 1)
endif ()

if (WIN32 AND CMAKE_CXX_COMPILER_ID MATCHES "[Cc]lang" AND "x${CMAKE_CXX_SIMULATE_ID}" STREQUAL "xMSVC")
  set (_CLANG_MSVC_WINDOWS 1)
endif()

# MSVC 14.28 enables C5105, but the Windows SDK 10.0.18362.0 triggers it.
if ((_CLANG_MSVC_WINDOWS OR CMAKE_CXX_COMPILER_ID STREQUAL "MSVC") AND CMAKE_CXX_COMPILER_LOADED)
  set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /EHsc")
  if (CMAKE_CXX_COMPILER_ID STREQUAL "MSVC" AND NOT CMAKE_CXX_COMPILER_VERSION VERSION_LESS 19.28)
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -wd5105")
  endif ()
endif ()

if (CMAKE_CXX_COMPILER_ID STREQUAL SunPro AND CMAKE_CXX_COMPILER_LOADED)
  if (NOT DEFINED CMAKE_CXX${CMAKE_CXX_STANDARD}_STANDARD_COMPILE_OPTION)
    if (NOT CMAKE_CXX_COMPILER_VERSION VERSION_LESS 5.13)
      if (NOT CMAKE_CXX_STANDARD OR CMAKE_CXX_STANDARD EQUAL 98)
        set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++03")
      endif ()
    else ()
      set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -library=stlport4")
    endif ()
  endif ()
endif ()

if (CMAKE_CXX_COMPILER_ID STREQUAL "NVHPC" AND CMAKE_CXX_COMPILER_LOADED)
  if (NOT DEFINED CMAKE_CXX${CMAKE_CXX_STANDARD}_STANDARD_COMPILE_OPTION)
    if (NOT CMAKE_CXX_STANDARD OR CMAKE_CXX_STANDARD EQUAL 11)
      set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${CMAKE_C11_STANDARD_COMPILE_OPTION}")
    endif ()
  endif ()
  if (NOT ${HDF_CFG_NAME} MATCHES "Debug" AND NOT ${HDF_CFG_NAME} MATCHES "Developer")
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Minform=warn")
    if (NOT ${HDF_CFG_NAME} MATCHES "RelWithDebInfo")
      set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -s")
    endif ()
  else ()
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Mbounds -gopt -g")
  endif ()
endif ()

if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU" AND CMAKE_CXX_COMPILER_LOADED)
  set (CMAKE_CXX_FLAGS "${CMAKE_ANSI_CFLAGS} ${CMAKE_CXX_FLAGS}")
  if (${HDF_CFG_NAME} MATCHES "Debug" OR ${HDF_CFG_NAME} MATCHES "Developer")
    if (NOT CMAKE_CXX_COMPILER_VERSION VERSION_LESS 5.0)
      set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Og -ftrapv -fno-common")
    endif ()
  else ()
    if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU" AND NOT CMAKE_CXX_COMPILER_VERSION VERSION_LESS 5.0)
      set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fstdarg-opt")
    endif ()
    if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU" AND NOT CMAKE_CXX_COMPILER_VERSION VERSION_LESS 10.0)
      if (HDF5_ENABLE_BUILD_DIAGS)
        message (STATUS "... default color and URL extended diagnostic messages enabled")
      else ()
        message (STATUS "... disable color and URL extended diagnostic messages")
        set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fdiagnostics-urls=never -fno-diagnostics-color")
      endif ()
    endif ()
  endif ()
endif ()

#-----------------------------------------------------------------------------
# Option to allow the user to disable compiler warnings
#-----------------------------------------------------------------------------
if (HDF5_DISABLE_COMPILER_WARNINGS)
  message (STATUS "....Compiler warnings are suppressed")
  # MSVC uses /w to suppress warnings.  It also complains if another
  # warning level is given, so remove it.
  if (MSVC)
    set (HDF5_WARNINGS_BLOCKED 1)
    if (CMAKE_CXX_COMPILER_LOADED)
      string (REGEX REPLACE "(^| )([/-])W[0-9]( |$)" " " CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
      set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /W0")
    endif ()
  endif ()
  if (WIN32)
    add_definitions (-D_CRT_SECURE_NO_WARNINGS)
  endif ()

  # Most compilers use -w to suppress warnings.
  if (NOT HDF5_WARNINGS_BLOCKED)
    if (CMAKE_CXX_COMPILER_LOADED)
      set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -w")
    endif ()
  endif ()
endif ()

#-----------------------------------------------------------------------------
# HDF5 library compile options - to be made available to all targets
#-----------------------------------------------------------------------------

if (${CMAKE_SYSTEM_NAME} MATCHES "SunOS")
  list (APPEND HDF5_CMAKE_CXX_FLAGS "-erroff=%none -DBSD_COMP")
else ()
  # General flags
  #
  # Note that some of the flags listed here really should be developer
  # flags (listed in a separate variable, below) but we put them here
  # because they are not raised by the current code and we'd like to
  # know if they do start showing up.
  #
  # NOTE: Don't add -Wpadded here since we can't/won't fix the (many)
  # warnings that are emitted. If you need it, add it at configure time.
  if (CMAKE_CXX_COMPILER_ID STREQUAL "Intel")
    if (_INTEL_WINDOWS)
      ADD_H5_FLAGS (HDF5_CMAKE_CXX_FLAGS "${HDF5_SOURCE_DIR}/config/intel-warnings/classic/win-general")
    else ()
      ADD_H5_FLAGS (HDF5_CMAKE_CXX_FLAGS "${HDF5_SOURCE_DIR}/config/intel-warnings/classic/general")
    endif()
    if (NOT _INTEL_WINDOWS)
      if(NOT CMAKE_CXX_COMPILER_VERSION VERSION_LESS 15.0)
        ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/intel-warnings/classic/15")
      endif()
      if(NOT CMAKE_CXX_COMPILER_VERSION VERSION_LESS 18.0)
        ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/intel-warnings/classic/18")
      endif()
    endif()
  elseif (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU" AND CMAKE_CXX_COMPILER_LOADED
        AND CMAKE_CXX_COMPILER_VERSION VERSION_GREATER_EQUAL 4.8)
      # add the general CXX flags for g++ compiler versions 4.8 and above.
      ADD_H5_FLAGS (HDF5_CMAKE_CXX_FLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/cxx-general")
      ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/cxx-error-general")
    endif ()
  elseif (CMAKE_CXX_COMPILER_ID MATCHES "IntelLLVM")
    if (_INTEL_WINDOWS)
      ADD_H5_FLAGS (HDF5_CMAKE_CXX_FLAGS "${HDF5_SOURCE_DIR}/config/intel-warnings/oneapi/win-general")
    else ()
      ADD_H5_FLAGS (HDF5_CMAKE_CXX_FLAGS "${HDF5_SOURCE_DIR}/config/intel-warnings/oneapi/general")
    endif()
  elseif (CMAKE_CXX_COMPILER_ID MATCHES "[Cc]lang")
    ADD_H5_FLAGS (HDF5_CMAKE_CXX_FLAGS "${HDF5_SOURCE_DIR}/config/clang-warnings/general")
  elseif (CMAKE_CXX_COMPILER_ID STREQUAL "PGI")
    list (APPEND HDF5_CMAKE_CXX_FLAGS "-Minform=inform")
  endif ()
  message (VERBOSE "CMAKE_CXX_FLAGS_GENERAL=${HDF5_CMAKE_CXX_FLAGS}")
endif ()

#-----------------------------------------------------------------------------
# Option to allow the user to enable developer warnings
# Developer warnings (suggestions from gcc, not code problems)
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_DEV_WARNINGS)
  message (STATUS "....HDF5 developer group warnings are enabled")
  if (CMAKE_CXX_COMPILER_ID STREQUAL "Intel")
    if (_INTEL_WINDOWS)
      ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/intel-warnings/classic/win-developer-general")
    else ()
      ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/intel-warnings/classic/developer-general")
    endif()
  elseif (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    # Use the C warnings as CXX warnings are the same
    ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/developer-general")
  elseif (CMAKE_CXX_COMPILER_ID MATCHES "IntelLLVM")
    if (_INTEL_WINDOWS)
      ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/intel-warnings/oneapi/win-developer-general")
    else ()
      ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/intel-warnings/oneapi/developer-general")
    endif()
  elseif (CMAKE_CXX_COMPILER_ID MATCHES "[Cc]lang")
    ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/clang-warnings/developer-general")
  endif ()
else ()
  if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    # Use the C warnings as CXX warnings are the same
    ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/no-developer-general")
  elseif (CMAKE_CXX_COMPILER_ID MATCHES "[Cc]lang")
    ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/clang-warnings/no-developer-general")
  endif ()
endif ()

if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
  # Technically, variable-length arrays are part of the C99 standard, but
  #   we should approach them a bit cautiously... Only needed for gcc 4.X
  if (CMAKE_CXX_COMPILER_VERSION VERSION_LESS 5.0 AND CMAKE_C_COMPILER_VERSION VERSION_GREATER_EQUAL 4.8)
    # Use the C warnings as CXX warnings are the same
    ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/4.8-4.last")
  endif ()

  # Append more extra warning flags that only gcc 4.8+ knows about
  if (NOT CMAKE_CXX_COMPILER_VERSION VERSION_LESS 4.8)
    ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/cxx-4.8")
    if (HDF5_ENABLE_DEV_WARNINGS)
      # Use the C warnings as CXX warnings are the same
      ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/cxx-developer-4.8")
    else ()
      ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/no-cxx-developer-4.8")
    endif ()
  endif ()

  # Append more extra warning flags that only gcc 4.9+ knows about
  if (NOT CMAKE_CXX_COMPILER_VERSION VERSION_LESS 4.9)
    # Use the C warnings as CXX warnings are the same
    ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/cxx-4.9")
  endif ()

  # Append more extra warning flags that only gcc 5.1+ knows about
  if (NOT CMAKE_CXX_COMPILER_VERSION VERSION_LESS 5.0)
    # autotools always add the C flags with the CXX flags
    ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/cxx-5")
    ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/cxx-error-5")
  endif ()

  # Append more extra warning flags that only gcc 6.x+ knows about
  if (NOT CMAKE_CXX_COMPILER_VERSION VERSION_LESS 6.0)
    # Use the C warnings as CXX warnings are the same
    ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/6")
  endif ()

  # Append more extra warning flags that only gcc 7.x+ knows about
  if (NOT CMAKE_CXX_COMPILER_VERSION VERSION_LESS 7.0)
    # Use the C warnings as CXX warnings are the same
    ADD_H5_FLAGS (H5_CXxFLAGS2 "${HDF5_SOURCE_DIR}/config/gnu-warnings/7")
    if (HDF5_ENABLE_DEV_WARNINGS)
    # Use the C warnings as CXX warnings are the same
      ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/developer-7")
    #else ()
    #  ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/no-developer-7")
    endif ()
  endif ()

  # Append more extra warning flags that only gcc 8.x+ knows about
  if (NOT CMAKE_CXX_COMPILER_VERSION VERSION_LESS 8.0)
    # Use the C warnings as CXX warnings are the same
    ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/8")
    ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/error-8")
    if (HDF5_ENABLE_DEV_WARNINGS)
      # Use the C warnings as CXX warnings are the same
      ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/developer-8")
    else ()
      # autotools always add the C flags with the CXX flags
      ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/no-developer-8")
    endif ()
  endif ()

  # Append more extra warning flags that only gcc 9.x+ knows about
  if (NOT CMAKE_CXX_COMPILER_VERSION VERSION_LESS 9.0)
    # Use the C warnings as CXX warnings are the same
    ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/cxx-9")
  endif ()

  # Append more extra warning flags that only gcc 9.3+ knows about
  if (NOT CMAKE_CXX_COMPILER_VERSION VERSION_LESS 9.3)
    # do not use C warnings, gnu-warnings 9.3, no cxx warnings
    # ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/9.3")
  endif ()

  # Append more extra warning flags that only gcc 10.x+ knows about
  if (NOT CMAKE_CXX_COMPILER_VERSION VERSION_LESS 10.0)
    if (HDF5_ENABLE_DEV_WARNINGS)
      # Use the C warnings as CXX warnings are the same
      ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/developer-10")
    #else ()
    #  ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/no-developer-10")
    endif ()
  endif ()
endif ()

#-----------------------------------------------------------------------------
# Option to allow the user to enable all warnings
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_ALL_WARNINGS)
  message (STATUS "....All Warnings are enabled")
  if (MSVC)
    if (HDF5_ENABLE_DEV_WARNINGS)
      if (CMAKE_CXX_COMPILER_LOADED)
        string (REGEX REPLACE "(^| )([/-])W[0-9]( |$)" " " CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
        list (APPEND HDF5_CMAKE_CXX_FLAGS "/Wall" "/wd4668")
      endif ()
    else ()
      if (CMAKE_CXX_COMPILER_LOADED)
        string (REGEX REPLACE "(^| )([/-])W[0-9]( |$)" " " CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
        list (APPEND HDF5_CMAKE_CXX_FLAGS "/W3" "/wd4100" "/wd4706" "/wd4127")
      endif ()
    endif ()
  else ()
    if (CMAKE_CXX_COMPILER_LOADED)
      list (APPEND HDF5_CMAKE_CXX_FLAGS ${H5_CXXFLAGS})
    endif ()
  endif ()
endif ()

#-----------------------------------------------------------------------------
# This is in here to help some of the GCC based IDES like Eclipse
# and code blocks parse the compiler errors and warnings better.
#-----------------------------------------------------------------------------
if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU" AND CMAKE_CXX_COMPILER_LOADED)
  set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fmessage-length=0")
endif ()

#-----------------------------------------------------------------------------
# Option for --enable-symbols
# This option will force/override the default setting for all configurations
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_SYMBOLS MATCHES "YES")
  if (CMAKE_CXX_COMPILER_LOADED)
    if (CMAKE_CXX_COMPILER_ID STREQUAL "Intel" AND NOT _INTEL_WINDOWS)
      set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g")
    elseif (CMAKE_C_COMPILER_ID MATCHES "IntelLLVM" AND NOT _INTEL_WINDOWS)
      set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g")
    elseif (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
      set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g")
    endif ()
  endif ()
elseif (HDF5_ENABLE_SYMBOLS MATCHES "NO")
  if (CMAKE_CXX_COMPILER_LOADED)
    if (CMAKE_CXX_COMPILER_ID STREQUAL "Intel" AND NOT _INTEL_WINDOWS)
      set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wl,-s")
    elseif (CMAKE_CXX_COMPILER_ID MATCHES "IntelLLVM" AND NOT _INTEL_WINDOWS)
       set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wl,-s")
    elseif (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
      set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -s")
    endif ()
  endif ()
endif ()

#-----------------------------------------------------------------------------
# Option for --enable-profiling
# This option will force/override the default setting for all configurations
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_PROFILING)
  if (CMAKE_CXX_COMPILER_LOADED)
    list (APPEND HDF5_CMAKE_CXX_FLAGS "${PROFILE_CXXFLAGS}")
  endif ()
endif ()

#-----------------------------------------------------------------------------
# Option for --enable-optimization
# This option will force/override the default setting for all configurations
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_OPTIMIZATION)
  if (CMAKE_CXX_COMPILER_LOADED)
    list (APPEND HDF5_CMAKE_CXX_FLAGS "${OPTIMIZE_CXXFLAGS}")
  endif ()
endif ()
