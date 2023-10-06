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
set(CMAKE_C_STANDARD 99)
set(CMAKE_C_STANDARD_REQUIRED TRUE)

set (CMAKE_C_FLAGS "${CMAKE_C99_STANDARD_COMPILE_OPTION} ${CMAKE_C_FLAGS}")
set (CMAKE_C_FLAGS "${CMAKE_C_SANITIZER_FLAGS} ${CMAKE_C_FLAGS}")
message (VERBOSE "Warnings Configuration: C default: ${CMAKE_C_FLAGS}")
#-----------------------------------------------------------------------------
# Compiler specific flags : Shouldn't there be compiler tests for these
#-----------------------------------------------------------------------------
if(WIN32 AND CMAKE_C_COMPILER_ID STREQUAL "Intel")
  set(_INTEL_WINDOWS 1)
endif()

if(WIN32 AND CMAKE_C_COMPILER_ID MATCHES "[Cc]lang" AND "x${CMAKE_C_SIMULATE_ID}" STREQUAL "xMSVC")
  set(_CLANG_MSVC_WINDOWS 1)
endif()

# Disable deprecation warnings for standard C functions.
# really only needed for newer versions of VS, but should
# not hurt other versions, and this will work into the
# future
if(MSVC OR _INTEL_WINDOWS OR _CLANG_MSVC_WINDOWS)
  add_definitions(-D_CRT_SECURE_NO_DEPRECATE -D_CRT_NONSTDC_NO_DEPRECATE)
endif()

if(MSVC)
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -stack:10000000")
endif()

# MSVC 14.28 enables C5105, but the Windows SDK 10.0.18362.0 triggers it.
if(CMAKE_C_COMPILER_ID STREQUAL "MSVC" AND NOT CMAKE_C_COMPILER_VERSION VERSION_LESS 19.28)
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -wd5105")
endif()

if(_CLANG_MSVC_WINDOWS AND "x${CMAKE_C_COMPILER_FRONTEND_VARIANT}" STREQUAL "xGNU")
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -Xlinker -stack:20000000")
endif()

if (CMAKE_COMPILER_IS_GNUCC)
  set (CMAKE_C_FLAGS "${CMAKE_ANSI_CFLAGS} ${CMAKE_C_FLAGS}")
  if (${HDF_CFG_NAME} MATCHES "Debug" OR ${HDF_CFG_NAME} MATCHES "Developer")
    if (NOT CMAKE_C_COMPILER_VERSION VERSION_LESS 5.0)
      set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Og -ftrapv -fno-common")
    endif ()
  else ()
    if (CMAKE_C_COMPILER_ID STREQUAL "GNU" AND NOT CMAKE_C_COMPILER_VERSION VERSION_LESS 5.0 AND
        NOT CMAKE_C_CLANG_TIDY)
      # `clang-tidy` does not understand -fstdarg-opt
      set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fstdarg-opt")
    endif ()
    if (CMAKE_C_COMPILER_ID STREQUAL "GNU" AND NOT CMAKE_C_COMPILER_VERSION VERSION_LESS 10.0)
      #-----------------------------------------------------------------------------
      # Option to allow the user to enable build extended diagnostics
      #
      # This should NOT be on by default as it can cause process issues.
      #-----------------------------------------------------------------------------
      option (HDF5_ENABLE_BUILD_DIAGS "Enable color and URL extended diagnostic messages" OFF)
      mark_as_advanced (HDF5_ENABLE_BUILD_DIAGS)
      if (HDF5_ENABLE_BUILD_DIAGS)
        message (STATUS "... default color and URL extended diagnostic messages enabled")
      else ()
        message (STATUS "... disable color and URL extended diagnostic messages")
        set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fdiagnostics-urls=never -fno-diagnostics-color")
      endif ()
    endif ()
  endif ()
endif ()

#-----------------------------------------------------------------------------
# Option to allow the user to disable compiler warnings
#-----------------------------------------------------------------------------
option (HDF5_DISABLE_COMPILER_WARNINGS "Disable compiler warnings" OFF)
if (HDF5_DISABLE_COMPILER_WARNINGS)
  message (STATUS "....Compiler warnings are suppressed")
  # MSVC uses /w to suppress warnings.  It also complains if another
  # warning level is given, so remove it.
  if (MSVC)
    set (HDF5_WARNINGS_BLOCKED 1)
    string (REGEX REPLACE "(^| )([/-])W[0-9]( |$)" " " CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} /W0")
  endif ()
  if (WIN32)
    add_definitions (-D_CRT_SECURE_NO_WARNINGS)
  endif ()
  # Borland uses -w- to suppress warnings.
  if (BORLAND)
    set (HDF5_WARNINGS_BLOCKED 1)
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -w-")
  endif ()

  # Most compilers use -w to suppress warnings.
  if (NOT HDF5_WARNINGS_BLOCKED)
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -w")
  endif ()
endif ()

#-----------------------------------------------------------------------------
# HDF5 library compile options
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Option to allow the user to interpret certain warnings as errors
#
# This should NOT be on by default as it can cause a lot of conflicts with
# new operating systems and compiler versions. Header files that are out of
# our control (MPI, HDFS, etc.) can also raise warnings.
#-----------------------------------------------------------------------------
option (HDF5_ENABLE_WARNINGS_AS_ERRORS "Interpret some warnings as errors" OFF)
if (HDF5_ENABLE_WARNINGS_AS_ERRORS)
  message (STATUS "...some warnings will be interpreted as errors")
endif ()

if (${CMAKE_SYSTEM_NAME} MATCHES "SunOS")
  list (APPEND HDF5_CMAKE_C_FLAGS "-erroff=%none -DBSD_COMP")
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
  if (CMAKE_C_COMPILER_ID STREQUAL "Intel")
    if (_INTEL_WINDOWS)
      ADD_H5_FLAGS (HDF5_CMAKE_C_FLAGS "${HDF5_SOURCE_DIR}/config/intel-warnings/win-general")
    else ()
      ADD_H5_FLAGS (HDF5_CMAKE_C_FLAGS "${HDF5_SOURCE_DIR}/config/intel-warnings/general")
    endif()
    if (NOT _INTEL_WINDOWS)
      if(NOT CMAKE_C_COMPILER_VERSION VERSION_LESS 15.0)
        ADD_H5_FLAGS (H5_CFLAGS "${HDF5_SOURCE_DIR}/config/intel-warnings/15")
      endif()
      # this is just a failsafe
      list (APPEND H5_CFLAGS "-finline-functions")
      if(NOT CMAKE_C_COMPILER_VERSION VERSION_LESS 18.0)
        ADD_H5_FLAGS (H5_CFLAGS "${HDF5_SOURCE_DIR}/config/intel-warnings/18")
      endif()
    endif()
  elseif (CMAKE_C_COMPILER_ID STREQUAL "GNU")
    # Add general CFlags for GCC versions 4.8 and above
    if (CMAKE_C_COMPILER_VERSION VERSION_GREATER_EQUAL 4.8)
      ADD_H5_FLAGS (HDF5_CMAKE_C_FLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/general")
      ADD_H5_FLAGS (H5_CFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/error-general")
    endif ()
    # gcc automatically inlines based on the optimization level
    # this is just a failsafe
    list (APPEND H5_CFLAGS "-finline-functions")
  elseif (CMAKE_C_COMPILER_ID MATCHES "IntelLLVM" OR CMAKE_C_COMPILER_ID MATCHES "[Cc]lang")
    ADD_H5_FLAGS (HDF5_CMAKE_C_FLAGS "${HDF5_SOURCE_DIR}/config/clang-warnings/general")
    ADD_H5_FLAGS (H5_CFLAGS "${HDF5_SOURCE_DIR}/config/clang-warnings/error-general")
  elseif (CMAKE_C_COMPILER_ID STREQUAL "PGI")
    list (APPEND HDF5_CMAKE_C_FLAGS "-Minform=inform")
  endif ()
  message (VERBOSE "CMAKE_C_FLAGS_GENERAL=${HDF5_CMAKE_C_FLAGS}")
endif ()

#-----------------------------------------------------------------------------
# Option to allow the user to enable developer warnings
# Developer warnings (suggestions from gcc, not code problems)
#-----------------------------------------------------------------------------
option (HDF5_ENABLE_DEV_WARNINGS "Enable HDF5 developer group warnings" OFF)
if (${HDF_CFG_NAME} MATCHES "Developer")
  # Developer build modes should always have these types of warnings enabled
  set (HDF5_ENABLE_DEV_WARNINGS ON CACHE BOOL "Enable HDF5 developer group warnings" FORCE)
endif ()
if (HDF5_ENABLE_DEV_WARNINGS)
  message (STATUS "....HDF5 developer group warnings are enabled")
  if (CMAKE_C_COMPILER_ID STREQUAL "Intel")
    if (_INTEL_WINDOWS)
      ADD_H5_FLAGS (H5_CFLAGS "${HDF5_SOURCE_DIR}/config/intel-warnings/win-developer-general")
    else ()
      ADD_H5_FLAGS (H5_CFLAGS "${HDF5_SOURCE_DIR}/config/intel-warnings/developer-general")
    endif ()
  elseif (CMAKE_C_COMPILER_ID STREQUAL "GNU" AND CMAKE_C_COMPILER_VERSION VERSION_GREATER_EQUAL 4.8)
    ADD_H5_FLAGS (H5_CFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/developer-general")
  elseif (CMAKE_C_COMPILER_ID MATCHES "IntelLLVM" OR CMAKE_C_COMPILER_ID MATCHES "[Cc]lang")
    ADD_H5_FLAGS (H5_CFLAGS "${HDF5_SOURCE_DIR}/config/clang-warnings/developer-general")
  endif ()

  # Turn on -Winline warnings now only for non-Debug and
  # non-Developer builds. For at least GNU compilers this
  # flag appears to conflict specifically with the -Og
  # optimization flag and will produce warnings about functions
  # not being considered for inlining
  if (NOT ${HDF_CFG_NAME} MATCHES "Debug" AND NOT ${HDF_CFG_NAME} MATCHES "Developer")
    if (CMAKE_C_COMPILER_ID STREQUAL "GNU")
      list (APPEND H5_CFLAGS "-Winline")
    elseif (CMAKE_C_COMPILER_ID STREQUAL "Intel" AND NOT _INTEL_WINDOWS)
      list (APPEND H5_CFLAGS "-Winline")
    endif ()
  endif ()
else ()
  if (CMAKE_C_COMPILER_ID STREQUAL "GNU" AND CMAKE_C_COMPILER_VERSION VERSION_GREATER_EQUAL 4.8)
    ADD_H5_FLAGS (H5_CFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/no-developer-general")
  elseif (CMAKE_C_COMPILER_ID MATCHES "IntelLLVM" OR CMAKE_C_COMPILER_ID MATCHES "[Cc]lang")
    ADD_H5_FLAGS (H5_CFLAGS "${HDF5_SOURCE_DIR}/config/clang-warnings/no-developer-general")
  endif ()
endif ()

if (CMAKE_C_COMPILER_ID STREQUAL "GNU")
  # Technically, variable-length arrays are part of the C99 standard, but
  #   we should approach them a bit cautiously... Only needed for gcc 4.X
  if (CMAKE_C_COMPILER_VERSION VERSION_LESS 5.0 AND CMAKE_C_COMPILER_VERSION VERSION_GREATER_EQUAL 4.8)
    ADD_H5_FLAGS (H5_CFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/4.8-4.last")
  endif ()

  # Append more extra warning flags that only gcc 4.8+ knows about
  if (NOT CMAKE_C_COMPILER_VERSION VERSION_LESS 4.8)
    ADD_H5_FLAGS (H5_CFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/4.8")
    if (HDF5_ENABLE_DEV_WARNINGS)
      ADD_H5_FLAGS (H5_CFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/developer-4.8")
    else ()
      ADD_H5_FLAGS (H5_CFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/no-developer-4.8")
    endif ()
  endif ()

  # Append more extra warning flags that only gcc 4.9+ knows about
  if (NOT CMAKE_C_COMPILER_VERSION VERSION_LESS 4.9)
    ADD_H5_FLAGS (H5_CFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/4.9")
  endif ()

  # Append more extra warning flags that only gcc 5.x+ knows about
  if (NOT CMAKE_C_COMPILER_VERSION VERSION_LESS 5.0)
    ADD_H5_FLAGS (H5_CFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/5")
    ADD_H5_FLAGS (H5_CFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/error-5")
  endif ()

  # Append more extra warning flags that only gcc 6.x+ knows about
  if (NOT CMAKE_C_COMPILER_VERSION VERSION_LESS 6.0)
    ADD_H5_FLAGS (H5_CFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/6")
  endif ()

  # Append more extra warning flags that only gcc 7.x+ knows about
  if (NOT CMAKE_C_COMPILER_VERSION VERSION_LESS 7.0)
    ADD_H5_FLAGS (H5_CFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/7")
    ADD_H5_FLAGS (H5_CFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/error-7")
    if (HDF5_ENABLE_DEV_WARNINGS)
      ADD_H5_FLAGS (H5_CFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/developer-7")
    #else ()
    #  ADD_H5_FLAGS (H5_CFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/no-developer-7")
    endif ()
  endif ()

  # Append more extra warning flags that only gcc 8.x+ knows about
  if (NOT CMAKE_C_COMPILER_VERSION VERSION_LESS 8.0)
    ADD_H5_FLAGS (H5_CFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/8")
    ADD_H5_FLAGS (H5_CFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/error-8")
    if (HDF5_ENABLE_DEV_WARNINGS)
      ADD_H5_FLAGS (H5_CFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/developer-8")
    else ()
      ADD_H5_FLAGS (H5_CFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/no-developer-8")
    endif ()
  endif ()

  # Append more extra warning flags that only gcc 9.x+ knows about
  if (NOT CMAKE_C_COMPILER_VERSION VERSION_LESS 9.0)
    ADD_H5_FLAGS (H5_CFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/9")
  endif ()

  # Append more extra warning flags that only gcc 9.3+ knows about
  if (NOT CMAKE_C_COMPILER_VERSION VERSION_LESS 9.3)
    ADD_H5_FLAGS (H5_CFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/9.3")
  endif ()

  # Append more extra warning flags that only gcc 10.x+ knows about
  if (NOT CMAKE_C_COMPILER_VERSION VERSION_LESS 10.0)
    if (HDF5_ENABLE_DEV_WARNINGS)
      ADD_H5_FLAGS (H5_CFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/developer-10")
    #else ()
    #  ADD_H5_FLAGS (H5_CFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/no-developer-10")
    endif ()
  endif ()

  # Append more extra warning flags that only gcc 12.x+ knows about
  # or which should only be enabled for gcc 12.x+
  if (NOT CMAKE_C_COMPILER_VERSION VERSION_LESS 12.0)
    if (HDF5_ENABLE_DEV_WARNINGS)
      ADD_H5_FLAGS (H5_CFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/developer-12")
    #else ()
    #  ADD_H5_FLAGS (H5_CFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/no-developer-12")
    endif ()
  endif ()
endif ()

#-----------------------------------------------------------------------------
# Option to allow the user to enable debug output
# from various HDF5 modules
#-----------------------------------------------------------------------------
option (HDF5_ENABLE_DEBUG_APIS "Turn on extra debug output in all packages" OFF)
if (HDF5_ENABLE_DEBUG_APIS)
  # Add standard debug definitions to any existing ones
  list (APPEND HDF5_DEBUG_APIS
      H5AC_DEBUG
      H5CX_DEBUG
      H5D_DEBUG
      H5D_CHUNK_DEBUG
      H5F_DEBUG
      H5HL_DEBUG
      H5I_DEBUG
      H5O_DEBUG
      H5S_DEBUG
      H5T_DEBUG
      H5Z_DEBUG
  )
endif ()

#-----------------------------------------------------------------------------
# Option to allow the user to enable all warnings
#-----------------------------------------------------------------------------
option (HDF5_ENABLE_ALL_WARNINGS "Enable all warnings" ON)
if (HDF5_ENABLE_ALL_WARNINGS)
  message (STATUS "....All Warnings are enabled")
  if (MSVC)
    if (HDF5_ENABLE_DEV_WARNINGS)
      string (REGEX REPLACE "(^| )([/-])W[0-9]( |$)" " " CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
      list (APPEND HDF5_CMAKE_C_FLAGS "/Wall" "/wd4668")
    else ()
      string (REGEX REPLACE "(^| )([/-])W[0-9]( |$)" " " CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
      list (APPEND HDF5_CMAKE_C_FLAGS "/W3" "/wd4100" "/wd4706" "/wd4127")
    endif ()
  else ()
    list (APPEND HDF5_CMAKE_C_FLAGS ${H5_CFLAGS})
  endif ()
endif ()

#-----------------------------------------------------------------------------
# This is in here to help some of the GCC based IDES like Eclipse
# and code blocks parse the compiler errors and warnings better.
#-----------------------------------------------------------------------------
if (CMAKE_COMPILER_IS_GNUCC)
  set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fmessage-length=0")
endif ()

#-----------------------------------------------------------------------------
# Option for --enable-asserts
# By default, CMake adds NDEBUG to CMAKE_${lang}_FLAGS for Release build types
# This option will force/override the default setting for all configurations
#-----------------------------------------------------------------------------
#option (HDF5_ENABLE_ASSERTS "Determines whether NDEBUG is defined to control assertions." OFF)
set (HDF5_ENABLE_ASSERTS "OFF" CACHE STRING "Determines whether NDEBUG is defined to control assertions (OFF NO YES)")
set_property (CACHE HDF5_ENABLE_ASSERTS PROPERTY STRINGS OFF NO YES)
if (HDF5_ENABLE_ASSERTS MATCHES "YES")
  add_compile_options ("-UNDEBUG")
elseif (HDF5_ENABLE_ASSERTS MATCHES "NO")
  add_compile_options ("-DNDEBUG")
endif ()
MARK_AS_ADVANCED (HDF5_ENABLE_ASSERTS)

#-----------------------------------------------------------------------------
# Option for --enable-symbols
# This option will force/override the default setting for all configurations
#-----------------------------------------------------------------------------
#option (HDF5_ENABLE_SYMBOLS "Add debug symbols to the library independent of the build mode and optimization level." OFF)
set (HDF5_ENABLE_SYMBOLS "OFF" CACHE STRING "Add debug symbols to the library independent of the build mode and optimization level (OFF NO YES)")
set_property (CACHE HDF5_ENABLE_SYMBOLS PROPERTY STRINGS OFF NO YES)
if (HDF5_ENABLE_SYMBOLS MATCHES "YES")
  if (CMAKE_C_COMPILER_ID STREQUAL "Intel" AND NOT _INTEL_WINDOWS)
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -g")
  elseif (CMAKE_C_COMPILER_ID STREQUAL "GNU")
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -g -fno-omit-frame-pointer")
  endif ()
elseif (HDF5_ENABLE_SYMBOLS MATCHES "NO")
  if (CMAKE_C_COMPILER_ID STREQUAL "Intel" AND NOT _INTEL_WINDOWS)
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wl,-s")
  elseif (CMAKE_C_COMPILER_ID STREQUAL "GNU")
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -s")
  endif ()
endif ()
MARK_AS_ADVANCED (HDF5_ENABLE_SYMBOLS)

#-----------------------------------------------------------------------------
# Option for --enable-profiling
# This option will force/override the default setting for all configurations
#-----------------------------------------------------------------------------
option (HDF5_ENABLE_PROFILING "Enable profiling flags independently from the build mode." OFF)
if (HDF5_ENABLE_PROFILING)
  list (APPEND HDF5_CMAKE_C_FLAGS "${PROFILE_CFLAGS}")
endif ()
MARK_AS_ADVANCED (HDF5_ENABLE_PROFILING)

#-----------------------------------------------------------------------------
# Option for --enable-optimization
# This option will force/override the default setting for all configurations
#-----------------------------------------------------------------------------
option (HDF5_ENABLE_OPTIMIZATION "Enable optimization flags/settings independently from the build mode" OFF)
if (HDF5_ENABLE_OPTIMIZATION)
  list (APPEND HDF5_CMAKE_C_FLAGS "${OPTIMIZE_CFLAGS}")
endif ()
MARK_AS_ADVANCED (HDF5_ENABLE_OPTIMIZATION)
