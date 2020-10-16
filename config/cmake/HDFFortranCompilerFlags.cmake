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

message (STATUS "Warnings Configuration: default Fortran: ${CMAKE_Fortran_FLAGS}")

#-----------------------------------------------------------------------------
# Option to allow the user to disable compiler warnings
#-----------------------------------------------------------------------------
if (HDF5_DISABLE_COMPILER_WARNINGS)
  message (STATUS "....Compiler warnings are suppressed")
  # MSVC uses /w to suppress warnings.  It also complains if another
  # warning level is given, so remove it.
  if (MSVC)
    set (HDF5_WARNINGS_BLOCKED 1)
    if (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
      set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} /warn:none")
    endif ()
  endif ()
  if (WIN32)
    add_definitions (-D_CRT_SECURE_NO_WARNINGS)
  endif ()
  # Borland uses -w- to suppress warnings.
  if (BORLAND)
    set (HDF5_WARNINGS_BLOCKED 1)
    set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -w-")
  endif ()

  # Most compilers use -w to suppress warnings.
  if (NOT HDF5_WARNINGS_BLOCKED)
    set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -w")
  endif ()
endif ()

#-----------------------------------------------------------------------------
# HDF5 library compile options
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# CDash is configured to only allow 3000 warnings, so
# break into groups (from the config/gnu-flags file)
#-----------------------------------------------------------------------------
if (NOT MSVC AND NOT MINGW)
  # General flags
  if (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
    ADD_H5_FLAGS (HDF5_CMAKE_Fortran_FLAGS "${HDF5_SOURCE_DIR}/config/intel-warnings/ifort-general")
    list (APPEND HDF5_CMAKE_Fortran_FLAGS "-stand:f03" "-free")
  elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    ADD_H5_FLAGS (HDF5_CMAKE_Fortran_FLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/gfort-general")
    list (APPEND HDF5_CMAKE_Fortran_FLAGS "-ffree-form" "-fimplicit-none")
    if (CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 8.0 AND NOT CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 4.6)
      list (APPEND HDF5_CMAKE_Fortran_FLAGS "-std=f2008ts")
    else ()
      list (APPEND HDF5_CMAKE_Fortran_FLAGS "-std=f2008")
    endif ()
  elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "PGI")
    list (APPEND HDF5_CMAKE_Fortran_FLAGS "-Mfreeform" "-Mdclchk" "-Mstandard" "-Mallocatable=03")
  endif ()
  message (STATUS "HDF5_CMAKE_Fortran_FLAGS=${HDF5_CMAKE_Fortran_FLAGS}")

  if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")

    # Append more extra warning flags that only gcc 4.8+ know about
    if (NOT CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 4.8)
      ADD_H5_FLAGS (HDF5_CMAKE_Fortran_FLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/gfort-4.8")
    endif ()

    # Append more extra warning flags that only gcc 4.9+ know about
    #if (NOT CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 4.9)
    #  ADD_H5_FLAGS (HDF5_CMAKE_Fortran_FLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/gfort-4.9")
    #endif ()

    # Append more extra warning flags that only gcc 5.x+ know about
    if (NOT CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 5.0)
      ADD_H5_FLAGS (HDF5_CMAKE_Fortran_FLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/gfort-5")
    endif ()

    # Append more extra warning flags that only gcc 6.x+ know about
    if (NOT CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 6.0)
      ADD_H5_FLAGS (HDF5_CMAKE_Fortran_FLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/gfort-6")
    endif ()

    # Append more extra warning flags that only gcc 7.x+ know about
    #if (NOT CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 7.0)
    #  ADD_H5_FLAGS (HDF5_CMAKE_Fortran_FLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/gfort-7")
    #endif ()

    # Append more extra warning flags that only gcc 8.x+ know about
    if (NOT CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 8.0)
      ADD_H5_FLAGS (HDF5_CMAKE_Fortran_FLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/gfort-8")
    endif ()

    # Append more extra warning flags that only gcc 9.x+ know about
    #if (NOT CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 9.0)
    #  ADD_H5_FLAGS (HDF5_CMAKE_Fortran_FLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/gfort-9")
    #endif ()
  endif ()
else ()
  if (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
    #ADD_H5_FLAGS (HDF5_CMAKE_Fortran_FLAGS "${HDF5_SOURCE_DIR}/config/intel-warnings/win-ifort-general")
    list (APPEND HDF5_CMAKE_Fortran_FLAGS "/warn:all" "/stand:f03" "/free")
  endif ()
endif ()

