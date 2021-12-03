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
#
# This file provides functions for Fortran support.
#
#-------------------------------------------------------------------------------
enable_language (Fortran)
set (HDF_PREFIX "H5")

if (NOT CMAKE_VERSION VERSION_LESS "3.14.0")
  include (CheckFortranSourceRuns)
  include (CheckFortranSourceCompiles)
endif ()

#-------------------------------------------------------------------------------
#  Fix Fortran flags if we are compiling staticly on Windows using
#  Windows_MT.cmake from config/cmake/UserMacros
#-------------------------------------------------------------------------------
if (BUILD_STATIC_CRT_LIBS)
  TARGET_STATIC_CRT_FLAGS ()
endif ()

#-----------------------------------------------------------------------------
# Detect name mangling convention used between Fortran and C
#-----------------------------------------------------------------------------
include (FortranCInterface)
FortranCInterface_HEADER (
    ${CMAKE_BINARY_DIR}/FCMangle.h
    MACRO_NAMESPACE "H5_FC_"
    SYMBOL_NAMESPACE "H5_FC_"
)

file (STRINGS ${CMAKE_BINARY_DIR}/FCMangle.h CONTENTS REGEX "H5_FC_GLOBAL\\(.*,.*\\) +(.*)")
string (REGEX MATCH "H5_FC_GLOBAL\\(.*,.*\\) +(.*)" RESULT ${CONTENTS})
set (H5_FC_FUNC "H5_FC_FUNC(name,NAME) ${CMAKE_MATCH_1}")

file (STRINGS ${CMAKE_BINARY_DIR}/FCMangle.h CONTENTS REGEX "H5_FC_GLOBAL_\\(.*,.*\\) +(.*)")
string (REGEX MATCH "H5_FC_GLOBAL_\\(.*,.*\\) +(.*)" RESULT ${CONTENTS})
set (H5_FC_FUNC_ "H5_FC_FUNC_(name,NAME) ${CMAKE_MATCH_1}")

#test code source
set (SIZEOF_CODE
  "
       PROGRAM main
       i = sizeof(x)
       END PROGRAM
  "
)
set (C_SIZEOF_CODE
  "
       PROGRAM main
         USE ISO_C_BINDING
         INTEGER(C_INT) :: a
         INTEGER(C_SIZE_T) :: result
         result = c_sizeof(a)
       END PROGRAM
  "
)
set (STORAGE_SIZE_CODE
  "
       PROGRAM main
         INTEGER :: a
         INTEGER :: result
         result = storage_size(a)
       END PROGRAM
  "
)
set (ISO_FORTRAN_ENV_CODE
  "
       PROGRAM main
         USE, INTRINSIC :: ISO_FORTRAN_ENV
       END PROGRAM
  "
)
set (REALISNOTDOUBLE_CODE
  "
       MODULE type_mod
         INTERFACE h5t
           MODULE PROCEDURE h5t_real
           MODULE PROCEDURE h5t_dble
         END INTERFACE
       CONTAINS
         SUBROUTINE h5t_real(r)
           REAL :: r
         END SUBROUTINE h5t_real
         SUBROUTINE h5t_dble(d)
           DOUBLE PRECISION :: d
         END SUBROUTINE h5t_dble
       END MODULE type_mod
       PROGRAM main
         USE type_mod
         REAL :: r
         DOUBLE PRECISION :: d
         CALL h5t(r)
         CALL h5t(d)
       END PROGRAM main
  "
)
set (ISO_C_BINDING_CODE
  "
       PROGRAM main
            USE iso_c_binding
            IMPLICIT NONE
            TYPE(C_PTR) :: ptr
            TYPE(C_FUNPTR) :: funptr
            INTEGER(C_INT64_T) :: c_int64_type
            CHARACTER(LEN=80, KIND=c_char), TARGET :: ichr
            ptr = C_LOC(ichr(1:1))
       END PROGRAM
  "
)

if (NOT CMAKE_VERSION VERSION_LESS "3.14.0")
  if (HDF5_REQUIRED_LIBRARIES)
    set (CMAKE_REQUIRED_LIBRARIES "${HDF5_REQUIRED_LIBRARIES}")
  endif ()
  check_fortran_source_compiles (${SIZEOF_CODE} ${HDF_PREFIX}_FORTRAN_HAVE_SIZEOF SRC_EXT f90)
  check_fortran_source_compiles (${C_SIZEOF_CODE} ${HDF_PREFIX}_FORTRAN_HAVE_C_SIZEOF SRC_EXT f90)
  check_fortran_source_compiles (${STORAGE_SIZE_CODE} ${HDF_PREFIX}_FORTRAN_HAVE_STORAGE_SIZE SRC_EXT f90)
  check_fortran_source_compiles (${ISO_FORTRAN_ENV_CODE} ${HDF_PREFIX}_HAVE_ISO_FORTRAN_ENV SRC_EXT f90)
  check_fortran_source_compiles (${REALISNOTDOUBLE_CODE} ${HDF_PREFIX}_FORTRAN_DEFAULT_REAL_NOT_DOUBLE SRC_EXT f90)
  check_fortran_source_compiles (${ISO_C_BINDING_CODE} ${HDF_PREFIX}_FORTRAN_HAVE_ISO_C_BINDING SRC_EXT f90)
else ()
  #-----------------------------------------------------------------------------
  # The provided CMake Fortran macros don't provide a general check function
  # so this one is used for a sizeof test.
  #-----------------------------------------------------------------------------
  macro (CHECK_FORTRAN_FEATURE FUNCTION CODE VARIABLE)
      if (CMAKE_VERSION VERSION_GREATER_EQUAL "3.15.0")
        message (VERBOSE "Testing Fortran ${FUNCTION}")
      endif ()
      if (HDF5_REQUIRED_LIBRARIES)
        set (CHECK_FUNCTION_EXISTS_ADD_LIBRARIES
            "-DLINK_LIBRARIES:STRING=${HDF5_REQUIRED_LIBRARIES}")
      else ()
        set (CHECK_FUNCTION_EXISTS_ADD_LIBRARIES)
      endif ()
      file (WRITE
          ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/testFortranCompiler.f90
          "${CODE}"
      )
      TRY_COMPILE (RESULT_VAR
          ${CMAKE_BINARY_DIR}
          ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/testFortranCompiler.f90
          CMAKE_FLAGS "${CHECK_FUNCTION_EXISTS_ADD_LIBRARIES}"
          OUTPUT_VARIABLE OUTPUT
      )

  #    if (CMAKE_VERSION VERSION_GREATER_EQUAL "3.15.0")
  #      message (TRACE "* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * ")
  #      message (TRACE "Test result ${OUTPUT}")
  #      message (TRACE "* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * ")
  #    endif ()

      if (${RESULT_VAR})
        set (${VARIABLE} 1 CACHE INTERNAL "Have Fortran function ${FUNCTION}")
        if (CMAKE_VERSION VERSION_GREATER_EQUAL "3.15.0")
          message (VERBOSE "Testing Fortran ${FUNCTION} - OK")
        endif ()
        file (APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeOutput.log
            "Determining if the Fortran ${FUNCTION} exists passed with the following output:\n"
            "${OUTPUT}\n\n"
        )
      else ()
        message (STATUS "Testing Fortran ${FUNCTION} - Fail")
        set (${VARIABLE} 0 CACHE INTERNAL "Have Fortran function ${FUNCTION}")
        file (APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
            "Determining if the Fortran ${FUNCTION} exists failed with the following output:\n"
            "${OUTPUT}\n\n")
      endif ()
  endmacro ()

  #-----------------------------------------------------------------------------
  # Configure Checks which require Fortran compilation must go in here
  # not in the main ConfigureChecks.cmake files, because if the user has
  # no Fortran compiler, problems arise.
  #
  # Be careful with leading spaces here, do not remove them.
  #-----------------------------------------------------------------------------

  # Check for Non-standard extension intrinsic function SIZEOF
  set (${HDF_PREFIX}_FORTRAN_HAVE_SIZEOF FALSE)
  CHECK_FORTRAN_FEATURE(sizeof_code ${SIZEOF_CODE} ${HDF_PREFIX}_FORTRAN_HAVE_SIZEOF)

  # Check for F2008 standard intrinsic function C_SIZEOF
  set (${HDF_PREFIX}_FORTRAN_HAVE_C_SIZEOF FALSE)
  CHECK_FORTRAN_FEATURE(c_sizeof_code ${C_SIZEOF_CODE} ${HDF_PREFIX}_FORTRAN_HAVE_C_SIZEOF)

  # Check for F2008 standard intrinsic function STORAGE_SIZE
  CHECK_FORTRAN_FEATURE(storage_size_code ${STORAGE_SIZE_CODE} ${HDF_PREFIX}_FORTRAN_HAVE_STORAGE_SIZE)

  # Check for F2008 standard intrinsic module "ISO_FORTRAN_ENV"
  set (${HDF_PREFIX}_HAVE_ISO_FORTRAN_ENV FALSE)
  CHECK_FORTRAN_FEATURE(iso_fortran_env_code ${ISO_FORTRAN_ENV_CODE} ${HDF_PREFIX}_HAVE_ISO_FORTRAN_ENV)

  set (${HDF_PREFIX}_FORTRAN_DEFAULT_REAL_NOT_DOUBLE FALSE)
  CHECK_FORTRAN_FEATURE(realisnotdouble_code ${REALISNOTDOUBLE_CODE} ${HDF_PREFIX}_FORTRAN_DEFAULT_REAL_NOT_DOUBLE)

  #-----------------------------------------------------------------------------
  # Checks if the ISO_C_BINDING module meets all the requirements
  #-----------------------------------------------------------------------------
  set (${HDF_PREFIX}_FORTRAN_HAVE_ISO_C_BINDING FALSE)
  CHECK_FORTRAN_FEATURE(iso_c_binding_code ${ISO_C_BINDING_CODE} ${HDF_PREFIX}_FORTRAN_HAVE_ISO_C_BINDING)
endif ()

#-----------------------------------------------------------------------------
# Add debug information (intel Fortran : JB)
#-----------------------------------------------------------------------------
if (CMAKE_Fortran_COMPILER MATCHES ifort)
    if (WIN32 AND NOT MINGW)
        set (CMAKE_Fortran_FLAGS_DEBUG "/debug:full /dbglibs " CACHE STRING "flags" FORCE)
        set (CMAKE_EXE_LINKER_FLAGS_DEBUG "/DEBUG" CACHE STRING "flags" FORCE)
    endif ()
endif ()
