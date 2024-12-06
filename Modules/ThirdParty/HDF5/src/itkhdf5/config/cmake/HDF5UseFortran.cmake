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
# This file provides functions for HDF5 specific Fortran support.
#
#-------------------------------------------------------------------------------
include (${HDF_RESOURCES_DIR}/HDFUseFortran.cmake)

include (CheckFortranFunctionExists)

# Force lowercase Fortran module file names
if (CMAKE_Fortran_COMPILER_ID STREQUAL "Cray")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ef")
endif ()

set (RUN_OUTPUT_PATH_DEFAULT ${CMAKE_BINARY_DIR})
# The provided CMake Fortran macros don't provide a general compile/run function
# so this one is used.
#-----------------------------------------------------------------------------
macro (FORTRAN_RUN FUNCTION_NAME SOURCE_CODE RUN_RESULT_VAR1 COMPILE_RESULT_VAR1 RETURN_VAR RETURN_OUTPUT_VAR)
    message (VERBOSE "Detecting Fortran ${FUNCTION_NAME}")
    file (WRITE
        ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/testFortranCompiler1.f90
        "${SOURCE_CODE}"
    )
    if (CMAKE_VERSION VERSION_LESS 3.25)
      set (_RUN_OUTPUT_VARIABLE "RUN_OUTPUT_VARIABLE")
    else ()
      set (_RUN_OUTPUT_VARIABLE  "RUN_OUTPUT_STDOUT_VARIABLE")
    endif()
    if (${FUNCTION_NAME} STREQUAL "SIZEOF NATIVE KINDs")
        set(TMP_CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}")
    else ()
        set(TMP_CMAKE_Fortran_FLAGS "")
    endif ()
    TRY_RUN (RUN_RESULT_VAR COMPILE_RESULT_VAR
        ${CMAKE_BINARY_DIR}
        ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/testFortranCompiler1.f90
        CMAKE_FLAGS "${TMP_CMAKE_Fortran_FLAGS}"
        LINK_LIBRARIES "${HDF5_REQUIRED_LIBRARIES}"
        ${_RUN_OUTPUT_VARIABLE} OUTPUT_VAR
    )
    set (${RETURN_OUTPUT_VAR} ${OUTPUT_VAR})

    if (${COMPILE_RESULT_VAR})
      set(${RETURN_VAR} ${RUN_RESULT_VAR})
      if (${RUN_RESULT_VAR} MATCHES 0)
        message (VERBOSE "Testing Fortran ${FUNCTION_NAME} - OK")
        file (APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeOutput.log
            "Determining if the Fortran ${FUNCTION_NAME} exists passed\n"
        )
      else ()
        message (VERBOSE "Testing Fortran ${FUNCTION_NAME} - Fail")
        file (APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
            "Determining if the Fortran ${FUNCTION_NAME} exists failed: ${RUN_RESULT_VAR}\n"
        )
      endif ()
    else ()
        message (VERBOSE "Compiling Fortran ${FUNCTION_NAME} - Fail")
        file (APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
            "Determining if the Fortran ${FUNCTION_NAME} compiles failed: ${COMPILE_RESULT_VAR}\n"
        )
        set(${RETURN_VAR} ${COMPILE_RESULT_VAR})
    endif ()
endmacro ()
#-----------------------------------------------------------------------------
#  Check to see C_LONG_DOUBLE is available

READ_SOURCE("PROGRAM PROG_FC_HAVE_C_LONG_DOUBLE" "END PROGRAM PROG_FC_HAVE_C_LONG_DOUBLE" SOURCE_CODE)
check_fortran_source_compiles (${SOURCE_CODE} FORTRAN_HAVE_C_LONG_DOUBLE SRC_EXT f90)

if (${FORTRAN_HAVE_C_LONG_DOUBLE})
  set (${HDF_PREFIX}_FORTRAN_HAVE_C_LONG_DOUBLE 1)
else ()
  set (${HDF_PREFIX}_FORTRAN_HAVE_C_LONG_DOUBLE 0)
endif ()

# Check to see C_LONG_DOUBLE is different from C_DOUBLE

READ_SOURCE("MODULE type_mod" "END PROGRAM PROG_FC_C_LONG_DOUBLE_EQ_C_DOUBLE" SOURCE_CODE)
check_fortran_source_compiles (${SOURCE_CODE} FORTRAN_C_LONG_DOUBLE_IS_UNIQUE SRC_EXT f90)
if (${FORTRAN_C_LONG_DOUBLE_IS_UNIQUE})
  set (${HDF_PREFIX}_FORTRAN_C_LONG_DOUBLE_IS_UNIQUE 1)
else ()
  set (${HDF_PREFIX}_FORTRAN_C_LONG_DOUBLE_IS_UNIQUE 0)
endif ()

# Check to see C_BOOL is different from default LOGICAL

READ_SOURCE("MODULE l_type_mod" "END PROGRAM PROG_FC_C_BOOL_EQ_LOGICAL" SOURCE_CODE)
check_fortran_source_compiles (${SOURCE_CODE} FORTRAN_C_BOOL_IS_UNIQUE SRC_EXT f90)
if (${FORTRAN_C_BOOL_IS_UNIQUE})
  set (${HDF_PREFIX}_FORTRAN_C_BOOL_IS_UNIQUE 1)
else ()
  set (${HDF_PREFIX}_FORTRAN_C_BOOL_IS_UNIQUE 0)
endif ()

# Check if the fortran compiler supports the intrinsic module "ISO_FORTRAN_ENV" (F08)

READ_SOURCE("PROGRAM PROG_FC_ISO_FORTRAN_ENV" "END PROGRAM PROG_FC_ISO_FORTRAN_ENV" SOURCE_CODE)
check_fortran_source_compiles (${SOURCE_CODE} HAVE_ISO_FORTRAN_ENV SRC_EXT f90)
if (${HAVE_ISO_FORTRAN_ENV})
  set (${HDF_PREFIX}_HAVE_ISO_FORTRAN_ENV 1)
else ()
  set (${HDF_PREFIX}_HAVE_ISO_FORTRAN_ENV 0)
endif ()

## Set the sizeof function for use later in the fortran tests
if (${HDF_PREFIX}_FORTRAN_HAVE_STORAGE_SIZE)
  set (FC_SIZEOF_A "STORAGE_SIZE(a, c_size_t)/STORAGE_SIZE(c_char_'a',c_size_t)")
  set (FC_SIZEOF_B "STORAGE_SIZE(b, c_size_t)/STORAGE_SIZE(c_char_'a',c_size_t)")
  set (FC_SIZEOF_C "STORAGE_SIZE(c, c_size_t)/STORAGE_SIZE(c_char_'a',c_size_t)")
elseif (${HDF_PREFIX}_FORTRAN_HAVE_C_SIZEOF)
  set (FC_SIZEOF_A "SIZEOF(a)")
  set (FC_SIZEOF_B "SIZEOF(b)")
  set (FC_SIZEOF_C "SIZEOF(c)")
else ()
  message (FATAL_ERROR "Fortran compiler requires either intrinsic functions SIZEOF or STORAGE_SIZE")
endif ()

# Check to see of Fortran supports allocatable character
READ_SOURCE("PROGRAM PROG_CHAR_ALLOC" "END PROGRAM PROG_CHAR_ALLOC" SOURCE_CODE)
check_fortran_source_compiles (${SOURCE_CODE} FORTRAN_CHAR_ALLOC SRC_EXT f90)
if (${FORTRAN_CHAR_ALLOC})
  set (${HDF_PREFIX}_FORTRAN_HAVE_CHAR_ALLOC 1)
else ()
  set (${HDF_PREFIX}_FORTRAN_HAVE_CHAR_ALLOC 0)
endif ()

#-----------------------------------------------------------------------------
# Determine the available KINDs for REALs and INTEGERs
#-----------------------------------------------------------------------------
if (${HAVE_ISO_FORTRAN_ENV})
  READ_SOURCE ("PROGRAM FC08_AVAIL_KINDS" "END PROGRAM FC08_AVAIL_KINDS" SOURCE_CODE)
else ()
  READ_SOURCE ("PROGRAM FC_AVAIL_KINDS" "END PROGRAM FC_AVAIL_KINDS" SOURCE_CODE)
endif ()

FORTRAN_RUN ("REAL and INTEGER KINDs"
    "${SOURCE_CODE}"
    XX
    YY
    FC_AVAIL_KINDS_RESULT
    PROG_OUTPUT
)
# dnl The output from the above program will be:
# dnl    -- LINE 1 --  valid integer kinds (comma separated list)
# dnl    -- LINE 2 --  valid real kinds (comma separated list)
# dnl    -- LINE 3 --  max decimal precision for reals
# dnl    -- LINE 4 --  number of valid integer kinds
# dnl    -- LINE 5 --  number of valid real kinds
# dnl    -- LINE 6 --  number of valid logical kinds
# dnl    -- LINE 7 --  valid logical kinds (comma separated list)

#
# Convert the string to a list of strings by replacing the carriage return with a semicolon
string (REGEX REPLACE "[\r\n]+" ";" PROG_OUTPUT "${PROG_OUTPUT}")

list (GET PROG_OUTPUT 0 pac_validIntKinds)
list (GET PROG_OUTPUT 1 pac_validRealKinds)
list (GET PROG_OUTPUT 2 ${HDF_PREFIX}_PAC_FC_MAX_REAL_PRECISION)

# If the lists are empty then something went wrong.
if (NOT pac_validIntKinds)
    message (FATAL_ERROR "Failed to find available INTEGER KINDs for Fortran")
endif ()
if (NOT pac_validRealKinds)
    message (FATAL_ERROR "Failed to find available REAL KINDs for Fortran")
endif ()
if (NOT ${HDF_PREFIX}_PAC_FC_MAX_REAL_PRECISION)
    message (FATAL_ERROR "No output from Fortran decimal precision program")
endif ()

set (PAC_FC_ALL_INTEGER_KINDS "\{${pac_validIntKinds}\}")
set (PAC_FC_ALL_REAL_KINDS "\{${pac_validRealKinds}\}")

list (GET PROG_OUTPUT 3 NUM_IKIND)
list (GET PROG_OUTPUT 4 NUM_RKIND)

set (PAC_FORTRAN_NUM_INTEGER_KINDS "${NUM_IKIND}")

set (${HDF_PREFIX}_H5CONFIG_F_NUM_IKIND "INTEGER, PARAMETER :: num_ikinds = ${NUM_IKIND}")
set (${HDF_PREFIX}_H5CONFIG_F_IKIND "INTEGER, DIMENSION(1:num_ikinds) :: ikind = (/${pac_validIntKinds}/)")

message (STATUS "....NUMBER OF INTEGER KINDS FOUND ${PAC_FORTRAN_NUM_INTEGER_KINDS}")
message (STATUS "....REAL KINDS FOUND ${PAC_FC_ALL_REAL_KINDS}")
message (STATUS "....INTEGER KINDS FOUND ${PAC_FC_ALL_INTEGER_KINDS}")
message (STATUS "....MAX DECIMAL PRECISION ${${HDF_PREFIX}_PAC_FC_MAX_REAL_PRECISION}")

if (${HAVE_ISO_FORTRAN_ENV})

  list (GET PROG_OUTPUT 5 NUM_LKIND)
  set (PAC_FORTRAN_NUM_LOGICAL_KINDS "${NUM_LKIND}")

  list (GET PROG_OUTPUT 6 pac_validLogicalKinds)
  # If the list is empty then something went wrong.
  if (NOT pac_validLogicalKinds)
      message (FATAL_ERROR "Failed to find available LOGICAL KINDs for Fortran")
  endif ()

  set (PAC_FC_ALL_LOGICAL_KINDS "\{${pac_validLogicalKinds}\}")
  message (STATUS "....LOGICAL KINDS FOUND ${PAC_FC_ALL_LOGICAL_KINDS}")

# ********************
# LOGICAL KIND FOR MPI
# ********************
  if (HDF5_ENABLE_PARALLEL AND BUILD_TESTING)
    string (REGEX REPLACE "," ";" VAR "${pac_validLogicalKinds}")

    set(CMAKE_REQUIRED_QUIET TRUE)
    set(save_CMAKE_Fortran_FLAGS ${CMAKE_Fortran_FLAGS})
    if (CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
       set(CMAKE_Fortran_FLAGS "-warn error")
    endif ()

    foreach (KIND ${VAR})
      unset(MPI_LOGICAL_KIND CACHE)
      set (PROG_SRC
      "
          PROGRAM main
             USE MPI
             IMPLICIT NONE
             LOGICAL(KIND=${KIND}) :: flag
             INTEGER(KIND=MPI_INTEGER_KIND) :: info_ret, mpierror
             CHARACTER(LEN=3) :: info_val
             CALL mpi_info_get(info_ret,\"foo\", 3_MPI_INTEGER_KIND, info_val, flag, mpierror)
          END
       "
      )
      check_fortran_source_compiles (${PROG_SRC} MPI_LOGICAL_KIND SRC_EXT f90)

      if (MPI_LOGICAL_KIND)
        set (${HDF_PREFIX}_MPI_LOGICAL_KIND ${KIND})
        message (STATUS "....FORTRAN LOGICAL KIND for MPI is ${KIND}")
      endif ()
    endforeach ()
    if (${HDF_PREFIX}_MPI_LOGICAL_KIND STREQUAL "")
       message (FATAL_ERROR "Failed to determine LOGICAL KIND for MPI")
    endif ()
    set(CMAKE_REQUIRED_QUIET FALSE)
    set(CMAKE_Fortran_FLAGS ${save_CMAKE_Fortran_FLAGS})
  endif()
endif()

#-----------------------------------------------------------------------------
# Determine the available KINDs for REALs and INTEGERs
#-----------------------------------------------------------------------------
# **********
# INTEGERS
# **********
string (REGEX REPLACE "," ";" VAR "${pac_validIntKinds}")

foreach (KIND ${VAR})
  set (PROG_SRC_${KIND}
  "
       PROGRAM main
          USE ISO_C_BINDING
          USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY : stdout=>OUTPUT_UNIT
          IMPLICIT NONE
          INTEGER (KIND=${KIND}) a
          WRITE(stdout,'(I0)') ${FC_SIZEOF_A}
       END
   "
  )
  FORTRAN_RUN("INTEGER KIND SIZEOF" ${PROG_SRC_${KIND}} XX YY VALIDINTKINDS_RESULT_${KIND} PROG_OUTPUT1)
  string (REGEX REPLACE "[\r\n]+" "" PROG_OUTPUT1 "${PROG_OUTPUT1}")
  set (pack_int_sizeof "${pack_int_sizeof} ${PROG_OUTPUT1},")
endforeach ()

if (pack_int_sizeof STREQUAL "")
   message (FATAL_ERROR "Failed to find available INTEGER KINDs for Fortran")
endif ()

string (STRIP ${pack_int_sizeof} pack_int_sizeof)

#Remove trailing comma
string (REGEX REPLACE ",$" "" pack_int_sizeof "${pack_int_sizeof}")
#Remove spaces
string (REGEX REPLACE " " "" pack_int_sizeof "${pack_int_sizeof}")

set (PAC_FC_ALL_INTEGER_KINDS_SIZEOF "\{${pack_int_sizeof}\}")

message (VERBOSE "....FOUND SIZEOF for INTEGER KINDs ${PAC_FC_ALL_INTEGER_KINDS_SIZEOF}")
# **********
# REALS
# **********
string (REGEX REPLACE "," ";" VAR "${pac_validRealKinds}")

#find the maximum kind of the real
list (LENGTH VAR LEN_VAR)
math (EXPR _LEN "${LEN_VAR}-1")
list (GET VAR ${_LEN} max_real_fortran_kind)

foreach (KIND ${VAR} )
  set (PROG_SRC2_${KIND}
  "
       PROGRAM main
          USE ISO_C_BINDING
          USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY : stdout=>OUTPUT_UNIT
          IMPLICIT NONE
          REAL (KIND=${KIND}) a
          WRITE(stdout,'(I0)') ${FC_SIZEOF_A}
       END
  "
  )
  FORTRAN_RUN ("REAL KIND SIZEOF" ${PROG_SRC2_${KIND}} XX YY VALIDREALKINDS_RESULT_${KIND} PROG_OUTPUT2)
  string (REGEX REPLACE "[\r\n]+" "" PROG_OUTPUT2 "${PROG_OUTPUT2}")
  set (pack_real_sizeof "${pack_real_sizeof} ${PROG_OUTPUT2},")
endforeach ()

if (pack_real_sizeof STREQUAL "")
   message (FATAL_ERROR "Failed to find available REAL KINDs for Fortran")
endif ()

string(STRIP ${pack_real_sizeof} pack_real_sizeof)

#Remove trailing comma
string (REGEX REPLACE ",$" "" pack_real_sizeof "${pack_real_sizeof}")
#Remove spaces
string (REGEX REPLACE " " "" pack_real_sizeof "${pack_real_sizeof}")

set (${HDF_PREFIX}_H5CONFIG_F_RKIND_SIZEOF "INTEGER, DIMENSION(1:num_rkinds) :: rkind_sizeof = (/${pack_real_sizeof}/)")

message (STATUS "....FOUND SIZEOF for REAL KINDs \{${pack_real_sizeof}\}")

set (PAC_FC_ALL_REAL_KINDS_SIZEOF "\{${pack_real_sizeof}\}")

#find the maximum kind of the real
string (REGEX REPLACE "," ";" VAR "${pack_real_sizeof}")
list (LENGTH VAR LEN_VAR)
math (EXPR _LEN "${LEN_VAR}-1")
list (GET VAR ${_LEN} max_real_fortran_sizeof)

#-----------------------------------------------------------------------------
# Find sizeof of native kinds
#-----------------------------------------------------------------------------
set (PROG_SRC3
  "
       PROGRAM main
          USE ISO_C_BINDING
          USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY : stdout=>OUTPUT_UNIT
          IMPLICIT NONE
          INTEGER a
          REAL b
          DOUBLE PRECISION c
          WRITE(stdout,*) ${FC_SIZEOF_A}
          WRITE(stdout,*) kind(a)
          WRITE(stdout,*) ${FC_SIZEOF_B}
          WRITE(stdout,*) kind(b)
          WRITE(stdout,*) ${FC_SIZEOF_C}
          WRITE(stdout,*) kind(c)
       END
  "
)
FORTRAN_RUN ("SIZEOF NATIVE KINDs" ${PROG_SRC3} XX YY PAC_SIZEOF_NATIVE_KINDS_RESULT PROG_OUTPUT3)
# The output from the above program will be:
#    -- LINE 1 --  sizeof INTEGER
#    -- LINE 2 --  kind of INTEGER
#    -- LINE 3 --  sizeof REAL
#    -- LINE 4 --  kind of REAL
#    -- LINE 5 --  sizeof DOUBLE PRECISION
#    -- LINE 6 --  kind of DOUBLE PRECISION
#
# Convert the string to a list of strings by replacing the carriage return with a semicolon
string (REGEX REPLACE "[\r\n]+" ";" PROG_OUTPUT3 "${PROG_OUTPUT3}")

list (GET PROG_OUTPUT3 0 PAC_FORTRAN_NATIVE_INTEGER_SIZEOF)
list (GET PROG_OUTPUT3 1 PAC_FORTRAN_NATIVE_INTEGER_KIND)
list (GET PROG_OUTPUT3 2 PAC_FORTRAN_NATIVE_REAL_SIZEOF)
list (GET PROG_OUTPUT3 3 PAC_FORTRAN_NATIVE_REAL_KIND)
list (GET PROG_OUTPUT3 4 PAC_FORTRAN_NATIVE_DOUBLE_SIZEOF)
list (GET PROG_OUTPUT3 5 PAC_FORTRAN_NATIVE_DOUBLE_KIND)

if (NOT PAC_FORTRAN_NATIVE_INTEGER_SIZEOF)
   message (FATAL_ERROR "Failed to find SIZEOF NATIVE INTEGER KINDs for Fortran")
endif ()
if (NOT PAC_FORTRAN_NATIVE_REAL_SIZEOF)
   message (FATAL_ERROR "Failed to find SIZEOF NATIVE REAL KINDs for Fortran")
endif ()
if (NOT PAC_FORTRAN_NATIVE_DOUBLE_SIZEOF)
   message (FATAL_ERROR "Failed to find SIZEOF NATIVE DOUBLE KINDs for Fortran")
endif ()
if (NOT PAC_FORTRAN_NATIVE_INTEGER_KIND)
   message (FATAL_ERROR "Failed to find KIND of NATIVE INTEGER for Fortran")
endif ()
if (NOT PAC_FORTRAN_NATIVE_REAL_KIND)
   message (FATAL_ERROR "Failed to find KIND of NATIVE REAL for Fortran")
endif ()
if (NOT PAC_FORTRAN_NATIVE_DOUBLE_KIND)
   message (FATAL_ERROR "Failed to find KIND of NATIVE DOUBLE for Fortran")
endif ()

set (${HDF_PREFIX}_FORTRAN_SIZEOF_LONG_DOUBLE ${${HDF_PREFIX}_SIZEOF_LONG_DOUBLE})

# Remove the invalid kind from the list
if (${${HDF_PREFIX}_HAVE_FLOAT128})
   if (NOT(16 EQUAL ${max_real_fortran_sizeof}) AND NOT(${${HDF_PREFIX}_FORTRAN_SIZEOF_LONG_DOUBLE} EQUAL ${max_real_fortran_sizeof})
       # Account for the fact that the C compiler can have 16-byte __float128 and the fortran compiler only has 8-byte doubles,
       # so we don't want to remove the 8-byte fortran doubles.
       AND NOT(${PAC_FORTRAN_NATIVE_DOUBLE_SIZEOF} EQUAL ${max_real_fortran_sizeof}))
     message (WARNING "
          Fortran REAL(KIND=${max_real_fortran_kind}) is $max_real_fortran_sizeof Bytes, but no corresponding C float type exists of that size
                                           !!! Fortran interfaces will not be generated for REAL(KIND=${max_real_fortran_kind}) !!!")
     string (REGEX REPLACE ",[0-9]+}" "}" PAC_FC_ALL_REAL_KINDS ${PAC_FC_ALL_REAL_KINDS})
     string (REGEX REPLACE ",[0-9]+}" "}" PAC_FC_ALL_REAL_KINDS_SIZEOF ${PAC_FC_ALL_REAL_KINDS_SIZEOF})
     math (EXPR NUM_RKIND "${NUM_RKIND} - 1")
   endif ()
endif ()

set (${HDF_PREFIX}_H5CONFIG_F_NUM_RKIND "INTEGER, PARAMETER :: num_rkinds = ${NUM_RKIND}")

string (REGEX REPLACE "{" "" OUT_VAR1 ${PAC_FC_ALL_REAL_KINDS})
string (REGEX REPLACE "}" "" OUT_VAR1 ${OUT_VAR1})
set (${HDF_PREFIX}_H5CONFIG_F_RKIND "INTEGER, DIMENSION(1:num_rkinds) :: rkind = (/${OUT_VAR1}/)")

string (REGEX REPLACE "{" "" OUT_VAR2 ${PAC_FC_ALL_REAL_KINDS_SIZEOF})
string (REGEX REPLACE "}" "" OUT_VAR2 ${OUT_VAR2})
set (${HDF_PREFIX}_H5CONFIG_F_RKIND_SIZEOF "INTEGER, DIMENSION(1:num_rkinds) :: rkind_sizeof = (/${OUT_VAR2}/)")

# Setting definition if there is a 16 byte fortran integer
string (FIND "${PAC_FC_ALL_INTEGER_KINDS_SIZEOF}" "16" pos)
if (${pos} EQUAL -1)
  set (${HDF_PREFIX}_HAVE_Fortran_INTEGER_SIZEOF_16 0)
else ()
  set (${HDF_PREFIX}_HAVE_Fortran_INTEGER_SIZEOF_16 1)
endif ()
