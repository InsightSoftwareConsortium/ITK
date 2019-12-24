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
# This file provides functions for HDF5 specific Fortran support.
#
#-------------------------------------------------------------------------------
enable_language (Fortran)

set (HDF_PREFIX "H5")
include (CheckFortranFunctionExists)

# The provided CMake Fortran macros don't provide a general compile/run function
# so this one is used.
#-----------------------------------------------------------------------------
macro (FORTRAN_RUN FUNCTION_NAME SOURCE_CODE RUN_RESULT_VAR1 COMPILE_RESULT_VAR1 RETURN_VAR)
    message (STATUS "Detecting Fortran ${FUNCTION_NAME}")
    file (WRITE
        ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/testFortranCompiler1.f90
        "${SOURCE_CODE}"
    )
    TRY_RUN (RUN_RESULT_VAR COMPILE_RESULT_VAR
        ${CMAKE_BINARY_DIR}
        ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/testFortranCompiler1.f90
        LINK_LIBRARIES "${HDF5_REQUIRED_LIBRARIES}"
    )

    if (${COMPILE_RESULT_VAR})
      set(${RETURN_VAR} ${RUN_RESULT_VAR})
      if (${RUN_RESULT_VAR} MATCHES 0)
        message (STATUS "Testing Fortran ${FUNCTION_NAME} - OK")
        file (APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeOutput.log
            "Determining if the Fortran ${FUNCTION_NAME} exists passed\n"
        )
      else ()
        message (STATUS "Testing Fortran ${FUNCTION_NAME} - Fail")
        file (APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
            "Determining if the Fortran ${FUNCTION_NAME} exists failed: ${RUN_RESULT_VAR}\n"
        )
      endif ()
    else ()
        message (STATUS "Compiling Fortran ${FUNCTION_NAME} - Fail")
        file (APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
            "Determining if the Fortran ${FUNCTION_NAME} compiles failed: ${COMPILE_RESULT_VAR}\n"
        )
        set(${RETURN_VAR} ${COMPILE_RESULT_VAR})
    endif ()
endmacro ()

# Read source line beginning at the line matching Input:"START" and ending at the line matching Input:"END"
macro (READ_SOURCE SOURCE_START SOURCE_END RETURN_VAR)
  file (READ "${HDF5_SOURCE_DIR}/m4/aclocal_fc.f90" SOURCE_MASTER)
  string (REGEX MATCH "${SOURCE_START}[\\\t\\\n\\\r[].+]*${SOURCE_END}" SOURCE_CODE ${SOURCE_MASTER})
  set (RETURN_VAR "${SOURCE_CODE}")
endmacro ()

#-----------------------------------------------------------------------------
#  Check to see C_LONG_DOUBLE is available

READ_SOURCE("PROGRAM PROG_FC_HAVE_C_LONG_DOUBLE" "END PROGRAM PROG_FC_HAVE_C_LONG_DOUBLE" SOURCE_CODE)
CHECK_FORTRAN_FEATURE(c_long_double
  "${SOURCE_CODE}"
  FORTRAN_HAVE_C_LONG_DOUBLE
)

if (${FORTRAN_HAVE_C_LONG_DOUBLE})
  set (${HDF_PREFIX}_FORTRAN_HAVE_C_LONG_DOUBLE 1)
else ()
  set (${HDF_PREFIX}_FORTRAN_HAVE_C_LONG_DOUBLE 0)
endif ()

# Check to see C_LONG_DOUBLE is different from C_DOUBLE

READ_SOURCE("MODULE type_mod" "END PROGRAM PROG_FC_C_LONG_DOUBLE_EQ_C_DOUBLE" SOURCE_CODE)
CHECK_FORTRAN_FEATURE(c_long_double
  "${SOURCE_CODE}"
  FORTRAN_C_LONG_DOUBLE_IS_UNIQUE
)
if (${FORTRAN_C_LONG_DOUBLE_IS_UNIQUE})
  set (${HDF_PREFIX}_FORTRAN_C_LONG_DOUBLE_IS_UNIQUE 1)
else ()
  set (${HDF_PREFIX}_FORTRAN_C_LONG_DOUBLE_IS_UNIQUE 0)
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

#-----------------------------------------------------------------------------
# Determine the available KINDs for REALs and INTEGERs
#-----------------------------------------------------------------------------

READ_SOURCE ("PROGRAM FC_AVAIL_KINDS" "END PROGRAM FC_AVAIL_KINDS" SOURCE_CODE)
FORTRAN_RUN ("REAL and INTEGER KINDs"
    "${SOURCE_CODE}"
    XX
    YY
    PROG_RESULT
)
# dnl The output from the above program will be:
# dnl    -- LINE 1 --  valid integer kinds (comma seperated list)
# dnl    -- LINE 2 --  valid real kinds (comma seperated list)
# dnl    -- LINE 3 --  max decimal precision for reals
# dnl    -- LINE 4 --  number of valid integer kinds
# dnl    -- LINE 5 --  number of valid real kinds

file (READ "${CMAKE_BINARY_DIR}/pac_fconftest.out" PROG_OUTPUT)
# Convert the string to a list of strings by replacing the carriage return with a semicolon
string (REGEX REPLACE "\n" ";" PROG_OUTPUT "${PROG_OUTPUT}")

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

#-----------------------------------------------------------------------------
# Determine the available KINDs for REALs and INTEGERs
#-----------------------------------------------------------------------------
# **********
# INTEGERS
# **********
string (REGEX REPLACE "," ";" VAR "${pac_validIntKinds}")

foreach (KIND ${VAR} )
  set (PROG_SRC
    "
        PROGRAM main
        USE ISO_C_BINDING
        IMPLICIT NONE
        INTEGER (KIND=${KIND}) a
        OPEN(8,FILE='pac_validIntKinds.out',FORM='formatted')
        WRITE(8,'(I0)') ${FC_SIZEOF_A}
        CLOSE(8)
        END
     "
  )
  FORTRAN_RUN("INTEGER KIND SIZEOF" ${PROG_SRC}
      XX
      YY
      PROG_RESULT1
  )
  file (READ "${CMAKE_BINARY_DIR}/pac_validIntKinds.out" PROG_OUTPUT1)
  string (REGEX REPLACE "\n" "" PROG_OUTPUT1 "${PROG_OUTPUT1}")
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

message (STATUS "....FOUND SIZEOF for INTEGER KINDs ${PAC_FC_ALL_INTEGER_KINDS_SIZEOF}")
# **********
# REALS
# **********
string (REGEX REPLACE "," ";" VAR "${pac_validRealKinds}")

#find the maximum kind of the real
list (LENGTH VAR LEN_VAR)
math (EXPR _LEN "${LEN_VAR}-1")
list (GET VAR ${_LEN} max_real_fortran_kind)

foreach (KIND ${VAR} )
  set (PROG_SRC
    "
        PROGRAM main
        USE ISO_C_BINDING
        IMPLICIT NONE
        REAL (KIND=${KIND}) a
        OPEN(8,FILE='pac_validRealKinds.out',FORM='formatted')
        WRITE(8,'(I0)') ${FC_SIZEOF_A}
        CLOSE(8)
        END
     "
  )
  FORTRAN_RUN ("REAL KIND SIZEOF" ${PROG_SRC}
      XX
      YY
      PROG_RESULT1
  )
  file (READ "${CMAKE_BINARY_DIR}/pac_validRealKinds.out" PROG_OUTPUT1)
  string (REGEX REPLACE "\n" "" PROG_OUTPUT1 "${PROG_OUTPUT1}")
  set (pack_real_sizeof "${pack_real_sizeof} ${PROG_OUTPUT1},")
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
FORTRAN_RUN ("SIZEOF NATIVE KINDs"
  "
       PROGRAM main
          USE ISO_C_BINDING
          IMPLICIT NONE
          INTEGER a
          REAL b
          DOUBLE PRECISION c
          OPEN(8,FILE='pac_sizeof_native_kinds.out',FORM='formatted')
          WRITE(8,*) ${FC_SIZEOF_A}
          WRITE(8,*) kind(a)
          WRITE(8,*) ${FC_SIZEOF_B}
          WRITE(8,*) kind(b)
          WRITE(8,*) ${FC_SIZEOF_C}
          WRITE(8,*) kind(c)
          CLOSE(8)
       END
  "
      XX
      YY
      PROG_RESULT
  )
  file (READ "${CMAKE_BINARY_DIR}/pac_sizeof_native_kinds.out" PROG_OUTPUT)
# dnl The output from the above program will be:
# dnl    -- LINE 1 --  sizeof INTEGER
# dnl    -- LINE 2 --  kind of INTEGER
# dnl    -- LINE 3 --  sizeof REAL
# dnl    -- LINE 4 --  kind of REAL
# dnl    -- LINE 5 --  sizeof DOUBLE PRECISION
# dnl    -- LINE 6 --  kind of DOUBLE PRECISION

# Convert the string to a list of strings by replacing the carriage return with a semicolon
string (REGEX REPLACE "\n" ";" PROG_OUTPUT "${PROG_OUTPUT}")

list (GET PROG_OUTPUT 0 PAC_FORTRAN_NATIVE_INTEGER_SIZEOF)
list (GET PROG_OUTPUT 1 PAC_FORTRAN_NATIVE_INTEGER_KIND)
list (GET PROG_OUTPUT 2 PAC_FORTRAN_NATIVE_REAL_SIZEOF)
list (GET PROG_OUTPUT 3 PAC_FORTRAN_NATIVE_REAL_KIND)
list (GET PROG_OUTPUT 4 PAC_FORTRAN_NATIVE_DOUBLE_SIZEOF)
list (GET PROG_OUTPUT 5 PAC_FORTRAN_NATIVE_DOUBLE_KIND)

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

# remove the invalid kind from the list
if (NOT(${${HDF_PREFIX}_SIZEOF___FLOAT128} EQUAL 0))
   if (NOT(${${HDF_PREFIX}_SIZEOF___FLOAT128} EQUAL ${max_real_fortran_sizeof})
       AND NOT(${${HDF_PREFIX}_FORTRAN_SIZEOF_LONG_DOUBLE} EQUAL ${max_real_fortran_sizeof})
       # account for the fact that the C compiler can have 16-byte __float128 and the fortran compiler only has 8-byte doubles,
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

string (REGEX REPLACE "{" "" OUT_VAR ${PAC_FC_ALL_REAL_KINDS})
string (REGEX REPLACE "}" "" OUT_VAR ${OUT_VAR})
set (${HDF_PREFIX}_H5CONFIG_F_RKIND "INTEGER, DIMENSION(1:num_rkinds) :: rkind = (/${OUT_VAR}/)")

string (REGEX REPLACE "{" "" OUT_VAR ${PAC_FC_ALL_REAL_KINDS_SIZEOF})
string (REGEX REPLACE "}" "" OUT_VAR ${OUT_VAR})
set (${HDF_PREFIX}_H5CONFIG_F_RKIND_SIZEOF "INTEGER, DIMENSION(1:num_rkinds) :: rkind_sizeof = (/${OUT_VAR}/)")

ENABLE_LANGUAGE (C)

#-----------------------------------------------------------------------------
# The provided CMake C macros don't provide a general compile/run function
# so this one is used.
#-----------------------------------------------------------------------------
macro (C_RUN FUNCTION_NAME SOURCE_CODE RETURN_VAR)
    message (STATUS "Detecting C ${FUNCTION_NAME}")
    if (HDF5_REQUIRED_LIBRARIES)
      set (CHECK_FUNCTION_EXISTS_ADD_LIBRARIES
          "-DLINK_LIBRARIES:STRING=${HDF5_REQUIRED_LIBRARIES}")
    else ()
      set (CHECK_FUNCTION_EXISTS_ADD_LIBRARIES)
    endif ()
    file (WRITE
        ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/testCCompiler1.c
        ${SOURCE_CODE}
    )
    TRY_RUN (RUN_RESULT_VAR COMPILE_RESULT_VAR
        ${CMAKE_BINARY_DIR}
        ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/testCCompiler1.c
        CMAKE_FLAGS "${CHECK_FUNCTION_EXISTS_ADD_LIBRARIES}"
        RUN_OUTPUT_VARIABLE OUTPUT_VAR
    )

    set (${RETURN_VAR} ${OUTPUT_VAR})

    #message (STATUS "* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * ")
    #message (STATUS "Test COMPILE_RESULT_VAR ${COMPILE_RESULT_VAR} ")
    #message (STATUS "* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * ")
    #message (STATUS "Test RUN_RESULT_VAR ${RUN_RESULT_VAR} ")
    #message (STATUS "* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * ")

    if (${COMPILE_RESULT_VAR})
      if (${RUN_RESULT_VAR} MATCHES 1)
        set (${RUN_RESULT_VAR} 1 CACHE INTERNAL "Have C function ${FUNCTION_NAME}")
        message (STATUS "Testing C ${FUNCTION_NAME} - OK")
        file (APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeOutput.log
            "Determining if the C ${FUNCTION_NAME} exists passed with the following output:\n"
            "${OUTPUT_VAR}\n\n"
        )
      else ()
        message (STATUS "Testing C ${FUNCTION_NAME} - Fail")
        set (${RUN_RESULT_VAR} 0 CACHE INTERNAL "Have C function ${FUNCTION_NAME}")
        file (APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
            "Determining if the C ${FUNCTION_NAME} exists failed with the following output:\n"
            "${OUTPUT_VAR}\n\n")
      endif ()
    else ()
        message (FATAL_ERROR "Compilation of C ${FUNCTION_NAME} - Failed")
    endif ()
endmacro ()

set (PROG_SRC
    "
#include <float.h>
#include <stdio.h>
#define CHECK_FLOAT128 ${${HDF_PREFIX}_SIZEOF___FLOAT128}
#if CHECK_FLOAT128!=0
# if ${${HDF_PREFIX}_HAVE_QUADMATH_H}!=0
#include <quadmath.h>
# endif
# ifdef FLT128_DIG
#define C_FLT128_DIG FLT128_DIG
# else
#define C_FLT128_DIG 0
# endif
#else
#define C_FLT128_DIG 0
#endif
#if defined (__STDC_VERSION__) && __STDC_VERSION__ >= 199901L
#define C_LDBL_DIG DECIMAL_DIG
#else
#define C_LDBL_DIG LDBL_DIG
#endif
   int main() {
       printf(\"%d\\\\n%d\\\\n\", C_LDBL_DIG, C_FLT128_DIG)\\\;
       return 1\\\;
   }
     "
)

C_RUN ("maximum decimal precision for C" ${PROG_SRC} PROG_OUTPUT)

# dnl The output from the above program will be:
# dnl    -- LINE 1 --  long double decimal precision
# dnl    -- LINE 2 --  __float128 decimal precision

# Convert the string to a list of strings by replacing the carriage return with a semicolon
string (REGEX REPLACE "\n" ";" PROG_OUTPUT "${PROG_OUTPUT}")

list (GET PROG_OUTPUT 0 LDBL_DIG)
list (GET PROG_OUTPUT 1 FLT128_DIG)

if (${HDF_PREFIX}_SIZEOF___FLOAT128 EQUAL 0 OR FLT128_DIG EQUAL 0)
  set (${HDF_PREFIX}_HAVE_FLOAT128 0)
  set (${HDF_PREFIX}_SIZEOF___FLOAT128 0)
  set (${HDF_PREFIX}_PAC_C_MAX_REAL_PRECISION ${LDBL_DIG})
else ()
  set(${HDF_PREFIX}_PAC_C_MAX_REAL_PRECISION ${FLT128_DIG})
endif ()


# Setting definition if there is a 16 byte fortran integer
string (FIND ${PAC_FC_ALL_INTEGER_KINDS_SIZEOF} "16" pos)
if (${pos} EQUAL -1)
  set (${HDF_PREFIX}_HAVE_Fortran_INTEGER_SIZEOF_16 0)
else ()
  set (${HDF_PREFIX}_HAVE_Fortran_INTEGER_SIZEOF_16 1)
endif ()
