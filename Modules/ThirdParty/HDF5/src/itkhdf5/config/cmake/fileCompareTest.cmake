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
# fileCompareTest.cmake compares two files.

# arguments checking
if (NOT TEST_FOLDER)
  message (FATAL_ERROR "Require TEST_FOLDER to be defined")
endif ()
if (NOT TEST_ONEFILE)
  message (FATAL_ERROR "Require TEST_ONEFILE the first file to be defined")
endif ()
if (NOT TEST_TWOFILE)
  message (FATAL_ERROR "Require TEST_TWOFILE the second file to be defined")
endif ()
if (NOT TEST_FUNCTION)
  message (FATAL_ERROR "Require TEST_FUNCTION (LT,LTEQ,EQ,GTEQ,GT) to be defined")
endif ()

set (TEST_ONE_SIZE 0)
set (TEST_TWO_SIZE 0)
set (TEST_ONE_STRING 0)
set (TEST_TWO_STRING 0)
set (TEST_ONE_STRING_LEN 0)
set (TEST_TWO_STRING_LEN 0)

if (TEST_STRINGS STREQUAL "YES")
  # find the length of the first file
  #s1=`cat $ufile | wc -c | sed -e 's/ //g'`
  file (STRINGS ${TEST_FOLDER}/${TEST_ONEFILE} TEST_ONE_STRING)
  string (LENGTH ${TEST_ONE_STRING} TEST_ONE_STRING_LEN)

  # Get the size of the second file.
  file (STRINGS ${TEST_FOLDER}/${TEST_TWOFILE} TEST_TWO_STRING)
  string (LENGTH ${TEST_TWO_STRING} TEST_TWO_STRING_LEN)

  math (EXPR TEST_STRING_SIZE "${TEST_ONE_STRING_LEN} - ${TEST_TWO_STRING_LEN}" )

  # now compare the outputs
  execute_process (
      COMMAND ${CMAKE_COMMAND} -E compare_files --ignore-eol ${TEST_FOLDER}/${TEST_ONEFILE} ${TEST_FOLDER}/${TEST_TWOFILE}
      RESULT_VARIABLE TEST_RESULT
  )

  message (VERBOSE "COMPARE Result: ${TEST_RESULT}: ${TEST_STRING_SIZE}=${TEST_U_STRING_LEN}-${TEST_O_STRING_LEN}")
  # if the return value is !=${TEST_EXPECT} bail out
  if (NOT TEST_RESULT EQUAL TEST_EXPECT)
    message (FATAL_ERROR "Failed: The output of ${TEST_FOLDER}/${TEST_ONEFILE} did not match ${TEST_FOLDER}/${TEST_TWOFILE}.\n${TEST_ERROR}")
  endif ()
else ()
  file (SIZE ${TEST_FOLDER}/${TEST_ONEFILE} TEST_ONE_SIZE)
  file (SIZE ${TEST_FOLDER}/${TEST_TWOFILE} TEST_TWO_SIZE)
  if (TEST_FUNCTION MATCHES "LT")
    if (TEST_ONE_SIZE LESS TEST_TWO_SIZE)
      message (VERBOSE "Passed: The size of ${TEST_FOLDER}/${TEST_ONEFILE} was less ${TEST_FOLDER}/${TEST_TWOFILE}")
    else ()
      message (FATAL_ERROR "The size of ${TEST_FOLDER}/${TEST_ONEFILE} was NOT less ${TEST_FOLDER}/${TEST_TWOFILE}")
    endif ()
  elseif (TEST_FUNCTION MATCHES "LTEQ")
    if (TEST_ONE_SIZE LESS_EQUAL TEST_TWO_SIZE)
      message (VERBOSE "Passed: The size of ${TEST_FOLDER}/${TEST_ONEFILE} was less or equal ${TEST_FOLDER}/${TEST_TWOFILE}")
    else ()
      message (FATAL_ERROR "The size of ${TEST_FOLDER}/${TEST_ONEFILE} was NOT less or equal ${TEST_FOLDER}/${TEST_TWOFILE}")
    endif ()
  elseif (TEST_FUNCTION MATCHES "EQ")
    if (TEST_ONE_SIZE LESS_EQUAL TEST_TWO_SIZE)
      message (VERBOSE "Passed: The size of ${TEST_FOLDER}/${TEST_ONEFILE} was equal ${TEST_FOLDER}/${TEST_TWOFILE}")
    else ()
      message (FATAL_ERROR "The size of ${TEST_FOLDER}/${TEST_ONEFILE} was NOT equal ${TEST_FOLDER}/${TEST_TWOFILE}")
    endif ()
  elseif (TEST_FUNCTION MATCHES "GTEQ")
    if (TEST_ONE_SIZE LESS_EQUAL TEST_TWO_SIZE)
      message (VERBOSE "Passed: The size of ${TEST_FOLDER}/${TEST_ONEFILE} was greater or equal ${TEST_FOLDER}/${TEST_TWOFILE}")
    else ()
      message (FATAL_ERROR "The size of ${TEST_FOLDER}/${TEST_ONEFILE} was NOT greater or equal ${TEST_FOLDER}/${TEST_TWOFILE}")
    endif ()
  elseif (TEST_FUNCTION MATCHES "GT")
    if (TEST_ONE_SIZE LESS_EQUAL TEST_TWO_SIZE)
      message (VERBOSE "Passed: The size of ${TEST_FOLDER}/${TEST_ONEFILE} was greater ${TEST_FOLDER}/${TEST_TWOFILE}")
    else ()
      message (FATAL_ERROR "The size of ${TEST_FOLDER}/${TEST_ONEFILE} was NOT greater ${TEST_FOLDER}/${TEST_TWOFILE}")
    endif ()
  else ()
    message (FATAL_ERROR "Failed: Incorrect test size compare command provided.\n${TEST_ERROR}")
  endif ()
endif ()

# everything went fine...

