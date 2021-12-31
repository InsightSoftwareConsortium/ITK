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
# userblockTest.cmake executes a command and captures the output in a file. File is then compared
# against a reference file. Exit status of command can also be compared.

# arguments checking
if (NOT TEST_PROGRAM)
  message (FATAL_ERROR "Require TEST_PROGRAM tellub to be defined")
endif ()
if (NOT TEST_GET_PROGRAM)
  message (FATAL_ERROR "Require TEST_GET_PROGRAM getub to be defined")
endif ()
if (NOT TEST_FOLDER)
  message (FATAL_ERROR "Require TEST_FOLDER to be defined")
endif ()
if (NOT TEST_HFILE)
  message (FATAL_ERROR "Require TEST_HFILE the hdf file to be defined")
endif ()
if (NOT TEST_UFILE)
  message (FATAL_ERROR "Require TEST_UFILE the ub file to be defined")
endif ()
if (NOT TEST_CHECKUB)
  message (STATUS "Require TEST_CHECKUB - YES or NO - to be defined")
endif ()
#if (NOT TEST_EXPECT)
#  message (STATUS "Require TEST_EXPECT to be defined")
#endif ()
#if (NOT TEST_OFILE)
#  message (FATAL_ERROR "Require TEST_OFILE the original hdf file to be defined")
#endif ()

set (TEST_U_STRING_LEN 0)
set (TEST_O_STRING_LEN 0)
set (TEST_H_STRING_LEN 0)
set (TEST_STRING_SIZE 0)

if (TEST_CHECKUB STREQUAL "YES")
  # find the length of the user block to check
  #s1=`cat $ufile | wc -c | sed -e 's/ //g'`
  file (STRINGS ${TEST_FOLDER}/${TEST_UFILE} TEST_U_STRING)
  string (LENGTH ${TEST_U_STRING} TEST_U_STRING_LEN)

  # Get the size of the original user block, if any.
  if (TEST_OFILE)
    # 'tellub' calls H5Fget_user_block to get the size
    #  of the user block
    #s2=`$JAM_BIN/tellub $origfile`
    execute_process (
        COMMAND ${TEST_EMULATOR} ${TEST_PROGRAM} ${TEST_OFILE}
        WORKING_DIRECTORY ${TEST_FOLDER}
        RESULT_VARIABLE TEST_RESULT
        OUTPUT_FILE ${TEST_HFILE}.len.txt
        OUTPUT_VARIABLE TEST_OUT
        ERROR_VARIABLE TEST_ERROR
    )
    if (TEST_RESULT)
      message (FATAL_ERROR "Failed: The output of ${TEST_PROGRAM} ${TEST_OFILE} is: ${TEST_ERROR}")
    endif ()
    file (READ ${TEST_HFILE}.len.txt TEST_O_STRING_LEN)
  endif ()

  math( EXPR TEST_STRING_SIZE "${TEST_U_STRING_LEN} + ${TEST_O_STRING_LEN}" )

  if (TEST_O_STRING_LEN)
    #$JAM_BIN/getub -c $s2 $origfile > $cmpfile
    execute_process (
        COMMAND ${TEST_EMULATOR} ${TEST_GET_PROGRAM} -c ${TEST_O_STRING_LEN} ${TEST_OFILE}
        WORKING_DIRECTORY ${TEST_FOLDER}
        RESULT_VARIABLE TEST_RESULT
        OUTPUT_FILE ${TEST_HFILE}-ub.cmp
        OUTPUT_VARIABLE TEST_OUT
        ERROR_VARIABLE TEST_ERROR
        OUTPUT_STRIP_TRAILING_WHITESPACE
    )
    #cat $ufile >> $cmpfile
    file (STRINGS ${TEST_UFILE} TEST_STREAM NEWLINE_CONSUME)
    file (APPEND ${TEST_HFILE}-ub.cmp "${TEST_STREAM}")
  else ()
    file (STRINGS ${TEST_UFILE} TEST_STREAM NEWLINE_CONSUME)
    file (WRITE ${TEST_HFILE}-ub.cmp ${TEST_STREAM})
  endif ()

  #$JAM_BIN/getub -c $size $hfile > $tfile
  execute_process (
      COMMAND ${TEST_EMULATOR} ${TEST_GET_PROGRAM} -c ${TEST_STRING_SIZE} ${TEST_HFILE}
      WORKING_DIRECTORY ${TEST_FOLDER}
      RESULT_VARIABLE TEST_RESULT
      OUTPUT_FILE ${TEST_HFILE}.cmp
      OUTPUT_VARIABLE TEST_OUT
      ERROR_VARIABLE TEST_ERROR
      OUTPUT_STRIP_TRAILING_WHITESPACE
  )

  # now compare the outputs
  execute_process (
      COMMAND ${CMAKE_COMMAND} -E compare_files ${CMAKE_IGNORE_EOL} ${TEST_HFILE}-ub.cmp ${TEST_HFILE}.cmp
      RESULT_VARIABLE TEST_RESULT
  )

  message (STATUS "COMPARE Result: ${TEST_RESULT}: ${TEST_STRING_SIZE}=${TEST_U_STRING_LEN}+${TEST_O_STRING_LEN}")
  # if the return value is !=${TEST_EXPECT} bail out
  if (NOT TEST_RESULT EQUAL TEST_EXPECT)
    message (FATAL_ERROR "Failed: The output of ${TEST_HFILE}-ub did not match ${TEST_HFILE}.\n${TEST_ERROR}")
  endif ()
else ()
    # call 'ubsize' to get the size of the user block
    #ubsize=`$JAM_BIN/tellub $hfile`
    execute_process (
        COMMAND ${TEST_EMULATOR} ${TEST_PROGRAM} ${TEST_HFILE}
        WORKING_DIRECTORY ${TEST_FOLDER}
        RESULT_VARIABLE TEST_H_STRING_LEN
        OUTPUT_VARIABLE TEST_OUT
        ERROR_VARIABLE TEST_ERROR
    )
  if (TEST_H_STRING_LEN)
    message (FATAL_ERROR "Failed: The output of ${TEST_HFILE} was NOT empty")
  endif ()
endif ()

# everything went fine...
message (STATUS "Passed: The output of CHECK matched expectation")

