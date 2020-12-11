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
# vfdTest.cmake executes a command and captures the output in a file. Command uses specified VFD.
# Exit status of command can also be compared.

# arguments checking
if (NOT TEST_PROGRAM)
  message (FATAL_ERROR "Require TEST_PROGRAM to be defined")
endif ()
if (NOT TEST_FOLDER)
  message (FATAL_ERROR "Require TEST_FOLDER to be defined")
endif ()
if (NOT TEST_VFD)
  message (FATAL_ERROR "Require TEST_VFD to be defined")
endif ()

if (EXISTS "${TEST_FOLDER}/${TEST_OUTPUT}")
  file (REMOVE ${TEST_FOLDER}/${TEST_OUTPUT})
endif ()

if (EXISTS "${TEST_FOLDER}/${TEST_OUTPUT}.err")
  file (REMOVE ${TEST_FOLDER}/${TEST_OUTPUT}.err)
endif ()

# if there is not an error reference file add the error output to the stdout file
#if (NOT TEST_ERRREF)
#  set (ERROR_APPEND 1)
#endif ()

message (STATUS "USING ${TEST_VFD} ON COMMAND: ${TEST_EMULATOR} ${TEST_PROGRAM} ${TEST_ARGS}")

set (ENV{HDF5_DRIVER} "${TEST_VFD}")

# run the test program, capture the stdout/stderr and the result var
execute_process (
    COMMAND ${TEST_EMULATOR} ${TEST_PROGRAM} ${TEST_ARGS}
    WORKING_DIRECTORY ${TEST_FOLDER}
    RESULT_VARIABLE TEST_RESULT
    OUTPUT_FILE ${TEST_OUTPUT}_${TEST_VFD}.out
    ERROR_FILE ${TEST_OUTPUT}_${TEST_VFD}.err
    OUTPUT_VARIABLE TEST_OUT
    ERROR_VARIABLE TEST_ERROR
)

message (STATUS "COMMAND Result: ${TEST_RESULT}")

# if the .err file exists and ERRROR_APPEND is enabled
if (ERROR_APPEND AND EXISTS "${TEST_FOLDER}/${TEST_OUTPUT}_${TEST_VFD}.err")
  file (READ ${TEST_FOLDER}/${TEST_OUTPUT}_${TEST_VFD}.err TEST_STREAM)
  file (APPEND ${TEST_FOLDER}/${TEST_OUTPUT}_${TEST_VFD}.out "${TEST_STREAM}")
endif ()

# if the return value is !=${TEST_EXPECT} bail out
if (NOT TEST_RESULT EQUAL TEST_EXPECT)
  if (NOT TEST_NOERRDISPLAY)
    if (EXISTS "${TEST_FOLDER}/${TEST_OUTPUT}_${TEST_VFD}.out")
      file (READ ${TEST_FOLDER}/${TEST_OUTPUT}_${TEST_VFD}.out TEST_STREAM)
    message (STATUS "Output USING ${TEST_VFD}:\n${TEST_STREAM}")
    endif ()
  endif ()
  message (FATAL_ERROR "Failed: Test program ${TEST_PROGRAM} exited != ${TEST_EXPECT}.\n${TEST_ERROR}")
endif ()

message (STATUS "COMMAND Error: ${TEST_ERROR}")

# everything went fine...
message (STATUS "Passed: The ${TEST_PROGRAM} program used vfd ${TEST_VFD}")
