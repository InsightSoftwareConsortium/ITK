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
# grepTest.cmake executes a command and captures the output in a file. File is then compared
# against a reference file. Exit status of command can also be compared.

# arguments checking
if (NOT TEST_PROGRAM)
  message (FATAL_ERROR "Require TEST_PROGRAM to be defined")
endif ()
#if (NOT TEST_ARGS)
#  message (STATUS "Require TEST_ARGS to be defined")
#endif ()
if (NOT TEST_FOLDER)
  message (FATAL_ERROR "Require TEST_FOLDER to be defined")
endif ()
if (NOT TEST_OUTPUT)
  message (FATAL_ERROR "Require TEST_OUTPUT to be defined")
endif ()
#if (NOT TEST_EXPECT)
#  message (STATUS "Require TEST_EXPECT to be defined")
#endif ()
if (NOT TEST_FILTER)
  message (STATUS "Optional TEST_FILTER to be defined")
endif ()
if (NOT TEST_REFERENCE)
  message (FATAL_ERROR "Require TEST_REFERENCE to be defined")
endif ()

if (EXISTS "${TEST_FOLDER}/${TEST_OUTPUT}")
  file (REMOVE ${TEST_FOLDER}/${TEST_OUTPUT})
endif ()

if (EXISTS "${TEST_FOLDER}/${TEST_OUTPUT}.err")
  file (REMOVE ${TEST_FOLDER}/${TEST_OUTPUT}.err)
endif ()

message (STATUS "COMMAND: ${TEST_EMULATOR} ${TEST_PROGRAM} ${TEST_ARGS}")

if (TEST_LIBRARY_DIRECTORY)
  if (WIN32 OR MINGW)
    set (ENV{PATH} "$ENV{PATH};${TEST_LIBRARY_DIRECTORY}")
  else ()
    set (ENV{LD_LIBRARY_PATH} "$ENV{LD_LIBRARY_PATH}:${TEST_LIBRARY_DIRECTORY}")
  endif ()
endif ()

if (TEST_ENV_VAR)
  set (ENV{${TEST_ENV_VAR}} "${TEST_ENV_VALUE}")
  #message (STATUS "ENV:${TEST_ENV_VAR}=$ENV{${TEST_ENV_VAR}}")
endif ()

# run the test program, capture the stdout/stderr and the result var
execute_process (
    COMMAND ${TEST_EMULATOR} ${TEST_PROGRAM} ${TEST_ARGS}
    WORKING_DIRECTORY ${TEST_FOLDER}
    RESULT_VARIABLE TEST_RESULT
    OUTPUT_FILE ${TEST_OUTPUT}
    ERROR_FILE ${TEST_OUTPUT}.err
    OUTPUT_VARIABLE TEST_OUT
    ERROR_VARIABLE TEST_ERROR
)

message (STATUS "COMMAND Result: ${TEST_RESULT}")

message (STATUS "COMMAND Error: ${TEST_ERROR}")

# remove special output
file (READ ${TEST_FOLDER}/${TEST_OUTPUT} TEST_STREAM)
string (FIND TEST_STREAM "_pmi_alps" "${TEST_FIND_RESULT}")
if (TEST_FIND_RESULT GREATER 0)
  string (REGEX REPLACE "^.*_pmi_alps[^\n]+\n" "" TEST_STREAM "${TEST_STREAM}")
  file (WRITE ${TEST_FOLDER}/${TEST_OUTPUT} ${TEST_STREAM})
endif ()

# if the TEST_ERRREF exists grep the error output with the error reference
if (TEST_ERRREF)
  # if the .err file exists grep the error output with the error reference before comparing stdout
  if (EXISTS "${TEST_FOLDER}/${TEST_OUTPUT}.err")
    file (READ ${TEST_FOLDER}/${TEST_OUTPUT}.err TEST_ERR_STREAM)

    # TEST_ERRREF should always be matched
    string (REGEX MATCH "${TEST_ERRREF}" TEST_MATCH ${TEST_ERR_STREAM})
    string (COMPARE EQUAL "${TEST_ERRREF}" "${TEST_MATCH}" TEST_RESULT)
    if (NOT TEST_RESULT)
      message (FATAL_ERROR "Failed: The error output of ${TEST_PROGRAM} did not contain ${TEST_ERRREF}")
    endif ()
  endif ()

  #always compare output file to reference unless this must be skipped
  if (NOT TEST_SKIP_COMPARE)
    if (EXISTS "${TEST_FOLDER}/${TEST_REFERENCE}")
      if (WIN32 OR MINGW)
        configure_file(${TEST_FOLDER}/${TEST_REFERENCE} ${TEST_FOLDER}/${TEST_REFERENCE}.tmp NEWLINE_STYLE CRLF)
        if (EXISTS "${TEST_FOLDER}/${TEST_REFERENCE}.tmp")
          file(RENAME ${TEST_FOLDER}/${TEST_REFERENCE}.tmp ${TEST_FOLDER}/${TEST_REFERENCE})
        endif ()
        #file (READ ${TEST_FOLDER}/${TEST_REFERENCE} TEST_STREAM)
        #file (WRITE ${TEST_FOLDER}/${TEST_REFERENCE} "${TEST_STREAM}")
      endif ()
      if (NOT TEST_SORT_COMPARE)
        # now compare the output with the reference
        execute_process (
            COMMAND ${CMAKE_COMMAND} -E compare_files ${TEST_FOLDER}/${TEST_OUTPUT} ${TEST_FOLDER}/${TEST_REFERENCE}
            RESULT_VARIABLE TEST_RESULT
        )
      else ()
        file (STRINGS ${TEST_FOLDER}/${TEST_OUTPUT} v1)
        file (STRINGS ${TEST_FOLDER}/${TEST_REFERENCE} v2)
        list (SORT v1)
        list (SORT v2)
        if (NOT v1 STREQUAL v2)
          set(TEST_RESULT 1)
        endif ()
      endif ()

      if (TEST_RESULT)
        set (TEST_RESULT 0)
        file (STRINGS ${TEST_FOLDER}/${TEST_OUTPUT} test_act)
        list (LENGTH test_act len_act)
        file (STRINGS ${TEST_FOLDER}/${TEST_REFERENCE} test_ref)
        list (LENGTH test_ref len_ref)
        if (len_act GREATER 0 AND len_ref GREATER 0)
          math (EXPR _FP_LEN "${len_ref} - 1")
          foreach (line RANGE 0 ${_FP_LEN})
            list (GET test_act ${line} str_act)
            list (GET test_ref ${line} str_ref)
            if (NOT str_act STREQUAL str_ref)
              if (str_act)
                set (TEST_RESULT 1)
                message (STATUS "line = ${line}\n***ACTUAL: ${str_act}\n****REFER: ${str_ref}\n")
              endif ()
            endif ()
          endforeach ()
        else ()
          if (len_act EQUAL 0)
            message (STATUS "COMPARE Failed: ${TEST_FOLDER}/${TEST_OUTPUT} is empty")
          endif ()
          if (len_ref EQUAL 0)
            message (STATUS "COMPARE Failed: ${TEST_FOLDER}/${TEST_REFERENCE} is empty")
          endif ()
        endif ()
        if (NOT len_act EQUAL len_ref)
          set (TEST_RESULT 1)
        endif ()
      endif ()

      message (STATUS "COMPARE Result: ${TEST_RESULT}")

      # again, if return value is !=0 scream and shout
      if (TEST_RESULT)
        message (FATAL_ERROR "Failed: The output of ${TEST_OUTPUT} did not match ${TEST_REFERENCE}")
      endif ()
    endif ()
  endif ()
else ()
  # else grep the output with the reference
  file (READ ${TEST_FOLDER}/${TEST_OUTPUT} TEST_STREAM)

  # TEST_REFERENCE should always be matched
  string (REGEX MATCH "${TEST_REFERENCE}" TEST_MATCH ${TEST_STREAM})
  string (COMPARE EQUAL "${TEST_REFERENCE}" "${TEST_MATCH}" TEST_RESULT)
  if (NOT TEST_RESULT)
    message (FATAL_ERROR "Failed: The output of ${TEST_PROGRAM} did not contain ${TEST_REFERENCE}")
  endif ()
endif ()


if (TEST_FILTER)
  string (REGEX MATCH "${TEST_FILTER}" TEST_MATCH ${TEST_STREAM})
  if (TEST_EXPECT)
    # TEST_EXPECT (1) interprets TEST_FILTER as; NOT to match
    string (LENGTH "${TEST_MATCH}" TEST_RESULT)
    if (TEST_RESULT)
      message (FATAL_ERROR "Failed: The output of ${TEST_PROGRAM} did contain ${TEST_FILTER}")
    endif ()
  endif ()
endif ()

# everything went fine...
message (STATUS "Passed: The output of ${TEST_PROGRAM} matched")

