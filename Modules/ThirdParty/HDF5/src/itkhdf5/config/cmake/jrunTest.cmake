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
# jrunTest.cmake executes a command and captures the output in a file. File is then compared
# against a reference file. Exit status of command can also be compared.
cmake_policy(SET CMP0007 NEW)

# arguments checking
if (NOT TEST_TESTER)
  message (FATAL_ERROR "Require TEST_TESTER to be defined")
endif ()
if (NOT TEST_PROGRAM)
  message (FATAL_ERROR "Require TEST_PROGRAM to be defined")
endif ()
if (NOT TEST_LIBRARY_DIRECTORY)
  message (STATUS "Require TEST_LIBRARY_DIRECTORY to be defined")
endif ()
if (NOT TEST_FOLDER)
  message (FATAL_ERROR "Require TEST_FOLDER to be defined")
endif ()
if (NOT TEST_OUTPUT)
  message (FATAL_ERROR "Require TEST_OUTPUT to be defined")
endif ()
if (NOT TEST_CLASSPATH)
  message (STATUS "Require TEST_CLASSPATH to be defined")
endif ()

if (EXISTS "${TEST_FOLDER}/${TEST_OUTPUT}")
  file (REMOVE ${TEST_FOLDER}/${TEST_OUTPUT})
endif ()

if (EXISTS "${TEST_FOLDER}/${TEST_OUTPUT}.err")
  file (REMOVE ${TEST_FOLDER}/${TEST_OUTPUT}.err)
endif ()

if (NOT TEST_LOG_LEVEL)
  set (LOG_LEVEL "info")
else ()
  set (LOG_LEVEL "${TEST_LOG_LEVEL}")
endif ()

message (STATUS "COMMAND: ${TEST_TESTER} -Xmx1024M -Dorg.slf4j.simpleLogger.defaultLog=${LOG_LEVEL} -Djava.library.path=\"${TEST_LIBRARY_DIRECTORY}\" -cp \"${TEST_CLASSPATH}\" ${TEST_ARGS} ${TEST_PROGRAM} ${ARGN}")

if (WIN32)
  set (ENV{PATH} "$ENV{PATH}\\;${TEST_LIBRARY_DIRECTORY}")
else ()
  set (ENV{LD_LIBRARY_PATH} "$ENV{LD_LIBRARY_PATH}:${TEST_LIBRARY_DIRECTORY}")
endif ()

# run the test program, capture the stdout/stderr and the result var
execute_process (
    COMMAND ${TEST_TESTER} -Xmx1024M
    -Dorg.slf4j.simpleLogger.defaultLogLevel=${LOG_LEVEL}
    -Djava.library.path=${TEST_LIBRARY_DIRECTORY}
    -cp "${TEST_CLASSPATH}" ${TEST_ARGS} ${TEST_PROGRAM}
    ${ARGN}
    WORKING_DIRECTORY ${TEST_FOLDER}
    RESULT_VARIABLE TEST_RESULT
    OUTPUT_FILE ${TEST_OUTPUT}
    ERROR_FILE ${TEST_OUTPUT}.err
    OUTPUT_VARIABLE TEST_OUT
    ERROR_VARIABLE TEST_ERROR
)

message (STATUS "COMMAND Result: ${TEST_RESULT}")

# if the .err file exists and ERRROR_APPEND is enabled
if (EXISTS "${TEST_FOLDER}/${TEST_OUTPUT}.err")
  file (READ ${TEST_FOLDER}/${TEST_OUTPUT}.err TEST_STREAM)
  list (LENGTH TEST_STREAM test_len)
  if (test_len GREATER 0)
    if (TEST_MASK_FILE)
      STRING(REGEX REPLACE "CurrentDir is [^\n]+\n" "CurrentDir is (dir name)\n" TEST_STREAM "${TEST_STREAM}")
    endif ()

    if (NOT ERROR_APPEND)
      # write back to original .err file
      file (WRITE ${TEST_FOLDER}/${TEST_OUTPUT}.err "${TEST_STREAM}")
    else ()
      # append error output to the stdout output file
      file (APPEND ${TEST_FOLDER}/${TEST_OUTPUT} "${TEST_STREAM}")
    endif ()
  endif ()
endif ()

# if the output file or the .err file needs to mask out error stack info
if (TEST_MASK_ERROR)
  if (NOT TEST_ERRREF)
    # the error stack has been appended to the output file
    file (READ ${TEST_FOLDER}/${TEST_OUTPUT} TEST_STREAM)
  else ()
    # the error stack remains in the .err file
    file (READ ${TEST_FOLDER}/${TEST_OUTPUT}.err TEST_STREAM)
  endif ()
  string (REGEX REPLACE "Time:[^\n]+\n" "Time:  XXXX\n" TEST_STREAM "${TEST_STREAM}")
  string (REGEX REPLACE "thread [0-9]*:" "thread (IDs):" TEST_STREAM "${TEST_STREAM}")
  string (REGEX REPLACE ": ([^\n]*)[.]c " ": (file name) " TEST_STREAM "${TEST_STREAM}")
  string (REGEX REPLACE " line [0-9]*" " line (number)" TEST_STREAM "${TEST_STREAM}")
  #string (REGEX REPLACE "v[1-9]*[.][0-9]*[.]" "version (number)." TEST_STREAM "${TEST_STREAM}")
  string (REGEX REPLACE "HDF5 .[1-9]*[.][0-9]*[.][0-9]*[^)]*" "HDF5 (version (number)" TEST_STREAM "${TEST_STREAM}")
  string (REGEX REPLACE "H5Eget_auto[1-2]*" "H5Eget_auto(1 or 2)" TEST_STREAM "${TEST_STREAM}")
  string (REGEX REPLACE "H5Eset_auto[1-2]*" "H5Eset_auto(1 or 2)" TEST_STREAM "${TEST_STREAM}")
  # write back the changes to the original files
  if (NOT TEST_ERRREF)
    file (WRITE ${TEST_FOLDER}/${TEST_OUTPUT} "${TEST_STREAM}")
  else ()
    file (WRITE ${TEST_FOLDER}/${TEST_OUTPUT}.err "${TEST_STREAM}")
  endif ()
endif ()

# if the return value is !=expected bail out
if (NOT TEST_RESULT EQUAL TEST_EXPECT)
  message (STATUS "ERROR OUTPUT: ${TEST_STREAM}")
  message (FATAL_ERROR "Failed: Test program ${TEST_PROGRAM} exited != 0.\n${TEST_ERROR}")
endif ()

message (STATUS "COMMAND Error: ${TEST_ERROR}")

# compare output files to references unless this must be skipped
set (TEST_COMPARE_RESULT 0)
if (NOT TEST_SKIP_COMPARE)
  if (EXISTS "${TEST_FOLDER}/${TEST_REFERENCE}")
    file (READ ${TEST_FOLDER}/${TEST_REFERENCE} TEST_STREAM)
    list (LENGTH TEST_STREAM test_len)
    if (test_len GREATER 0)
      if (WIN32)
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
            COMMAND ${CMAKE_COMMAND} -E compare_files ${CMAKE_IGNORE_EOL} ${TEST_FOLDER}/${TEST_OUTPUT} ${TEST_FOLDER}/${TEST_REFERENCE}
            RESULT_VARIABLE TEST_COMPARE_RESULT
        )
      else ()
        file (STRINGS ${TEST_FOLDER}/${TEST_OUTPUT} v1)
        file (STRINGS ${TEST_FOLDER}/${TEST_REFERENCE} v2)
        list (SORT v1)
        list (SORT v2)
        if (NOT v1 STREQUAL v2)
          set(TEST_COMPARE_RESULT 1)
        endif ()
      endif ()

      if (TEST_COMPARE_RESULT)
        set (TEST_COMPARE_RESULT 0)
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
                set (TEST_COMPARE_RESULT 1)
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
          set (TEST_COMPARE_RESULT 1)
        endif ()
      endif ()
    endif ()

    message (STATUS "COMPARE Result: ${TEST_COMPARE_RESULT}")

    # again, if return value is !=0 scream and shout
    if (TEST_COMPARE_RESULT)
      message (FATAL_ERROR "Failed: The output of ${TEST_OUTPUT} did not match ${TEST_REFERENCE}")
    endif ()
  endif ()

  # now compare the .err file with the error reference, if supplied
  set (TEST_ERRREF_RESULT 0)
  if (TEST_ERRREF)
    file (READ ${TEST_FOLDER}/${TEST_ERRREF} TEST_STREAM)
    list (LENGTH TEST_STREAM test_len)
    if (test_len GREATER 0)
      if (WIN32)
        configure_file(${TEST_FOLDER}/${TEST_ERRREF} ${TEST_FOLDER}/${TEST_ERRREF}.tmp NEWLINE_STYLE CRLF)
        if (EXISTS "${TEST_FOLDER}/${TEST_ERRREF}.tmp")
          file(RENAME ${TEST_FOLDER}/${TEST_ERRREF}.tmp ${TEST_FOLDER}/${TEST_ERRREF})
        endif ()
        #file (READ ${TEST_FOLDER}/${TEST_ERRREF} TEST_STREAM)
        #file (WRITE ${TEST_FOLDER}/${TEST_ERRREF} "${TEST_STREAM}")
      endif ()

      # now compare the error output with the error reference
      execute_process (
          COMMAND ${CMAKE_COMMAND} -E compare_files ${CMAKE_IGNORE_EOL} ${TEST_FOLDER}/${TEST_OUTPUT}.err ${TEST_FOLDER}/${TEST_ERRREF}
          RESULT_VARIABLE TEST_ERRREF_RESULT
      )
      if (TEST_ERRREF_RESULT)
        set (TEST_ERRREF_RESULT 0)
        file (STRINGS ${TEST_FOLDER}/${TEST_OUTPUT}.err test_act)
        list (LENGTH test_act len_act)
        file (STRINGS ${TEST_FOLDER}/${TEST_ERRREF} test_ref)
        list (LENGTH test_ref len_ref)
        math (EXPR _FP_LEN "${len_ref} - 1")
        if (len_act GREATER 0 AND len_ref GREATER 0)
          math (EXPR _FP_LEN "${len_ref} - 1")
          foreach (line RANGE 0 ${_FP_LEN})
            list (GET test_act ${line} str_act)
            list (GET test_ref ${line} str_ref)
            if (NOT str_act STREQUAL str_ref)
              if (str_act)
                set (TEST_ERRREF_RESULT 1)
                message (STATUS "line = ${line}\n***ACTUAL: ${str_act}\n****REFER: ${str_ref}\n")
              endif ()
            endif ()
          endforeach ()
        else ()
          if (len_act EQUAL 0)
            message (STATUS "COMPARE Failed: ${TEST_FOLDER}/${TEST_OUTPUT}.err is empty")
          endif ()
          if (len_ref EQUAL 0)
            message (STATUS "COMPARE Failed: ${TEST_FOLDER}/${TEST_ERRREF} is empty")
          endif ()
        endif ()
        if (NOT len_act EQUAL len_ref)
          set (TEST_ERRREF_RESULT 1)
        endif ()
      endif ()
    endif ()

    message (STATUS "COMPARE Result: ${TEST_ERRREF_RESULT}")

    # again, if return value is !=0 scream and shout
    if (TEST_ERRREF_RESULT)
      message (FATAL_ERROR "Failed: The error output of ${TEST_OUTPUT}.err did not match ${TEST_ERRREF}")
    endif ()
  endif ()
endif ()

set (TEST_GREP_RESULT 0)
if (TEST_GREP_COMPARE)
  # now grep the output with the reference
  file (READ ${TEST_FOLDER}/${TEST_OUTPUT} TEST_STREAM)
  list (LENGTH TEST_STREAM test_len)
  if (test_len GREATER 0)
    # TEST_REFERENCE should always be matched
    string (REGEX MATCH "${TEST_REFERENCE}" TEST_MATCH ${TEST_STREAM})
    string (COMPARE EQUAL "${TEST_REFERENCE}" "${TEST_MATCH}" TEST_GREP_RESULT)
    if (NOT TEST_GREP_RESULT)
      message (FATAL_ERROR "Failed: The output of ${TEST_PROGRAM} did not contain ${TEST_REFERENCE}")
    endif ()

    string (REGEX MATCH "${TEST_FILTER}" TEST_MATCH ${TEST_STREAM})
    if (TEST_EXPECT)
      # TEST_EXPECT (1) interprets TEST_FILTER as; NOT to match
      string (LENGTH "${TEST_MATCH}" TEST_GREP_RESULT)
      if (TEST_GREP_RESULT)
        message (FATAL_ERROR "Failed: The output of ${TEST_PROGRAM} did contain ${TEST_FILTER}")
      endif ()
    endif ()
  endif ()
endif ()

# dump the output unless nodisplay option is set
if (TEST_SKIP_COMPARE AND NOT TEST_NO_DISPLAY)
  if (EXISTS "${TEST_FOLDER}/${TEST_OUTPUT}")
    file (READ ${TEST_FOLDER}/${TEST_OUTPUT} TEST_STREAM)
    execute_process (
        COMMAND ${CMAKE_COMMAND} -E echo ${TEST_STREAM}
        RESULT_VARIABLE TEST_RESULT
    )
  endif ()
endif ()

# everything went fine...
message (STATUS "${TEST_PROGRAM} Passed")

