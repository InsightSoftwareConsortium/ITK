# runTest.cmake executes a command and captures the output in a file. File is then compared
# against a reference file. Exit status of command can also be compared.
cmake_policy(SET CMP0007 NEW)

# arguments checking
if (NOT TEST_PROGRAM)
  message (FATAL_ERROR "Require TEST_PROGRAM to be defined")
endif (NOT TEST_PROGRAM)
#if (NOT TEST_ARGS)
#  message (STATUS "Require TEST_ARGS to be defined")
#endif (NOT TEST_ARGS)
if (NOT TEST_FOLDER)
  message ( FATAL_ERROR "Require TEST_FOLDER to be defined")
endif (NOT TEST_FOLDER)
if (NOT TEST_OUTPUT)
  message (FATAL_ERROR "Require TEST_OUTPUT to be defined")
endif (NOT TEST_OUTPUT)
if (NOT TEST_EXPECT)
  message (STATUS "Require TEST_EXPECT to be defined")
endif (NOT TEST_EXPECT)
#if (NOT TEST_FILTER)
#  message (STATUS "Require TEST_FILTER to be defined")
#endif (NOT TEST_FILTER)
if (NOT TEST_SKIP_COMPARE AND NOT TEST_REFERENCE)
  message (FATAL_ERROR "Require TEST_REFERENCE to be defined")
endif (NOT TEST_SKIP_COMPARE AND NOT TEST_REFERENCE)

# if there is not an error reference file add the error output to the stdout file
if (NOT TEST_ERRREF)
  set (ERROR_APPEND 1)
endif (NOT TEST_ERRREF)

message (STATUS "COMMAND: ${TEST_PROGRAM} ${TEST_ARGS}")

if (TEST_ENV_VAR)
  set (ENV{${TEST_ENV_VAR}} "${TEST_ENV_VALUE}")
endif (TEST_ENV_VAR)

if (NOT TEST_INPUT)
  # run the test program, capture the stdout/stderr and the result var
  execute_process (
      COMMAND ${TEST_PROGRAM} ${TEST_ARGS}
      WORKING_DIRECTORY ${TEST_FOLDER}
      RESULT_VARIABLE TEST_RESULT
      OUTPUT_FILE ${TEST_OUTPUT}
      ERROR_FILE ${TEST_OUTPUT}.err
      OUTPUT_VARIABLE TEST_OUT
      ERROR_VARIABLE TEST_ERROR
  )
else (NOT TEST_INPUT)
  # run the test program with stdin, capture the stdout/stderr and the result var
  execute_process (
      COMMAND ${TEST_PROGRAM} ${TEST_ARGS}
      WORKING_DIRECTORY ${TEST_FOLDER}
      RESULT_VARIABLE TEST_RESULT
      INPUT_FILE ${TEST_INPUT}
      OUTPUT_FILE ${TEST_OUTPUT}
      ERROR_FILE ${TEST_OUTPUT}.err
      OUTPUT_VARIABLE TEST_OUT
      ERROR_VARIABLE TEST_ERROR
  )
endif (NOT TEST_INPUT)

message (STATUS "COMMAND Result: ${TEST_RESULT}")

# if the .err file exists and ERRROR_APPEND is enabled
if (ERROR_APPEND AND EXISTS ${TEST_FOLDER}/${TEST_OUTPUT}.err)
  file (READ ${TEST_FOLDER}/${TEST_OUTPUT}.err TEST_STREAM)
  file (APPEND ${TEST_FOLDER}/${TEST_OUTPUT} "${TEST_STREAM}")
endif (ERROR_APPEND AND EXISTS ${TEST_FOLDER}/${TEST_OUTPUT}.err)

# append the test result status with a predefined text
if (TEST_APPEND)
  file (APPEND ${TEST_FOLDER}/${TEST_OUTPUT} "${TEST_APPEND} ${TEST_RESULT}\n")
endif (TEST_APPEND)

# if the return value is !=${TEST_EXPECT} bail out
if (NOT ${TEST_RESULT} STREQUAL ${TEST_EXPECT})
  message ( FATAL_ERROR "Failed: Test program ${TEST_PROGRAM} exited != ${TEST_EXPECT}.\n${TEST_ERROR}")
endif (NOT ${TEST_RESULT} STREQUAL ${TEST_EXPECT})

message (STATUS "COMMAND Error: ${TEST_ERROR}")

# if the output file needs Storage text removed
if (TEST_MASK)
  file (READ ${TEST_FOLDER}/${TEST_OUTPUT} TEST_STREAM)
   string (REGEX REPLACE "Storage:[^\n]+\n" "Storage:   <details removed for portability>\n" TEST_STREAM "${TEST_STREAM}")
  file (WRITE ${TEST_FOLDER}/${TEST_OUTPUT} "${TEST_STREAM}")
endif (TEST_MASK)

# if the output file needs Modified text removed
if (TEST_MASK_MOD)
  file (READ ${TEST_FOLDER}/${TEST_OUTPUT} TEST_STREAM)
   string (REGEX REPLACE "Modified:[^\n]+\n" "Modified:  XXXX-XX-XX XX:XX:XX XXX\n" TEST_STREAM "${TEST_STREAM}")
  file (WRITE ${TEST_FOLDER}/${TEST_OUTPUT} "${TEST_STREAM}")
endif (TEST_MASK_MOD)

# if the output file or the .err file needs to mask out error stack info
if (TEST_MASK_ERROR)
  if (NOT TEST_ERRREF)
    # the error stack has been appended to the output file
    file (READ ${TEST_FOLDER}/${TEST_OUTPUT} TEST_STREAM)
  else ()
    # the error stack remains in the .err file
    file (READ ${TEST_FOLDER}/${TEST_OUTPUT}.err TEST_STREAM)
  endif ()
  string (REGEX REPLACE "thread [0-9]*:" "thread (IDs):" TEST_STREAM "${TEST_STREAM}")
  string (REGEX REPLACE ": ([^\n]*)[.]c " ": (file name) " TEST_STREAM "${TEST_STREAM}")
  string (REGEX REPLACE " line [0-9]*" " line (number)" TEST_STREAM "${TEST_STREAM}")
  string (REGEX REPLACE "v[1-9]*[.][0-9]*[.]" "version (number)." TEST_STREAM "${TEST_STREAM}")
  string (REGEX REPLACE "[1-9]*[.][0-9]*[.][0-9]*[^)]*" "version (number)" TEST_STREAM "${TEST_STREAM}")
  string (REGEX REPLACE "H5Eget_auto[1-2]*" "H5Eget_auto(1 or 2)" TEST_STREAM "${TEST_STREAM}")
  string (REGEX REPLACE "H5Eset_auto[1-2]*" "H5Eset_auto(1 or 2)" TEST_STREAM "${TEST_STREAM}")
  # write back the changes to the original files
  if (NOT TEST_ERRREF)
    file (WRITE ${TEST_FOLDER}/${TEST_OUTPUT} "${TEST_STREAM}")
  else ()
    file (WRITE ${TEST_FOLDER}/${TEST_OUTPUT}.err "${TEST_STREAM}")
  endif ()
endif (TEST_MASK_ERROR)

# remove text from the output file
if (TEST_FILTER)
  file (READ ${TEST_FOLDER}/${TEST_OUTPUT} TEST_STREAM)
  string (REGEX REPLACE "${TEST_FILTER}" "" TEST_STREAM "${TEST_STREAM}")
  file (WRITE ${TEST_FOLDER}/${TEST_OUTPUT} "${TEST_STREAM}")
endif (TEST_FILTER)

# compare output files to references unless this must be skipped
if (NOT TEST_SKIP_COMPARE)
  if (WIN32 AND NOT MINGW)
    file (READ ${TEST_FOLDER}/${TEST_REFERENCE} TEST_STREAM)
    file (WRITE ${TEST_FOLDER}/${TEST_REFERENCE} "${TEST_STREAM}")
  endif (WIN32 AND NOT MINGW)

  # now compare the output with the reference
  execute_process (
      COMMAND ${CMAKE_COMMAND} -E compare_files ${TEST_FOLDER}/${TEST_OUTPUT} ${TEST_FOLDER}/${TEST_REFERENCE}
      RESULT_VARIABLE TEST_RESULT
  )
  if (NOT ${TEST_RESULT} STREQUAL 0)
  set (TEST_RESULT 0)
  file (STRINGS ${TEST_FOLDER}/${TEST_OUTPUT} test_act)
  list (LENGTH test_act len_act)
  file (STRINGS ${TEST_FOLDER}/${TEST_REFERENCE} test_ref)
  list (LENGTH test_ref len_ref)
  if (NOT ${len_act} STREQUAL "0")
    math (EXPR _FP_LEN "${len_ref} - 1")
    foreach (line RANGE 0 ${_FP_LEN})
      list (GET test_act ${line} str_act)
      list (GET test_ref ${line} str_ref)
      if (NOT "${str_act}" STREQUAL "${str_ref}")
        if (NOT "${str_act}" STREQUAL "")
          set (TEST_RESULT 1)
          message ("line = ${line}\n***ACTUAL: ${str_act}\n****REFER: ${str_ref}\n")
         endif (NOT "${str_act}" STREQUAL "")
      endif (NOT "${str_act}" STREQUAL "${str_ref}")
    endforeach (line RANGE 0 ${_FP_LEN})
  endif (NOT ${len_act} STREQUAL "0")
  if (NOT ${len_act} STREQUAL ${len_ref})
    set (TEST_RESULT 1)
  endif (NOT ${len_act} STREQUAL ${len_ref})
  endif (NOT ${TEST_RESULT} STREQUAL 0)

  message (STATUS "COMPARE Result: ${TEST_RESULT}")

  # again, if return value is !=0 scream and shout
  if (NOT ${TEST_RESULT} STREQUAL 0)
    message (FATAL_ERROR "Failed: The output of ${TEST_OUTPUT} did not match ${TEST_REFERENCE}")
  endif (NOT ${TEST_RESULT} STREQUAL 0)

  # now compare the .err file with the error reference, if supplied
  if (TEST_ERRREF)
    if (WIN32 AND NOT MINGW)
      file (READ ${TEST_FOLDER}/${TEST_ERRREF} TEST_STREAM)
      file (WRITE ${TEST_FOLDER}/${TEST_ERRREF} "${TEST_STREAM}")
    endif (WIN32 AND NOT MINGW)

    # now compare the error output with the error reference
    execute_process (
        COMMAND ${CMAKE_COMMAND} -E compare_files ${TEST_FOLDER}/${TEST_OUTPUT}.err ${TEST_FOLDER}/${TEST_ERRREF}
        RESULT_VARIABLE TEST_RESULT
    )
    if (NOT ${TEST_RESULT} STREQUAL 0)
    set (TEST_RESULT 0)
    file (STRINGS ${TEST_FOLDER}/${TEST_OUTPUT}.err test_act)
    list (LENGTH test_act len_act)
    file (STRINGS ${TEST_FOLDER}/${TEST_ERRREF} test_ref)
    list (LENGTH test_ref len_ref)
    math (EXPR _FP_LEN "${len_ref} - 1")
    if (NOT ${len_act} STREQUAL "0")
      math (EXPR _FP_LEN "${len_ref} - 1")
      foreach (line RANGE 0 ${_FP_LEN})
        list (GET test_act ${line} str_act)
        list (GET test_ref ${line} str_ref)
        if (NOT "${str_act}" STREQUAL "${str_ref}")
          if (NOT "${str_act}" STREQUAL "")
            set (TEST_RESULT 1)
            message ("line = ${line}\n***ACTUAL: ${str_act}\n****REFER: ${str_ref}\n")
           endif (NOT "${str_act}" STREQUAL "")
        endif (NOT "${str_act}" STREQUAL "${str_ref}")
      endforeach (line RANGE 0 ${_FP_LEN})
    endif (NOT ${len_act} STREQUAL "0")
    if (NOT ${len_act} STREQUAL ${len_ref})
      set (TEST_RESULT 1)
    endif (NOT ${len_act} STREQUAL ${len_ref})
    endif (NOT ${TEST_RESULT} STREQUAL 0)

    message (STATUS "COMPARE Result: ${TEST_RESULT}")

    # again, if return value is !=0 scream and shout
    if (NOT ${TEST_RESULT} STREQUAL 0)
      message (FATAL_ERROR "Failed: The error output of ${TEST_OUTPUT}.err did not match ${TEST_ERRREF}")
    endif (NOT ${TEST_RESULT} STREQUAL 0)
  endif (TEST_ERRREF)
endif (NOT TEST_SKIP_COMPARE)

# everything went fine...
message ("Passed: The output of ${TEST_PROGRAM} matches ${TEST_REFERENCE}")
