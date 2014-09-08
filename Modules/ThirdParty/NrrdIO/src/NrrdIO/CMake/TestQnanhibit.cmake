#
# Checks whether the 22nd bit of a 32-bit quiet-NaN is 1 (1) or 0 (0).  This
# distinction is needed in handling of IEEE floating point special values.
# This quantity is independent of endian-ness.
#
# VARIABLE - variable to store the result to
#

macro(TEST_QNANHIBIT VARIABLE LOCAL_TEST_DIR)
  if(NOT DEFINED "HAVE_${VARIABLE}")
    try_run(${VARIABLE} HAVE_${VARIABLE}
      ${CMAKE_BINARY_DIR}
      ${LOCAL_TEST_DIR}/TestQnanhibit.c
      OUTPUT_VARIABLE OUTPUT)
    message(STATUS "Check the value of the 22nd bit of a 32-bit quiet-NaN")
    if(HAVE_${VARIABLE})
      if(${VARIABLE} LESS 0)
        message(ERROR " A test (qnanhibit.c) necessary for NrrdIO configuration returned error code. NrrdIO may not properly handle NaN's.")
      endif(${VARIABLE} LESS 0)
      if(${VARIABLE})
        file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
                    "Value of the 22nd bit of a 32-bit quiet-NaN is 1")
        message(STATUS "Check the value of the 22nd bit of a 32-bit quiet-NaN - 1")
      else(${VARIABLE})
        file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
                    "Value of the 22nd bit of a 32-bit quiet-NaN is 0")
        message(STATUS "Check the value of the 22nd bit of a 32-bit quiet-NaN - 0")
      endif(${VARIABLE})
    else(HAVE_${VARIABLE})
      file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
        "\tFailed to compile a test (TestQnanhibit.c) necessary to configure for proper handling of IEEE floating point NaN's.\n")
      message(STATUS "Failed to compile a test (TestQnanhibit.c) necessary to configure for proper handling of IEEE floating point NaN's")
    endif(HAVE_${VARIABLE})
    file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log "TestQnanhibit.c produced following output:\n${OUTPUT}\n\n")
  endif()
endmacro(TEST_QNANHIBIT)
