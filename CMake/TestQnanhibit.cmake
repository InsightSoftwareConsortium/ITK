#
# Checks whether the 22nd bit of a 32-bit quiet-NaN is 1 (1) or 0 (0).  This 
# distinction is needed in handling of IEEE floating point special values.  
# This quantity is independent of endian-ness.
#
# VARIABLE - variable to store the result to
#

MACRO(TEST_QNANHIBIT VARIABLE LOCAL_TEST_DIR)
  IF("HAVE_${VARIABLE}" MATCHES "^HAVE_${VARIABLE}$")
    TRY_RUN(${VARIABLE} HAVE_${VARIABLE}
      ${CMAKE_BINARY_DIR}
      ${LOCAL_TEST_DIR}/TestQnanhibit.c
      OUTPUT_VARIABLE OUTPUT)
    MESSAGE(STATUS "Check the value of the 22nd bit of a 32-bit quiet-NaN")
    IF(HAVE_${VARIABLE})
      IF(${VARIABLE} LESS 0)
        MESSAGE(ERROR " A test (qnanhibit.c) necessary for NrrdIO configuration returned error code. NrrdIO may not properly handle NaN's.")
      ENDIF(${VARIABLE} LESS 0)
      IF(${VARIABLE})
        FILE(APPEND ${CMAKE_BINARY_DIR}/CMakeError.log
                    "Value of the 22nd bit of a 32-bit quiet-NaN is 1")
        MESSAGE(STATUS "Check the value of the 22nd bit of a 32-bit quiet-NaN - 1")
      ELSE(${VARIABLE})
        FILE(APPEND ${CMAKE_BINARY_DIR}/CMakeError.log
                    "Value of the 22nd bit of a 32-bit quiet-NaN is 0")
        MESSAGE(STATUS "Check the value of the 22nd bit of a 32-bit quiet-NaN - 0")
      ENDIF(${VARIABLE})
    ELSE(HAVE_${VARIABLE})
      FILE(APPEND ${CMAKE_BINARY_DIR}/CMakeError.log
        "\tFailed to compile a test (TestQnanhibit.c) necessary to configure for proper handling of IEEE floating point NaN's.\n")
      MESSAGE(STATUS "Failed to compile a test (TestQnanhibit.c) necessary to configure for proper handling of IEEE floating point NaN's")
    ENDIF(HAVE_${VARIABLE})
    FILE(APPEND ${CMAKE_BINARY_DIR}/CMakeError.log "TestQnanhibit.c produced following output:\n${OUTPUT}\n\n")
  ENDIF("HAVE_${VARIABLE}" MATCHES "^HAVE_${VARIABLE}$")
ENDMACRO(TEST_QNANHIBIT)
