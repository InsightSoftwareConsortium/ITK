#
# Checks whether this platform can (1) or cannot(0) do bit-fields greater than
# 32 bits.  This is necessary for correct handling of IEEE floating point
# special values.
#
# VARIABLE - variable to store the result to
#

MACRO(CHECK_BIG_BITFIELD VARIABLE LOCAL_TEST_DIR)
 IF("HAVE_${VARIABLE}" MATCHES "^HAVE_${VARIABLE}$")
  MESSAGE(STATUS "Checking to see if this platform supports large bit-fields (>32 bits)")
  TRY_RUN(DUMMY ${VARIABLE}
    ${CMAKE_BINARY_DIR}
    ${LOCAL_TEST_DIR}/CheckBigBitfield.c
    OUTPUT_VARIABLE OUTPUT)
  IF(${VARIABLE})
    SET(HAVE_${VARIABLE} TRUE CACHE INTERNAL " ")
    MESSAGE(STATUS "Checking to see if this platform supports large bit-fields (>32 bits) - yes")
    FILE(APPEND ${CMAKE_BINARY_DIR}/CMakeOutput.log
      "Checking to see if this platform supports large bit-fields (>32 bits) passed with "
      "the following output:\n${OUTPUT}\n\n")
  ELSE(${VARIABLE})
    MESSAGE(STATUS "Checking to see if this platform supports large bit-fields (>32 bits) - no")
    FILE(APPEND ${CMAKE_BINARY_DIR}/CMakeError.log
      "Checking to see if this platform supports large bit-fields (>32 bits) failed with "
      "the following output:\n${OUTPUT}\n\n")
  ENDIF(${VARIABLE})
  ENDIF("HAVE_${VARIABLE}" MATCHES "^HAVE_${VARIABLE}$")
ENDMACRO(CHECK_BIG_BITFIELD)
