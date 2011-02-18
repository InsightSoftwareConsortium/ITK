#
# Checks whether this platform can (1) or cannot(0) do bit-fields greater than
# 32 bits.  This is necessary for correct handling of IEEE floating point
# special values.
#
# VARIABLE - variable to store the result to
#

macro(CHECK_BIG_BITFIELD VARIABLE LOCAL_TEST_DIR)
 if("HAVE_${VARIABLE}" MATCHES "^HAVE_${VARIABLE}$")
  message(STATUS "Checking to see if this platform supports large bit-fields (>32 bits)")
  try_run(DUMMY ${VARIABLE}
    ${CMAKE_BINARY_DIR}
    ${LOCAL_TEST_DIR}/CheckBigBitfield.c
    OUTPUT_VARIABLE OUTPUT)
  if(${VARIABLE})
    set(HAVE_${VARIABLE} TRUE CACHE INTERNAL " ")
    message(STATUS "Checking to see if this platform supports large bit-fields (>32 bits) - yes")
    file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeOutput.log
      "Checking to see if this platform supports large bit-fields (>32 bits) passed with "
      "the following output:\n${OUTPUT}\n\n")
  else(${VARIABLE})
    message(STATUS "Checking to see if this platform supports large bit-fields (>32 bits) - no")
    file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
      "Checking to see if this platform supports large bit-fields (>32 bits) failed with "
      "the following output:\n${OUTPUT}\n\n")
  endif(${VARIABLE})
  endif("HAVE_${VARIABLE}" MATCHES "^HAVE_${VARIABLE}$")
endmacro(CHECK_BIG_BITFIELD)
