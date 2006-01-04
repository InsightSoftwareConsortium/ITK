#
# Checks whether this platform has a given preprocessor directive
#
# VARIABLE - variable to store the result to
#

MACRO(CHECK_CPP_DIRECTIVE_EXISTS DIRECTIVE VARIABLE)
 IF("HAVE_${VARIABLE}" MATCHES "^HAVE_${VARIABLE}$")
  MESSAGE(STATUS "Checking to see if this platform has the ${DIRECTIVE} C-Preprocessor directive")
  SET(DIRECTIVE ${DIRECTIVE})
  CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/CMake/CheckCPPDirectiveExists.cxx.in 
    ${CMAKE_BINARY_DIR}/CMakeTmp/CheckCPPDirectiveExists.cxx IMMEDIATE)
  TRY_COMPILE(${VARIABLE}
    ${CMAKE_BINARY_DIR}
    ${CMAKE_BINARY_DIR}/CMakeTmp/CheckCPPDirectiveExists.cxx
    OUTPUT_VARIABLE OUTPUT)
  IF(${VARIABLE})
    SET(HAVE_${VARIABLE} TRUE CACHE INTERNAL " ")
    MESSAGE(STATUS "Checking to see if this platform supports has the ${DIRECTIVE} C-Preprocessor directive - yes")
    FILE(APPEND ${CMAKE_BINARY_DIR}/CMakeOutput.log
      "Checking to see if this platform supports has the ${DIRECTIVE} C-Preprocessor directive with "
      "the following output:\n${OUTPUT}\n\n")
  ELSE(${VARIABLE})
    MESSAGE(STATUS "Checking to see if this platform supports has the ${DIRECTIVE} C-Preprocessor directive - no")
    FILE(APPEND ${CMAKE_BINARY_DIR}/CMakeError.log
      "Checking to see if this platform supports has the ${DIRECTIVE} C-Preprocessor directive with "
      "the following output:\n${OUTPUT}\n\n")
  ENDIF(${VARIABLE})
  ENDIF("HAVE_${VARIABLE}" MATCHES "^HAVE_${VARIABLE}$")
ENDMACRO(CHECK_CPP_DIRECTIVE_EXISTS)
