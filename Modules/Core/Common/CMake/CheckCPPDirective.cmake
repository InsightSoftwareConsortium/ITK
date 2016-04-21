#
# Checks whether this platform has a given preprocessor directive
#
# VARIABLE - variable to store the result to
#

get_filename_component(_CheckCPPDirective_DIR "${CMAKE_CURRENT_LIST_FILE}" PATH)

macro(CHECK_CPP_DIRECTIVE_EXISTS DIRECTIVE VARIABLE)
 if(NOT DEFINED "HAVE_${VARIABLE}")
  message(STATUS "Checking to see if this platform has the ${DIRECTIVE} C-Preprocessor directive")
  set(DIRECTIVE ${DIRECTIVE})
  configure_file(${_CheckCPPDirective_DIR}/CheckCPPDirectiveExists.cxx.in
    ${CMAKE_BINARY_DIR}/CMakeFiles/CMakeTmp/CheckCPPDirectiveExists.cxx)
  try_compile(${VARIABLE}
    ${CMAKE_BINARY_DIR}
    ${CMAKE_BINARY_DIR}/CMakeFiles/CMakeTmp/CheckCPPDirectiveExists.cxx
    OUTPUT_VARIABLE OUTPUT)
  if(${VARIABLE})
    set(HAVE_${VARIABLE} TRUE CACHE INTERNAL " ")
    message(STATUS "Checking to see if this platform supports has the ${DIRECTIVE} C-Preprocessor directive - yes")
    file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeOutput.log
      "Checking to see if this platform supports has the ${DIRECTIVE} C-Preprocessor directive with "
      "the following output:\n${OUTPUT}\n\n")
  else()
    message(STATUS "Checking to see if this platform supports has the ${DIRECTIVE} C-Preprocessor directive - no")
    file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
      "Checking to see if this platform supports has the ${DIRECTIVE} C-Preprocessor directive with "
      "the following output:\n${OUTPUT}\n\n")
  endif()
  endif()
endmacro()
