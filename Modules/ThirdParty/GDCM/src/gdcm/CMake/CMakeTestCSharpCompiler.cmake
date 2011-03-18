
# This file is used by EnableLanguage in cmGlobalGenerator to
# determine that that selected CShapr compiler can actually compile
# and link the most basic of programs.   If not, a fatal error
# is set and cmake stops processing commands and will not generate
# any makefiles or projects.
IF(NOT CMAKE_CSharp_COMPILER_WORKS)
  MESSAGE(STATUS "Check for working CSharp compiler: ${CMAKE_CSharp_COMPILER}")
  FILE(WRITE ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/testCSharpCompiler.cs
    "class Dummy {\n"
    "static void Main() {\n"
    "}\n}\n")
  TRY_COMPILE(CMAKE_CSharp_COMPILER_WORKS ${CMAKE_BINARY_DIR}
    ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/testCSharpCompiler.cs
    OUTPUT_VARIABLE OUTPUT)
  SET(C_TEST_WAS_RUN 1)
ENDIF(NOT CMAKE_CSharp_COMPILER_WORKS)

IF(NOT CMAKE_CSharp_COMPILER_WORKS)
  MESSAGE(STATUS "Check for working CSharp compiler: ${CMAKE_CSharp_COMPILER} -- broken")
  FILE(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
    "Determining if the CSharp compiler works failed with "
    "the following output:\n${OUTPUT}\n\n")
  MESSAGE(FATAL_ERROR "The CSharp compiler \"${CMAKE_CSharp_COMPILER}\" "
    "is not able to compile a simple test program.\nIt fails "
    "with the following output:\n ${OUTPUT}\n\n"
    "CMake will not be able to correctly generate this project.")
ELSE(NOT CMAKE_CSharp_COMPILER_WORKS)
  IF(C_TEST_WAS_RUN)
    MESSAGE(STATUS "Check for working CSharp compiler: ${CMAKE_CSharp_COMPILER} -- works")
    FILE(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeOutput.log
      "Determining if the CSharp compiler works passed with "
      "the following output:\n${OUTPUT}\n\n")
  ENDIF(C_TEST_WAS_RUN)
  SET(CMAKE_CSharp_COMPILER_WORKS 1 CACHE INTERNAL "")

  IF(CMAKE_CSharp_COMPILER_FORCED)
    # The compiler configuration was forced by the user.
    # Assume the user has configured all compiler information.
  ELSE(CMAKE_CSharp_COMPILER_FORCED)
    # Try to identify the ABI and configure it into CMakeCSharpCompiler.cmake
    INCLUDE(${CMAKE_ROOT}/Modules/CMakeDetermineCompilerABI.cmake)
    CMAKE_DETERMINE_COMPILER_ABI(C ${CMAKE_ROOT}/Modules/CMakeCSharpCompilerABI.c)
    CONFIGURE_FILE(
      #${CMAKE_ROOT}/Modules/CMakeCSharpCompiler.cmake.in
      ${CMAKE_MODULE_PATH}/CMakeCSharpCompiler.cmake.in
      ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeCSharpCompiler.cmake
      @ONLY
      )
  ENDIF(CMAKE_CSharp_COMPILER_FORCED)
ENDIF(NOT CMAKE_CSharp_COMPILER_WORKS)
