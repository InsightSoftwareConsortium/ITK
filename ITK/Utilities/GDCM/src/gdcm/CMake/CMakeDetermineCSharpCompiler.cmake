
# determine the compiler to use for CSharp programs
# NOTE, a generator may set CMAKE_CSharp_COMPILER before
# loading this file to force a compiler.
# use environment variable CSHARP first if defined by user, next use
# the cmake variable CMAKE_GENERATOR_CSHARP which can be defined by a generator
# as a default compiler
#
# Sets the following variables:
#   CMAKE_CSharp_COMPILER
#   CMAKE_AR
#   CMAKE_RANLIB
#   CMAKE_COMPILER_IS_GNUGNAT

IF(NOT CMAKE_CSharp_COMPILER)
  # prefer the environment variable CSHARP
  IF($ENV{CSHARP} MATCHES ".+")
    GET_FILENAME_COMPONENT(CMAKE_CSharp_COMPILER_INIT $ENV{CSHARP} PROGRAM PROGRAM_ARGS CMAKE_CSharp_FLAGS_ENV_INIT)
    IF(CMAKE_CSharp_FLAGS_ENV_INIT)
      SET(CMAKE_CSharp_COMPILER_ARG1 "${CMAKE_CSharp_FLAGS_ENV_INIT}" CACHE STRING "First argument to CSharp compiler")
    ENDIF(CMAKE_CSharp_FLAGS_ENV_INIT)
    IF(NOT EXISTS ${CMAKE_CSharp_COMPILER_INIT})
      MESSAGE(FATAL_ERROR "Could not find compiler set in environment variable CSHARP:\n$ENV{CSHARP}.")
    ENDIF(NOT EXISTS ${CMAKE_CSharp_COMPILER_INIT})
  ENDIF($ENV{CSHARP} MATCHES ".+")

  # next prefer the generator-specified compiler
  IF(CMAKE_GENERATOR_CSHARP)
    IF(NOT CMAKE_CSharp_COMPILER_INIT)
      SET(CMAKE_CSharp_COMPILER_INIT ${CMAKE_GENERATOR_CSHARP})
    ENDIF(NOT CMAKE_CSharp_COMPILER_INIT)
  ENDIF(CMAKE_GENERATOR_CSHARP)

  # finally list compilers to try
  IF(CMAKE_CSharp_COMPILER_INIT)
    SET(CMAKE_CSharp_COMPILER_LIST ${CMAKE_CSharp_COMPILER_INIT})
  ELSE(CMAKE_CSharp_COMPILER_INIT)
    # Known compilers:
    # mcs/gmcs/smcs # mono
    # csc: DotNet
    SET(CMAKE_CSharp_COMPILER_LIST csc mcs gmcs smcs)
  ENDIF(CMAKE_CSharp_COMPILER_INIT)

  # Find the compiler.
  FIND_PROGRAM(CMAKE_CSharp_COMPILER NAMES ${CMAKE_CSharp_COMPILER_LIST} DOC "CSharp compiler")
  IF(CMAKE_CSharp_COMPILER_INIT AND NOT CMAKE_CSharp_COMPILER)
    SET(CMAKE_CSharp_COMPILER "${CMAKE_CSharp_COMPILER_INIT}" CACHE FILEPATH "CSharp compiler" FORCE)
  ENDIF(CMAKE_CSharp_COMPILER_INIT AND NOT CMAKE_CSharp_COMPILER)
ENDIF(NOT CMAKE_CSharp_COMPILER)
MARK_AS_ADVANCED(CMAKE_CSharp_COMPILER)

GET_FILENAME_COMPONENT(COMPILER_LOCATION "${CMAKE_CSharp_COMPILER}"
  PATH)


#INCLUDE(CMakeFindBinUtils)

# configure variables set in this file for fast reload later on
CONFIGURE_FILE(
  #${CMAKE_ROOT}/Modules/CMakeCSharpCompiler.cmake.in
  ${CMAKE_MODULE_PATH}/CMakeCSharpCompiler.cmake.in
  #  "${CMAKE_PLATFORM_ROOT_BIN}/CMakeCSharpCompiler.cmake"
  ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeCSharpCompiler.cmake
  @ONLY
  )

SET(CMAKE_CSharp_COMPILER_ENV_VAR "CSC")
