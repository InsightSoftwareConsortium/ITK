# - Check if a C++ function exists
# CHECK_CXX_EXPRESSION_COMPILES(<expression> <files> <variable>)
#
# Check that the <expression> compiles in a program that includes
# <files> and store the result in a <variable>.  Specify the list
# of files in one argument as a semicolon-separated list.
#
# The following variables may be set before calling this macro to
# modify the way the check is run:
#
#  CMAKE_REQUIRED_FLAGS = string of compile command line flags
#  CMAKE_REQUIRED_DEFINITIONS = list of macros to define (-DFOO=bar)
#  CMAKE_REQUIRED_INCLUDES = list of include directories
#  CMAKE_REQUIRED_LIBRARIES = list of libraries to link
#
# ${variable} is set to 1 on success 0 on failure

#=============================================================================
# Copyright 2003-2011 Kitware, Inc.
#
# Distributed under the OSI-approved BSD License (the "License");
# see accompanying file Copyright.txt for details.
#
# This software is distributed WITHOUT ANY WARRANTY; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the License for more information.
#=============================================================================
# (To distribute this file outside of CMake, substitute the full
#  License text for the above reference.)

include(CheckCXXSourceCompiles)

macro(check_cxx_expression_compiles EXPRESSION FILES VARIABLE)
  set(SOURCE "/* CHECK_CXX_EXPRESSION_COMPILES */\n")
  foreach(FILE ${FILES})
    set(SOURCE "${SOURCE}#include <${FILE}>\n")
  endforeach()
  set(SOURCE "${SOURCE}\nint main()\n{\n")
  set(SOURCE "${SOURCE}  static_cast<void>(${EXPRESSION});\n\n")
  set(SOURCE "${SOURCE}  return 0;\n}\n")
  CHECK_CXX_SOURCE_COMPILES("${SOURCE}" "${VARIABLE}") # Returns 1 - Success "" Failure
  if( NOT ${VARIABLE} ) # Variable is set to zero
    set(${VARIABLE} 0 CACHE INTERNAL "Test ${VARIABLE}") #Assume failure, only set to non-zero if CHECK_CXX_SOURCE_COMPILES
  endif()
endmacro()
