# - Check whether the CXX compiler supports a given flag.
# CHECK_CXX_COMPILER_FLAG(<flag> <var>)
#  <flag> - the compiler flag
#  <var>  - variable to store the result
# This internally calls the check_cxx_source_compiles macro.  See help
# for CheckCXXSourceCompiles for a listing of variables that can
# modify the build.

#=============================================================================
# Copyright 2006-2010 Kitware, Inc.
# Copyright 2006 Alexander Neundorf <neundorf@kde.org>
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

macro(ITK_CHECK_CXX_COMPILER_FLAG _FLAG _RESULT)
  if(ITK_LEGACY_REMOVE)
    message( FATAL_ERROR "REPLACE ITK_CHECK_CXX_COMPILER_FLAG with check_cxx_compiler_flag" )
  endif()
  check_cxx_compiler_flag( ${_FLAG} ${_RESULT} )
endmacro()
