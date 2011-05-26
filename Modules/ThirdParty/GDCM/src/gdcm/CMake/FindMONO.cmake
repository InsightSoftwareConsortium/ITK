# - Find MONO
# This module finds an installed MONO.  It sets the following variables:
#  MONO_FOUND - set to true if MONO is found
#  MONO_DIR - the directory where swig is installed
#  MONO_EXECUTABLE - the path to the swig executable
#  MONO_VERSION   - the version number of the swig executable
#
# All informations are collected from the MONO_EXECUTABLE so the
# version to be found can be changed from the command line by
# means of setting MONO_EXECUTABLE
#
#  Copyright (c) 2006-2010 Mathieu Malaterre <mathieu.malaterre@gmail.com>
#
#  Redistribution and use is allowed according to the terms of the New
#  BSD license.
#  For details see the accompanying COPYING-CMAKE-SCRIPTS file.
#
SET(MONO_FOUND FALSE)

# apt-get install mono-jit mono-mcs mono-gac mono-gmcs

# eg.
# $ gmcs HelloWorld.cs
# $ mono HelloWorld.exe

# TODO: what are 'cscc' and 'ilrun' ?

FIND_PROGRAM(MONO_EXECUTABLE mono)
FIND_PROGRAM(MCS_EXECUTABLE mcs)    # 1.0
FIND_PROGRAM(GMCS_EXECUTABLE gmcs)  # 2.0
FIND_PROGRAM(SMCS_EXECUTABLE smcs)  # Moonlight
# mono-gac: /usr/bin/gacutil
FIND_PROGRAM(GACUTIL_EXECUTABLE gacutil)  # gacutil - Global Assembly Cache management utility.
# mono-1.0-devel: /usr/bin/ilasm
FIND_PROGRAM(ILASM_EXECUTABLE ilasm)  #  ilasm, ilasm2 - Mono IL assembler
# mono-1.0-devel: /usr/bin/sn
FIND_PROGRAM(SN_EXECUTABLE sn)  #  sn - Digitally sign/verify/compare strongnames on CLR assemblies.

# We decide to declare mono found when both interpreter and compiler 1.0 are found.
IF(MONO_EXECUTABLE AND MCS_EXECUTABLE)
SET(MONO_FOUND TRUE)
# TODO get version
# TODO: there are multiple 'mcs' command on unix, need to check this is Mono:
# mcs --version should return "Mono C# compiler version 1.9.1.0"
ELSEIF(MONO_EXECUTABLE AND GMCS_EXECUTABLE)
SET(MONO_FOUND TRUE)
ELSEIF(MONO_EXECUTABLE AND SMCS_EXECUTABLE)
SET(MONO_FOUND TRUE)
ENDIF(MONO_EXECUTABLE AND MCS_EXECUTABLE)

IF(NOT MONO_FOUND)
  IF(NOT MONO_FIND_QUIETLY)
    IF(MONO_FIND_REQUIRED)
      MESSAGE(FATAL_ERROR "MONO was not found. Please specify mono/mcs executable location")
    ELSE(MONO_FIND_REQUIRED)
      MESSAGE(STATUS "MONO was not found. Please specify mono/mcs executable location")
    ENDIF(MONO_FIND_REQUIRED)
  ENDIF(NOT MONO_FIND_QUIETLY)
ENDIF(NOT MONO_FOUND)

GET_FILENAME_COMPONENT(current_list_path ${CMAKE_CURRENT_LIST_FILE} PATH)
SET(MONO_USE_FILE ${current_list_path}/UseMONO.cmake)

MARK_AS_ADVANCED(
  MONO_EXECUTABLE
  MCS_EXECUTABLE
  GMCS_EXECUTABLE
  SMCS_EXECUTABLE
  ILASM_EXECUTABLE
  SN_EXECUTABLE
  GACUTIL_EXECUTABLE
)
