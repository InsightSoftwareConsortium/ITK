#
#  Copyright (c) 2006-2010 Mathieu Malaterre <mathieu.malaterre@gmail.com>
#
#  Redistribution and use is allowed according to the terms of the New
#  BSD license.
#  For details see the accompanying COPYING-CMAKE-SCRIPTS file.
#
# Macro to centralize all the plateform specific
# tests.
MACRO(GDCM_PLATFORM_CXX_TEST var description invert)
  IF("${var}_COMPILED" MATCHES "^${var}_COMPILED$")
    MESSAGE(STATUS "${description}")
    TRY_COMPILE(${var}_COMPILED
      ${CMAKE_CURRENT_BINARY_DIR}
      ${CMAKE_CURRENT_SOURCE_DIR}/CMake/gdcmPlatformCxxTests.cxx
      COMPILE_DEFINITIONS -DTEST_${var} ${GDCM_PLATFORM_CXX_TEST_DEFINES}
      OUTPUT_VARIABLE OUTPUT)
    IF(${var}_COMPILED)
      FILE(APPEND ${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/CMakeOutput.log
        "${description} compiled with the following output:\n${OUTPUT}\n\n")
    ELSE(${var}_COMPILED)
      FILE(APPEND ${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/CMakeError.log
        "${description} failed to compile with the following output:\n${OUTPUT}\n\n")
    ENDIF(${var}_COMPILED)
    IF(${invert} MATCHES INVERT)
      IF(${var}_COMPILED)
        MESSAGE(STATUS "${description} - no")
      ELSE(${var}_COMPILED)
        MESSAGE(STATUS "${description} - yes")
      ENDIF(${var}_COMPILED)
    ELSE(${invert} MATCHES INVERT)
      IF(${var}_COMPILED)
        MESSAGE(STATUS "${description} - yes")
      ELSE(${var}_COMPILED)
        MESSAGE(STATUS "${description} - no")
      ENDIF(${var}_COMPILED)
    ENDIF(${invert} MATCHES INVERT)
  ENDIF("${var}_COMPILED" MATCHES "^${var}_COMPILED$")
  IF(${invert} MATCHES INVERT)
    IF(${var}_COMPILED)
      SET(${var} 0)
    ELSE(${var}_COMPILED)
      SET(${var} 1)
    ENDIF(${var}_COMPILED)
  ELSE(${invert} MATCHES INVERT)
    IF(${var}_COMPILED)
      SET(${var} 1)
    ELSE(${var}_COMPILED)
      SET(${var} 0)
    ENDIF(${var}_COMPILED)
  ENDIF(${invert} MATCHES INVERT)
ENDMACRO(GDCM_PLATFORM_CXX_TEST)
