IF(WIN32)
  IF(NOT MINGW)
  CONFIGURE_FILE(${ITK_SOURCE_DIR}/Utilities/Dart/AllowContinuous.bat.in
                 ${ITK_BINARY_DIR}/AllowContinuous.bat)
  CONFIGURE_FILE(${ITK_SOURCE_DIR}/Utilities/Dart/PreventContinuous.bat.in
                 ${ITK_BINARY_DIR}/PreventContinuous.bat)
  CONFIGURE_FILE(${ITK_SOURCE_DIR}/Utilities/Dart/BuildContinuous.bat.in
                 ${ITK_BINARY_DIR}/BuildContinuous.bat)
  ELSE(NOT MINGW)
    CONFIGURE_FILE(${ITK_SOURCE_DIR}/Utilities/Dart/BuildContinuous.sh.in
                   ${ITK_BINARY_DIR}/BuildContinuous.sh
                   @ONLY IMMEDIATE)
    EXEC_PROGRAM(chmod ARGS 755 \"${ITK_BINARY_DIR}/BuildContinuous.sh\"
                       OUTPUT_VARIABLE ITK_EXEC_PLEASE_BE_QUIET)
    CONFIGURE_FILE(${ITK_SOURCE_DIR}/Utilities/Dart/BuildContinuousWithCTest.sh.in
                   ${ITK_BINARY_DIR}/BuildContinuousWithCTest.sh
                   @ONLY IMMEDIATE)
    EXEC_PROGRAM(chmod ARGS 755 \"${ITK_BINARY_DIR}/BuildContinuousWithCTest.sh\"
                       OUTPUT_VARIABLE ITK_EXEC_PLEASE_BE_QUIET)
    CONFIGURE_FILE(${ITK_SOURCE_DIR}/Utilities/Dart/AllowContinuous.sh.in
                   ${ITK_BINARY_DIR}/AllowContinuous.sh
                   @ONLY IMMEDIATE)
    EXEC_PROGRAM(chmod ARGS 755 \"${ITK_BINARY_DIR}/AllowContinuous.sh\"
                       OUTPUT_VARIABLE ITK_EXEC_PLEASE_BE_QUIET)
    CONFIGURE_FILE(${ITK_SOURCE_DIR}/Utilities/Dart/PreventContinuous.sh.in
                   ${ITK_BINARY_DIR}/PreventContinuous.sh
                   @ONLY IMMEDIATE)
    EXEC_PROGRAM(chmod ARGS 755 \"${ITK_BINARY_DIR}/PreventContinuous.sh\"
                       OUTPUT_VARIABLE ITK_EXEC_PLEASE_BE_QUIET)
    CONFIGURE_FILE(${ITK_SOURCE_DIR}/Utilities/Dart/cronentry.in
                   ${ITK_BINARY_DIR}/cronentry
                   @ONLY IMMEDIATE)
  ENDIF(NOT MINGW)
ENDIF(WIN32)
IF(UNIX)
  CONFIGURE_FILE(${ITK_SOURCE_DIR}/Utilities/Dart/BuildContinuous.sh.in
                 ${ITK_BINARY_DIR}/BuildContinuous.sh
                 @ONLY IMMEDIATE)
  EXEC_PROGRAM(chmod ARGS 755 \"${ITK_BINARY_DIR}/BuildContinuous.sh\"
                     OUTPUT_VARIABLE ITK_EXEC_PLEASE_BE_QUIET)
  CONFIGURE_FILE(${ITK_SOURCE_DIR}/Utilities/Dart/BuildContinuousWithCTest.sh.in
                 ${ITK_BINARY_DIR}/BuildContinuousWithCTest.sh
                 @ONLY IMMEDIATE)
  EXEC_PROGRAM(chmod ARGS 755 \"${ITK_BINARY_DIR}/BuildContinuousWithCTest.sh\"
                       OUTPUT_VARIABLE ITK_EXEC_PLEASE_BE_QUIET)
  CONFIGURE_FILE(${ITK_SOURCE_DIR}/Utilities/Dart/AllowContinuous.sh.in
                 ${ITK_BINARY_DIR}/AllowContinuous.sh
                 @ONLY IMMEDIATE)
  EXEC_PROGRAM(chmod ARGS 755 \"${ITK_BINARY_DIR}/AllowContinuous.sh\"
                     OUTPUT_VARIABLE ITK_EXEC_PLEASE_BE_QUIET)
  CONFIGURE_FILE(${ITK_SOURCE_DIR}/Utilities/Dart/PreventContinuous.sh.in
                 ${ITK_BINARY_DIR}/PreventContinuous.sh
                 @ONLY IMMEDIATE)
  EXEC_PROGRAM(chmod ARGS 755 \"${ITK_BINARY_DIR}/PreventContinuous.sh\"
                     OUTPUT_VARIABLE ITK_EXEC_PLEASE_BE_QUIET)
  CONFIGURE_FILE(${ITK_SOURCE_DIR}/Utilities/Dart/cronentry.in
                 ${ITK_BINARY_DIR}/cronentry
                 @ONLY IMMEDIATE)
  CONFIGURE_FILE(${ITK_SOURCE_DIR}/Utilities/Dart/NewSince.csh.in
                 ${ITK_BINARY_DIR}/NewSince.csh
                 @ONLY IMMEDIATE)
  EXEC_PROGRAM(chmod ARGS 755 \"${ITK_BINARY_DIR}/NewSince.csh\"
                 OUTPUT_VARIABLE ITK_EXEC_PLEASE_BE_QUIET)
ENDIF(UNIX)
