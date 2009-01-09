IF(UNIX)
  CONFIGURE_FILE(${ITK_SOURCE_DIR}/Utilities/KWStyle/doAllStyle.csh.in
                 ${ITK_BINARY_DIR}/doAllStyle.csh
                 @ONLY IMMEDIATE)
  EXEC_PROGRAM(chmod ARGS 755 \"${ITK_BINARY_DIR}/doAllStyle.csh\"
                     OUTPUT_VARIABLE ITK_EXEC_PLEASE_BE_QUIET)
ENDIF(UNIX)

