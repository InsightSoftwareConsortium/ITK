#
# try to find GTK2 (and glib2) and GTKGLArea
#

# GTK2_INCLUDE_DIR   - Directories to include to use GTK2
# GTK2_LIBRARIES     - Files to link against to use GTK2
# GTK2_DEFINITIONS   - Compiler flags to compile against GTK2
# GTK2_FOUND         - If false, don't try to use GTK2

SET( GTK2_FOUND "NO" )

FIND_PROGRAM( PKG_CONFIG pkg-config
  /usr/bin
  /usr/local/bin
  ${HOME}/bin
)

IF( PKG_CONFIG )
  EXEC_PROGRAM( ${PKG_CONFIG} ARGS "--exists gtkglext-1.0" RETURN_VALUE GTK2_PKG_RET_VAL )

  IF( ${GTK2_PKG_RET_VAL} MATCHES "0" ) 

    # The dependendies for GTK-GLExt should have all the GTK parts too, so no need to explictly find them
    EXEC_PROGRAM( ${PKG_CONFIG} ARGS "--cflags gtkglext-1.0" OUTPUT_VARIABLE GTK2_glext_DEFINITIONS )
    EXEC_PROGRAM( ${PKG_CONFIG} ARGS "--libs gtkglext-1.0" OUTPUT_VARIABLE GTK2_glext_LIBRARIES )

    SET( GTK2_FOUND "YES" )
    SET( GTK2_DEFINITIONS "${GTK2_gtk_DEFINITIONS} ${GTK2_glext_DEFINITIONS}" )
    SET( GTK2_LIBRARIES   "${GTK2_gtk_LIBRARIES} ${GTK2_glext_LIBRARIES}" )

  ENDIF( ${GTK2_PKG_RET_VAL} MATCHES "0" )

ENDIF( PKG_CONFIG )
