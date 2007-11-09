# Find the ECW library
#
# Sets
#   ECW_FOUND.  If false, don't try to use ecw
#   ECW_INCLUDE_DIR
#   ECW_LIBRARIES

SET( ECW_FOUND "NO" )

FIND_PATH( ECW_INCLUDE_DIR NCSEcw.h
  /usr/include
  /usr/local/include
)

IF( ECW_INCLUDE_DIR )

  FIND_LIBRARY( ECW_ncsutil_LIBRARY NCSUtild
    /usr/lib
    /usr/local/lib
    /usr/lib64
    /usr/local/lib64
  )

  FIND_LIBRARY( ECW_ncsecw_LIBRARY NCSEcwd
    /usr/lib
    /usr/local/lib
    /usr/lib64
    /usr/local/lib64
  )
  
  IF( ECW_ncsutil_LIBRARY )
  IF( ECW_ncsecw_LIBRARY )

    SET( ECW_FOUND "YES" )
    SET( ECW_LIBRARIES ${ECW_ncsutil_LIBRARY} ${ECW_ncsecw_LIBRARY} )

  ENDIF( ECW_ncsecw_LIBRARY )
  ENDIF( ECW_ncsutil_LIBRARY )


ENDIF( ECW_INCLUDE_DIR )
