#
# try to find the dc1394 library (version 2) and include files
#
# DC1394_INCLUDE_DIR, where to find dc1394/dc1394_control.h, etc.
# DC1394_LIBRARIES, the libraries to link against to use DC1394.
# DC1394_FOUND, If false, do not try to use DC1394.
#

# Look for one of the header files
FIND_PATH( DC1394_INCLUDE_DIR dc1394/dc1394.h)

# Look for the library
FIND_LIBRARY( DC1394_LIBRARIES dc1394)

# handle the QUIETLY and REQUIRED arguments and set DC1394_FOUND to TRUE if 
# all listed variables are TRUE
INCLUDE(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(DC1394 DEFAULT_MSG DC1394_LIBRARIES DC1394_INCLUDE_DIR)

MARK_AS_ADVANCED(DC1394_INCLUDE_DIR DC1394_LIBRARIES )


# Find Apple Framework dependencies
IF(APPLE AND DC1394_FOUND)
  SET(DC1394_LIBRARIES ${DC1394_LIBRARIES} 
                       "-framework CoreServices" 
                       "-framework IOKit" )
ENDIF(APPLE AND DC1394_FOUND)





