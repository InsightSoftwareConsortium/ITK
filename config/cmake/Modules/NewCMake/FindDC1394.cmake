#
# try to find the dc1394 library (version 2) and include files
#
# DC1394_INCLUDE_DIR, where to find dc1394/dc1394_control.h, etc.
# DC1394_LIBRARIES, the libraries to link against to use DC1394.
# DC1394_FOUND, If false, do not try to use DC1394.
#

# Look for one of the header files
find_path( DC1394_INCLUDE_DIR dc1394/dc1394.h)

# Look for the library
find_library( DC1394_LIBRARIES dc1394)

# handle the QUIETLY and REQUIRED arguments and set DC1394_FOUND to TRUE if
# all listed variables are TRUE
include(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(DC1394 DEFAULT_MSG DC1394_LIBRARIES DC1394_INCLUDE_DIR)

mark_as_advanced(DC1394_INCLUDE_DIR DC1394_LIBRARIES )


# Find Apple Framework dependencies
if(APPLE AND DC1394_FOUND)
  set(DC1394_LIBRARIES ${DC1394_LIBRARIES}
                       "-framework CoreServices"
                       "-framework IOKit" )
endif()





