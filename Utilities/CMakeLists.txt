SUBDIRS(zlib png MetaIO GlutMaster DICOMParser)
IF(VXL_FROM_UTILITIES)
  SUBDIRS(vxl)
ENDIF(VXL_FROM_UTILITIES)

# Use the system version of STD library (STL and streams)
OPTION(ITK_USE_SYSTEM_STDLIB "Use the system's version of the standard C++ library (STL and streams). If 'OFF', ITK will build against a supplied version of the standard library from SGI. On SGI systems, this should be set to 'OFF'." ON)
IF (NOT ITK_USE_SYSTEM_STDLIB)
   SUBDIRS(stdlib)
ENDIF (NOT ITK_USE_SYSTEM_STDLIB)

IF(CMAKE_SYSTEM MATCHES "IRIX.*")
ELSE(CMAKE_SYSTEM MATCHES "IRIX.*")
  MARK_AS_ADVANCED(ITK_USE_SYSTEM_STDLIB)
ENDIF(CMAKE_SYSTEM MATCHES "IRIX.*")

# Install the UseITK.cmake file configured from the top-level.
INSTALL_FILES(/lib/InsightToolkit ".cmake" "ITKConfig")
