# Build third-party utilities.
ITK_THIRD_PARTY_SUBDIR(ZLIB zlib)
ITK_THIRD_PARTY_SUBDIR(PNG  png)
ITK_THIRD_PARTY_SUBDIR(JPEG jpeg)

SUBDIRS(MetaIO DICOMParser)
SUBDIRS(vxl)

# Use the system version of STD library (STL and streams).
IF (NOT ITK_USE_SYSTEM_STDLIB)
  SUBDIRS(stdlib)
ENDIF (NOT ITK_USE_SYSTEM_STDLIB)

# Install the UseITK.cmake file configured from the top-level.
INSTALL_FILES(/lib/InsightToolkit ".cmake" "ITKConfig")

# Build the KWSys library in the itkkwsys namespace.
SET(KWSYS_NAMESPACE itkkwsys)
SET(KWSYS_LIBRARY_INSTALL_DIR /lib/InsightToolkit)
SET(KWSYS_INCLUDE_INSTALL_DIR /include/InsightToolkit/Utilities)
SUBDIRS(kwsys)
