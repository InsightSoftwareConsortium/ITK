# Build third-party utilities.
ITK_THIRD_PARTY_SUBDIR(ZLIB zlib)
ITK_THIRD_PARTY_SUBDIR(PNG  png)

SUBDIRS(MetaIO DICOMParser)
SUBDIRS(vxl)

# Use the system version of STD library (STL and streams).
IF (NOT ITK_USE_SYSTEM_STDLIB)
  SUBDIRS(stdlib)
ENDIF (NOT ITK_USE_SYSTEM_STDLIB)

# Install the UseITK.cmake file configured from the top-level.
INSTALL_FILES(/lib/InsightToolkit ".cmake" "ITKConfig")
