#
# Find the packages required by this module
#
find_package(VTK REQUIRED)
set(VERSION_MIN "5.9.20110419")
if (${VTK_VERSION} VERSION_LESS ${VERSION_MIN})
  message(ERROR " VtkGlue requires VTK version ${VERSION_MIN} or newer but the current version is ${VTK_VERSION}")
endif()

# The VTK DICOMParser and vtkmetaio includes conflict with the ITK
# versions. Here we remove them from the include directories.
#
string(REGEX REPLACE "[^;]*MetaIO;"
         "" VTK_INCLUDE_DIRS "${VTK_INCLUDE_DIRS}")
string(REGEX REPLACE "[^;]*vtkmetaio;"
         "" VTK_INCLUDE_DIRS "${VTK_INCLUDE_DIRS}")
string(REGEX REPLACE "[^;]*DICOMParser;"
         "" VTK_INCLUDE_DIRS "${VTK_INCLUDE_DIRS}")
include(${VTK_USE_FILE})
