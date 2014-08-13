#
# Find the packages required by this module
#

# Needed VTK version
set(VERSION_MIN "5.10.0")

# Look for VTK
find_package(VTK COMPONENTS
  vtkCommonCore
  vtkRenderingCore
  vtkRenderingOpenGL
  vtkRenderingFreeType
  vtkInteractionStyle
  vtkIOImage
  vtkImagingSources
  REQUIRED)

# Older versions of VTK (VTK 5.5 for example) do not have VTK_VERSION, in this
# case it needs to be defined manually
if(NOT VTK_VERSION)
set(VTK_VERSION "${VTK_MAJOR_VERSION}.${VTK_MINOR_VERSION}.${VTK_BUILD_VERSION}")
endif()

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
