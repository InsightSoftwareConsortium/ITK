project(ITKItkVtkGlue)
set(ITKItkVtkGlue_LIBRARIES ITKItkVtkGlue)

#
# Add the third party includes and libraries
#
# VTK
# The VTK DICOMParser and vtkmetaio includes conflict with the ITK
# versions. Here we remove them from the include directories.
#
string(REGEX REPLACE "[^;]*vtkmetaio;"
         "" VTK_INCLUDE_DIRS "${VTK_INCLUDE_DIRS}")
string(REGEX REPLACE "[^;]*DICOMParser;"
         "" VTK_INCLUDE_DIRS "${VTK_INCLUDE_DIRS}")

set(ITKItkVtkGlue_SYSTEM_INCLUDE_DIRS ${VTK_INCLUDE_DIRS})
set(ITKItkVtkGlue_SYSTEM_LIBRARY_DIRS ${VTK_LIBRARY_DIRS})

itk_module_impl()
