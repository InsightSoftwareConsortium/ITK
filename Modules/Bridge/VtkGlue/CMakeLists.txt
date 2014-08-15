project(ITKVtkGlue)
set(ITKVtkGlue_LIBRARIES ITKVtkGlue)

#
# Add the third party includes and libraries
#

set(ITKVtkGlue_SYSTEM_INCLUDE_DIRS ${VTK_INCLUDE_DIRS})
set(ITKVtkGlue_SYSTEM_LIBRARY_DIRS ${VTK_LIBRARY_DIRS})

# When this module is loaded by an app, load VTK too.
set(ITKVtkGlue_EXPORT_CODE_INSTALL "
set(VTK_DIR \"${VTK_DIR}\")
find_package(VTK NO_MODULE
COMPONENTS
  vtkCommonCore
  vtkRenderingCore
  vtkRenderingOpenGL
  vtkRenderingFreeType
  vtkInteractionStyle
  vtkIOImage
  vtkImagingSources
 REQUIRED)
")
set(ITKVtkGlue_EXPORT_CODE_BUILD "
if(NOT ITK_BINARY_DIR)
  set(VTK_DIR \"${VTK_DIR}\")
  find_package(VTK NO_MODULE
COMPONENTS
  vtkCommonCore
  vtkRenderingCore
  vtkRenderingOpenGL
  vtkRenderingFreeType
  vtkInteractionStyle
  vtkIOImage
  vtkImagingSources
REQUIRED)
endif()
")

itk_module_impl()
