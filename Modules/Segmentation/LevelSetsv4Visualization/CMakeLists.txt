project(ITKLevelSetsv4Visualization)

#
# Add the third party includes and libraries
#

set(ITKLevelSetsv4Visualization_SYSTEM_INCLUDE_DIRS ${VTK_INCLUDE_DIRS})
set(ITKLevelSetsv4Visualization_SYSTEM_LIBRARY_DIRS ${VTK_LIBRARY_DIRS})

set(ITKLevelSetsv4Visualization_LIBRARIES ${VTK_LIBRARIES}
  ${VTK_LIBRARIES_EXPLICIT} ITKVtkGlue )

# When this module is loaded by an app, load VTK too.
set(ITKLevelSetsv4Visualization_EXPORT_CODE_INSTALL "
set(VTK_DIR \"${VTK_DIR}\")
find_package(VTK NO_MODULE REQUIRED
COMPONENTS
  vtkCommonCore
  vtkRenderingAnnotation
  vtkRenderingCore
  vtkRenderingOpenGL
  vtkRenderingFreeType
  vtkInteractionStyle
  vtkIOImage
  vtkImagingSources
REQUIRED
 )
")
set(ITKLevelSetsv4Visualization_EXPORT_CODE_BUILD "
if(NOT ITK_BINARY_DIR)
  set(VTK_DIR \"${VTK_DIR}\")
  find_package(VTK NO_MODULE
COMPONENTS
  vtkCommonCore
  vtkRenderingAnnotation
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
