project(ITKLevelSetsv4Visualization)

# VTK 5.8 and greater defines VTK_LIBRARIES
if(NOT VTK_LIBRARIES)
  set(VTK_LIBRARIES_EXPLICIT vtkRendering vtkHybrid)
endif()

set(ITKLevelSetsv4Visualization_LIBRARIES ${VTK_LIBRARIES}
  ${VTK_LIBRARIES_EXPLICIT} ITKVtkGlue )

# When this module is loaded by an app, load VTK too.
set(ITKLevelSetsv4Visualization_EXPORT_CODE_INSTALL "
set(VTK_DIR \"${VTK_DIR}\")
find_package(VTK NO_MODULE REQUIRED)
")
set(ITKLevelSetsv4Visualization_EXPORT_CODE_BUILD "
if(NOT ITK_BINARY_DIR)
  set(VTK_DIR \"${VTK_DIR}\")
  find_package(VTK NO_MODULE REQUIRED)
endif()
")

itk_module_impl()
