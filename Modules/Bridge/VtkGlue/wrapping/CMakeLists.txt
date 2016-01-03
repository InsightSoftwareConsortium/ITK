itk_wrap_module(ITKVtkGlue)
  if(PYTHON_VERSION_STRING VERSION_GREATER 3.0 AND "${VTK_VERSION}" VERSION_LESS 7.0.0)
    message(WARNING
      "The ITKVtkGlue module can only built with Python 3 and VTK >= 7.\n"
      "Only the c++ version of the module will be built.")
  else()
    list(APPEND WRAPPER_SWIG_LIBRARY_FILES "${CMAKE_CURRENT_SOURCE_DIR}/VtkGlue.i")
    set(WRAPPER_LIBRARY_DEPENDS ITKCommon
      ITKVTK)

    set(WRAPPER_LIBRARY_LINK_LIBRARIES ${ITK_LIBRARIES})

    itk_auto_load_submodules()
  endif()
itk_end_wrap_module()

if(ITK_WRAP_PYTHON AND PYTHON_VERSION_STRING VERSION_LESS 3.0)
  if(${VTK_VERSION} VERSION_LESS 6.0.0)
    target_link_libraries(ITKVtkGluePython LINK_PUBLIC vtkImagingPythonD vtkPythonCore)
  elseif(TARGET vtkImagingKitPythonD)
    target_link_libraries(ITKVtkGluePython LINK_PUBLIC vtkImagingKitPythonD)
  else()
    target_link_libraries(ITKVtkGluePython LINK_PUBLIC vtkImagingCorePythonD)
  endif()
endif()
