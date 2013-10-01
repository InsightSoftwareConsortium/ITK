itk_wrap_module(ITKVtkGlue)
  set(WRAPPER_SWIG_LIBRARY_FILES ${WRAPPER_SWIG_LIBRARY_FILES} "${CMAKE_CURRENT_SOURCE_DIR}/VtkGlue.i")
  set(WRAPPER_LIBRARY_DEPENDS ITKCommon
    ITKVTK)
  set(WRAPPER_LIBRARY_LINK_LIBRARIES ${ITK_LIBRARIES} ${PYTHON_LIBRARY} )

  itk_auto_load_submodules()
itk_end_wrap_module()

if(ITK_WRAP_PYTHON)
  if(${VTK_VERSION} VERSION_LESS 6.0.0)
    target_link_libraries(ITKVtkGluePython vtkImagingPythonD vtkPythonCore)
  else()
    target_link_libraries(ITKVtkGluePython vtkImagingCorePythonD)
  endif()
endif()
