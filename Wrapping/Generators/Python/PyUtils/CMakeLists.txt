###############################################################################
# Add the ITKPythonSupport library which contains the itkPyCommand class.

itk_wrap_module("ITKPyUtils" Python)

  set(WRAPPER_LIBRARY_DEPENDS ITKCommon)
  set(WRAPPER_LIBRARY_LINK_LIBRARIES ${ITK_LIBRARIES})

  set(WRAPPER_LIBRARY_CXX_SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/itkPyCommand.cxx")

  itk_auto_load_submodules()
itk_end_wrap_module()
