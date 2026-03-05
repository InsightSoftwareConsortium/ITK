itk_wrap_module(ITKVtkGlue)
if("${VTK_VERSION}" VERSION_LESS 7.0.0)
  message(
    WARNING
    "The ITKVtkGlue module can only built with Python 3 and VTK >= 7."
  )
elseif(ITK_USE_PYTHON_LIMITED_API)
  message(
    FATAL_ERROR
    "The ITKVtkGlue module can only built without Python limited API due to VTK limitations."
    "Please set `ITK_USE_PYTHON_LIMITED_API` to `FALSE`."
  )
else()
  list(
    APPEND
    WRAPPER_SWIG_LIBRARY_FILES
    "${CMAKE_CURRENT_SOURCE_DIR}/VtkGlue.i"
  )
  itk_auto_load_and_end_wrap_submodules()
endif()
