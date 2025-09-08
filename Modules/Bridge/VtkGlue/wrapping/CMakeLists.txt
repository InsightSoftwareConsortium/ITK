# Forward include directories to CastXML
foreach(_vtk_lib IN LISTS ITKVtkGlue_VTK_LIBRARIES)
  get_target_property(
    _vtk_lib_include_dirs
    ${_vtk_lib}
    INTERFACE_INCLUDE_DIRECTORIES
  )
  if(_vtk_lib_include_dirs)
    include_directories(${_vtk_lib_include_dirs})
  endif()
endforeach()

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
  set(
    WRAPPER_LIBRARY_DEPENDS
    ITKCommon
    ITKVTK
  )
  set(
    WRAPPER_LIBRARY_LINK_LIBRARIES
    ${ITK_LIBRARIES}
    ${ITKVtkGlue_VTK_LIBRARIES}
  )
  itk_auto_load_and_end_wrap_submodules()
endif()
