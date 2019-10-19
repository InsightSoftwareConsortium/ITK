itk_wrap_module(ITKVtkGlue)
  if("${VTK_VERSION}" VERSION_LESS 7.0.0)
    message(WARNING
      "The ITKVtkGlue module can only built with Python 3 and VTK >= 7.\n"
      "Only the c++ version of the module will be built.")
  else()
    list(APPEND WRAPPER_SWIG_LIBRARY_FILES "${CMAKE_CURRENT_SOURCE_DIR}/VtkGlue.i")
    set(WRAPPER_LIBRARY_DEPENDS ITKCommon ITKVTK)

    set(WRAPPER_LIBRARY_LINK_LIBRARIES ${ITK_LIBRARIES})

    itk_auto_load_submodules()
  endif()
itk_end_wrap_module()
