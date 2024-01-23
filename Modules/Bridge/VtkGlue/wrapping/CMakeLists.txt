# Forward include directories to CastXML
if(NOT
   VTK_RENDERING_BACKEND
   STREQUAL
   "None")
  foreach(_vtk_lib IN LISTS ITKVtkGlue_VTK_LIBRARIES)
    get_target_property(_vtk_lib_include_dirs ${_vtk_lib} INTERFACE_INCLUDE_DIRECTORIES)
    if(_vtk_lib_include_dirs)
      include_directories(${_vtk_lib_include_dirs})
    endif()
  endforeach()
endif()

# Python provides native conversion between ITK and VTK
if(ITK_WRAP_PYTHON)
  message(
    STATUS
      "ITK_WRAP_PYTHON is turned on - skipping ITKVtkGlue, since native version is available https://docs.itk.org/en/latest/learn/python_quick_start.html#itk-and-vtk."
  )
else()
  itk_wrap_module(ITKVtkGlue)
  if("${VTK_VERSION}" VERSION_LESS 7.0.0)
    message(WARNING "The ITKVtkGlue module can only built with Python 3 and VTK >= 7.\n"
                    "Only the c++ version of the module will be built.")
  else()
    list(APPEND WRAPPER_SWIG_LIBRARY_FILES "${CMAKE_CURRENT_SOURCE_DIR}/VtkGlue.i")
    set(WRAPPER_LIBRARY_DEPENDS ITKCommon ITKVTK)

    set(WRAPPER_LIBRARY_LINK_LIBRARIES ${ITK_LIBRARIES})

    itk_auto_load_and_end_wrap_submodules()
  endif()
endif()
