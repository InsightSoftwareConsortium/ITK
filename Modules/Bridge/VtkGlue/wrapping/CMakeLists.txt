itk_wrap_module(ITKVtkGlue)
list(APPEND WRAPPER_SWIG_LIBRARY_FILES "${CMAKE_CURRENT_SOURCE_DIR}/VtkGlue.i")
if(WIN32)
  # Python >= 3.8 ignores PATH for DLLs, so `import itk` registers VTK's DLL directory.
  # All configs share one generated module, so multi-config builds assume Release.
  set(_vtk_config Release)
  if(CMAKE_BUILD_TYPE)
    set(_vtk_config "${CMAKE_BUILD_TYPE}")
  endif()
  string(TOUPPER "${_vtk_config}" _vtk_config)
  get_target_property(_vtk_common_core VTK::CommonCore LOCATION_${_vtk_config})
  get_filename_component(_vtk_dll_dir "${_vtk_common_core}" DIRECTORY)
  file(TO_NATIVE_PATH "${_vtk_dll_dir}" _vtk_dll_dir)
  set(
    ITK_WRAP_PYTHON_CONFIGURATION_PREAMBLE
    "import os as _os

if _os.path.isdir(r'${_vtk_dll_dir}'):
    _os.add_dll_directory(r'${_vtk_dll_dir}')
"
  )
  unset(_vtk_config)
  unset(_vtk_common_core)
  unset(_vtk_dll_dir)
endif()
itk_auto_load_and_end_wrap_submodules()
