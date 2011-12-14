itk_wrap_module("ITKJavaBase" Java)

  set(WRAPPER_LIBRARY_DEPENDS )
  set(WRAPPER_LIBRARY_LINK_LIBRARIES )

  set(WRAPPER_LIBRARY_SWIG_INPUTS "${CMAKE_CURRENT_SOURCE_DIR}/javaBase.i")

  # no wrap_*.cmake file to include, no need to run that
  # itk_auto_load_submodules()

itk_end_wrap_module()
