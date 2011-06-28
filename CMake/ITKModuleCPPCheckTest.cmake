macro( itk_module_cppcheck_test _name )
  if( EXISTS "${${_name}_SOURCE_DIR}/src" )
    add_cppcheck_dir(
      ${_name}_SRC_DIR # _name
      "${${_name}_SOURCE_DIR}/src" #_dir
      ${${_name}_SOURCE_DIR}/include #_include_dirs
      VERBOSE
      FORCE
      )
  endif()

  if( EXISTS "${${_name}_SOURCE_DIR}/test" )
    add_cppcheck_dir(
      ${_name}_TEST_DIR # _name
      "${${_name}_SOURCE_DIR}/test" #_dir
      ${${_name}_SOURCE_DIR}/include #_include_dirs
      VERBOSE
      FORCE
      ALL
      )
  endif()
endmacro()
