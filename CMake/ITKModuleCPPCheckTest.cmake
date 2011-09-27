macro( itk_module_cppcheck_test _name )
  if( EXISTS "${${_name}_SOURCE_DIR}/src" )
    file( GLOB _cxx_files *.cxx )
    list( LENGTH _cxx_files _num_cxx_files )
    # let's make sure there are actually cxx files in the src dir
    if( ${_num_cxx_files} GREATER 0 )
      add_cppcheck_dir(
        ${_name}_SRC_DIR # _name
        "${${_name}_SOURCE_DIR}/src" #_dir
        ${${_name}_SOURCE_DIR}/include #_include_dirs
        VERBOSE
        FORCE
        ALL
       )
    endif()
  endif()

  if( EXISTS "${${_name}_SOURCE_DIR}/test" )
    file( GLOB _cxx_files *.cxx )
    list( LENGTH _cxx_files _num_cxx_files )
    # let's make sure there are actually cxx files in the src dir
    if( ${_num_cxx_files} GREATER 0 )
      add_cppcheck_dir(
        ${_name}_TEST_DIR # _name
        "${${_name}_SOURCE_DIR}/test" #_dir
        ${${_name}_SOURCE_DIR}/include #_include_dirs
        VERBOSE
        FORCE
        ALL
        )
    endif()
  endif()
endmacro()
