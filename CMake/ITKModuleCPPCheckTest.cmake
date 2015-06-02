#-----------------------------------------------------------------------------
# Enable running cppcheck for each module on its source and test directories.
option(ITK_CPPCHECK_TEST "Run cppcheck for static code analysis" OFF)
mark_as_advanced(ITK_CPPCHECK_TEST)

macro( itk_module_cppcheck_test _name )
  if( EXISTS "${${_name}_SOURCE_DIR}/src" )
    file( GLOB _cxx_files "${${_name}_SOURCE_DIR}/src/*.cxx" )
    list( LENGTH _cxx_files _num_cxx_files )
    # let's make sure there are actually cxx files in the src dir
    if( ${_num_cxx_files} GREATER 0 )
      add_cppcheck_dir(
        ${_name}Sources # _name
        "${${_name}_SOURCE_DIR}/src" #_dir
        ${${_name}_SOURCE_DIR}/include #_include_dirs
        VERBOSE
        FORCE
        ALL
       )
    endif()
  endif()

  if( EXISTS "${${_name}_SOURCE_DIR}/test" )
    file( GLOB _cxx_files "${${_name}_SOURCE_DIR}/test/*.cxx" )
    list( LENGTH _cxx_files _num_cxx_files )
    # let's make sure there are actually cxx files in the src dir
    if( ${_num_cxx_files} GREATER 0 )
      add_cppcheck_dir(
        ${_name}Tests # _name
        "${${_name}_SOURCE_DIR}/test" #_dir
        ${${_name}_SOURCE_DIR}/include #_include_dirs
        VERBOSE
        FORCE
        ALL
        )
    endif()
  endif()
endmacro()
