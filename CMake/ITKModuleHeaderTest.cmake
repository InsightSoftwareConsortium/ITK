# This checks HeaderTest's in each module.  A HeaderTest can be found in the
# module 'test' directory in a file itk<module_name>HeaderTest.cxx.  This
# contains a null main(), but includes all the classes in the module.  The
# primary purpose of this test is to make sure there are not missing module
# dependencies.

# This does not force the developer to install python to be able to build ITK.
# The tests will simply not be run if python is unavailable.
find_package(PythonInterp)

# Improve performance of MSVC GUI, by reducing number of header tests.
set( MAXIMUM_NUMBER_OF_HEADERS_default 35 )
if( MSVC )
  set( MAXIMUM_NUMBER_OF_HEADERS_default 9999 )
endif()

# The maximum number of headers in a test.  This helps limit memory issues,
# and the cppcheck tests.  However, if this is not unity, there is a slight
# chance that problems may be hidden.  For a complete header check, set to "1".
set( MAXIMUM_NUMBER_OF_HEADERS ${MAXIMUM_NUMBER_OF_HEADERS_default} CACHE STRING "The number of headers in a HeaderTest code." )
mark_as_advanced( MAXIMUM_NUMBER_OF_HEADERS )

if(NOT TARGET ITKHeaderTests)
  add_custom_target( ITKHeaderTests
    ${CMAKE_COMMAND} --build ${ITK_BINARY_DIR}
    COMMENT "Regenerating and building the header tests." )
endif()

macro( itk_module_headertest _name )
  if( NOT ${_name}_THIRD_PARTY
      AND EXISTS ${${_name}_SOURCE_DIR}/include
      AND PYTHON_EXECUTABLE
      AND NOT (PYTHON_VERSION_STRING VERSION_LESS 2.6)
      AND NOT (${_name} STREQUAL ITKTestKernel)
      AND NOT (CMAKE_GENERATOR MATCHES "^Visual Studio 10.*"))

    # Count how many tests we are going to get, and put the source files in
    # the list _outputs.
    # WARNING: This code is highly coupled with the BuildHeaderTest.py file
    # below.  Before making any logic changes here, make sure that script is not
    # effected.
    set( _include ${${_name}_SOURCE_DIR}/include )
    file( GLOB _h_files ${_include}/*.h )
    file( GLOB _hxx_files ${_include}/*.hxx )
    set( _header_files ${_h_files} ${_hxx_files} )
    list( LENGTH _h_files _num_headers )
    set( _outputs ${${_name}_BINARY_DIR}/test/${_name}HeaderTest1.cxx )
    set( _test_num 1 )
    set( _available_headers "${MAXIMUM_NUMBER_OF_HEADERS}" )
    while( ${_num_headers} GREATER ${_available_headers} )
      math( EXPR _test_num "${_test_num} + 1" )
      math( EXPR _available_headers "${_available_headers} + ${MAXIMUM_NUMBER_OF_HEADERS}" )
      list( APPEND _outputs
        ${${_name}_BINARY_DIR}/test/${_name}HeaderTest${_test_num}.cxx )
    endwhile()

    add_custom_target( ${_name}HeaderTestClean
      ${CMAKE_COMMAND} -E remove ${_outputs} )
    add_dependencies( ITKHeaderTests ${_name}HeaderTestClean )

    # We check to see if the headers are changed.  If so, remove the header test
    # source files so they are regenerated.
    set( _headers_list_md5 "${${_name}_BINARY_DIR}/test/CMakeFiles/HeadersList.md5" )
    list( SORT _header_files )
    string( MD5 _new_md5 "${_header_files}" )
    set( _regenerate_sources FALSE )
    if( NOT EXISTS "${_headers_list_md5}" )
      set( _regenerate_sources TRUE )
    else()
      file( READ "${_headers_list_md5}" _old_md5 )
      if( NOT ("${_old_md5}" STREQUAL "${_new_md5}"))
        set( _regenerate_sources TRUE )
      endif()
    endif()
    file( WRITE "${_headers_list_md5}" "${_new_md5}" )
    if( ${_regenerate_sources} )
      file( REMOVE ${_outputs} )
    endif()

    set( _test_num 1 )
    foreach( _header_test_src ${_outputs} )
      get_filename_component( _test_name ${_header_test_src} NAME_WE )
      add_custom_command(
        OUTPUT ${_header_test_src}
        COMMAND ${PYTHON_EXECUTABLE} ${ITK_CMAKE_DIR}/../Utilities/Maintenance/BuildHeaderTest.py
        ${_name}
        ${${_name}_SOURCE_DIR}
        ${${_name}_BINARY_DIR}
        ${MAXIMUM_NUMBER_OF_HEADERS}
        ${_test_num}
        )
      add_executable( ${_test_name} ${_header_test_src} )
      target_link_libraries( ${_test_name} ${${_name}_LIBRARIES} itksys )

      add_dependencies(${_name}-all ${_test_name})
      math( EXPR _test_num "${_test_num} + 1" )
    endforeach()
  endif()
endmacro()
