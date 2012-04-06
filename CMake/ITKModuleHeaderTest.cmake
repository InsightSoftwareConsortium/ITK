# This checks HeaderTest's in each module.  A HeaderTest can be found in the
# module 'test' directory in a file itk<module_name>HeaderTest.cxx.  This
# contains a null main(), but includes all the classes in the module.  The
# primary purpose of this test is to make sure there are not missing module
# dependencies.

# This does not force the developer to install python to be able to build ITK.
# The tests will simply not be run if python is unavailable.
find_package(PythonInterp)

# The maximum number of headers in a test.  (This helps limit memory issues,
# and the cppcheck tests.)
set( MAXIMUM_NUMBER_OF_HEADERS 35
  CACHE STRING "The number of headers in a HeaderTest code." )
mark_as_advanced( MAXIMUM_NUMBER_OF_HEADERS )

add_custom_target( ITKHeaderTests
  ${CMAKE_COMMAND} --build ${ITK_BINARY_DIR}
  COMMENT "Regenerating and building the header tests." )

macro( itk_module_headertest _name )
  if( NOT ${_name}_THIRD_PARTY AND
      EXISTS ${${_name}_SOURCE_DIR}/include
      AND PYTHON_EXECUTABLE
      AND NOT (${_name} STREQUAL ITKTestKernel)
      AND NOT (CMAKE_GENERATOR MATCHES "^Visual Studio 10.*"))

    # Count how many tests we are going to get, and put the source files in
    # the list _outputs.
    set( _include ${${_name}_SOURCE_DIR}/include )
    file( GLOB _h_files ${_include}/*.h )
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

    set( _test_num 1 )
    foreach( _header_test_src ${_outputs} )
      get_filename_component( _test_name ${_header_test_src} NAME_WE )
      add_custom_command(
        OUTPUT ${_header_test_src}
        COMMAND ${PYTHON_EXECUTABLE} ${ITK_SOURCE_DIR}/Utilities/Maintenance/BuildHeaderTest.py
        ${_name}
        ${${_name}_SOURCE_DIR}
        ${${_name}_BINARY_DIR}
        ${MAXIMUM_NUMBER_OF_HEADERS}
        ${_test_num}
        )
      add_executable( ${_test_name} ${_header_test_src} )
      target_link_libraries( ${_test_name} ITKCommon )
      add_dependencies(${_name}-all ${_test_name})
      math( EXPR _test_num "${_test_num} + 1" )
    endforeach()
  endif()
endmacro()
