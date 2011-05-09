#
# Build the documentation
#
include (${CMAKE_ROOT}/Modules/Documentation.cmake)

mark_as_advanced(CLEAR BUILD_DOCUMENTATION)

if (BUILD_DOCUMENTATION)

  if( ${DOXYGEN_DOT_FOUND} )
    set( ITK_HAVE_DOT "YES" )
  else()
    set( ITK_HAVE_DOT "NO" )
  endif()

  set( DOX_MODULE_LIST ${ITK_MODULES_ENABLED} )

  foreach(itk-module ${DOX_MODULE_LIST})
    if(${itk-module}_IS_TEST)
      list( APPEND DOXYGEN_TEST_DIRS "\"${${itk-module}_SOURCE_DIR}\" \\ \n ")
    else()
      if( EXISTS ${${itk-module}_SOURCE_DIR}/include )
        list( APPEND DOXYGEN_INCLUDE_DIRS "\"${${itk-module}_SOURCE_DIR}/include\" \\ \n")
      endif()

      if( ${DOXYGEN_DOT_FOUND} )
        # generates here one image for each module which corresponds to
        # its local dependency.
        set(_png ${itk-module}_dependencies.png)
        add_custom_command( OUTPUT ${_png}
          COMMAND ${DOXYGEN_DOT_EXECUTABLE} -Tpng
            -o ${_png} ${itk-module}.dot
          DEPENDS ${ITK_BINARY_DIR}/Utilities/Doxygen/Modules/${itk-module}.dot
          WORKING_DIRECTORY ${ITK_BINARY_DIR}/Utilities/Doxygen/Modules/
          )

        list( APPEND DOX_MODULE_PNGS ${_png} )
      endif()
    endif()
  endforeach()

  list( APPEND DOXYGEN_TEST_DIRS "\"${ITK_SOURCE_DIR}/Examples\" \n")

  string( REGEX REPLACE ";" "\$" DOXYGEN_TEST_DIRS ${DOXYGEN_TEST_DIRS} )

  string( REGEX REPLACE ";" "\$" DOXYGEN_INCLUDE_DIRS ${DOXYGEN_INCLUDE_DIRS} )

  #
  # Configure the script and the doxyfile, then add target
  #
  configure_file(${ITK_SOURCE_DIR}/Utilities/Doxygen/doxygen.config.in
                 ${ITK_BINARY_DIR}/Utilities/Doxygen/doxygen.config)

  configure_file(${ITK_SOURCE_DIR}/Utilities/Doxygen/itkdoxygen.pl.in
                 ${ITK_BINARY_DIR}/Utilities/Doxygen/itkdoxygen.pl)

  add_custom_target(Documentation ALL
    COMMAND ${DOXYGEN_EXECUTABLE} ${ITK_BINARY_DIR}/Utilities/Doxygen/doxygen.config
    MAIN_DEPENDENCY ${ITK_BINARY_DIR}/Utilities/Doxygen/doxygen.config
    DEPENDS         ${ITK_BINARY_DIR}/Utilities/Doxygen/itkdoxygen.pl
                    ${DOX_MODULE_PNGS}
    WORKING_DIRECTORY ${ITK_BINARY_DIR}/Utilities/Doxygen
    )

  # Somehow the logo image is not copied into the html folder.
  # Here we ensure that the logo image ends up in the right directory
  # where html pages could find it.
  add_custom_command(TARGET Documentation
    COMMAND ${CMAKE_COMMAND} -E copy
      ${ITK_SOURCE_DIR}/Documentation/Art/itkLogoSmall.jpg # logo
      ${ITK_BINARY_DIR}/Utilities/Doxygen/html # output doxygen directory
    )

  message( STATUS
           "To generate Doxygen's documentation, you need to build the Documentation target"
         )

endif (BUILD_DOCUMENTATION)

find_package(PythonInterp)
if(PYTHON_EXECUTABLE)
  itk_add_test(NAME ModuleInDoxygenGroup COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/mcdoc.py massive-check ${ITK_SOURCE_DIR})
endif()
