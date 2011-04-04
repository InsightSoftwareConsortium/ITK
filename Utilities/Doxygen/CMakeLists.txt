#
# Build the documentation
#
include (${CMAKE_ROOT}/Modules/Documentation.cmake)

mark_as_advanced(CLEAR BUILD_DOCUMENTATION)

if (BUILD_DOCUMENTATION)

  foreach(itk-module ${ITK_MODULES_ALL})
    if(${itk-module}_IS_TEST)
      list( APPEND DOXYGEN_TEST_DIRS "\"${${itk-module}_SOURCE_DIR}\" \\ \n ")
    endif()
  endforeach()

  string( REGEX REPLACE ";" "\$" DOXYGEN_TEST_DIRS ${DOXYGEN_TEST_DIRS} )

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
    WORKING_DIRECTORY ${ITK_BINARY_DIR}/Utilities/Doxygen
    )

  message( STATUS
           "To generate Doxygen's documentation, you need to build the Documentation target"
         )

endif (BUILD_DOCUMENTATION)
