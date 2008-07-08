#
# Build the documentation
#
INCLUDE (${CMAKE_ROOT}/Modules/Documentation.cmake)

IF (BUILD_DOXYGEN)

  #
  # Configure the script and the doxyfile, then add target
  #
  CONFIGURE_FILE(${ITK_SOURCE_DIR}/Utilities/Doxygen/doxygen.config.in
                 ${ITK_BINARY_DIR}/Utilities/Doxygen/doxygen.config)

  CONFIGURE_FILE(${ITK_SOURCE_DIR}/Utilities/Doxygen/itkdoxygen.pl.in
                 ${ITK_BINARY_DIR}/Utilities/Doxygen/itkdoxygen.pl)

  ADD_CUSTOM_TARGET(Documentation 
    COMMAND ${DOXYGEN_EXECUTABLE} ${ITK_BINARY_DIR}/Utilities/Doxygen/doxygen.config
    MAIN_DEPENDENCY ${ITK_BINARY_DIR}/Utilities/Doxygen/doxygen.config
    DEPENDS         ${ITK_BINARY_DIR}/Utilities/Doxygen/itkdoxygen.pl
    WORKING_DIRECTORY ${ITK_BINARY_DIR}/Utilities/Doxygen
    )

ENDIF (BUILD_DOXYGEN)
