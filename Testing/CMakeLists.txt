PROJECT(ITKTesting)

# The Testing directory is setup so it can build either as part of the
# normal ITK build or as a stand-alone project.  This design is useful
# for testing the installation tree of ITK.

MAKE_DIRECTORY(${ITKTesting_BINARY_DIR}/Temporary)
IF(ITK_BINARY_DIR)
  # We are building inside the tree.
  SUBDIRS(Code)

  IF(ITK_WRAP_TCL)
    SET(ITKTCL_EXECUTABLE "${CXX_TEST_PATH}/itktcl")
  ENDIF(ITK_WRAP_TCL)
ELSE(ITK_BINARY_DIR)
  # We are building as a stand-alone project.
  SET(LIBRARY_OUTPUT_PATH ${ITKTesting_BINARY_DIR}/bin CACHE PATH "Single output directory for building all libraries.")
  SET(EXECUTABLE_OUTPUT_PATH ${ITKTesting_BINARY_DIR}/bin CACHE PATH "Single output directory for building all executables.")
  MARK_AS_ADVANCED(LIBRARY_OUTPUT_PATH EXECUTABLE_OUTPUT_PATH)
  SET(CXX_TEST_PATH ${EXECUTABLE_OUTPUT_PATH})

  # Configure Dart testing support.
  SET(PROJECT_SOURCE_DIR "${ITKTesting_SOURCE_DIR}/..")
  INCLUDE(${CMAKE_ROOT}/Modules/Dart.cmake)
  SET(PROJECT_SOURCE_DIR "${ITKTesting_SOURCE_DIR}")
  MARK_AS_ADVANCED(TCL_TCLSH)
  IF(BUILD_TESTING)
    ENABLE_TESTING()
  ENDIF(BUILD_TESTING)

  # Find the data directory.
  FIND_PATH(ITK_DATA_ROOT ITKData.readme ${ITKTesting_SOURCE_DIR}/Data $ENV{ITK_DATA_ROOT})
  MARK_AS_ADVANCED(ITK_DATA_ROOT)

  # Find ITK and load its settings.
  FIND_PACKAGE(ITK)
  IF(ITK_FOUND)
    INCLUDE(${ITK_USE_FILE})
    IF(ITK_WRAP_TCL)
      FIND_PROGRAM(ITKTCL_EXECUTABLE itktcl ${ITK_DIR}/bin "${ITK_DIR}/../../bin")
    ENDIF(ITK_WRAP_TCL)
    SUBDIRS(Code)
  ELSE(ITK_FOUND)
    MESSAGE("ITK not found.  Please set ITK_DIR")
  ENDIF(ITK_FOUND)
ENDIF(ITK_BINARY_DIR)
