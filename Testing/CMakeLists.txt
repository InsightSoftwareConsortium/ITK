PROJECT(ITKTesting)

# The Testing directory is setup so it can build either as part of the
# normal ITK build or as a stand-alone project.  This design is useful
# for testing the installation tree of ITK.

MAKE_DIRECTORY(${ITKTesting_BINARY_DIR}/Temporary)
IF(ITK_BINARY_DIR)
  # We are building inside the tree.
  SUBDIRS(Code)

  IF(ITK_CSWIG_TCL)
    SET(ITKWISH_EXECUTABLE "${CXX_TEST_PATH}/itkwish")
  ENDIF(ITK_CSWIG_TCL)
ELSE(ITK_BINARY_DIR)
  # We are building as a stand-alone project.
  SET(LIBRARY_OUTPUT_PATH ${ITKTesting_BINARY_DIR}/bin CACHE PATH "Single output directory for building all libraries.")
  SET(EXECUTABLE_OUTPUT_PATH ${ITKTesting_BINARY_DIR}/bin CACHE PATH "Single output directory for building all executables.")
  MARK_AS_ADVANCED(LIBRARY_OUTPUT_PATH EXECUTABLE_OUTPUT_PATH)
  SET(CXX_TEST_PATH ${EXECUTABLE_OUTPUT_PATH})

  # Configure Dart testing support.
  INCLUDE(${CMAKE_ROOT}/Modules/Dart.cmake)
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
    SUBDIRS(Code)
    IF(ITK_CSWIG_TCL)
      FIND_PROGRAM(ITKWISH_EXECUTABLE itkwish ${ITK_DIR}/bin "${ITK_DIR}/../../bin")
    ENDIF(ITK_CSWIG_TCL)
    IF(ITK_CSWIG_PYTHON)
      FIND_PROGRAM(PYTHON_EXECUTABLE
        NAMES python python2.3 python2.2 python2.1 python2.0 python1.6 python1.5
        PATHS
        [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\2.3\\InstallPath]
        [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\2.2\\InstallPath]
        [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\2.1\\InstallPath]
        [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\2.0\\InstallPath]
        [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\1.6\\InstallPath]
        [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\1.5\\InstallPath]
        )
      MARK_AS_ADVANCED(PYTHON_EXECUTABLE)
    ENDIF(ITK_CSWIG_PYTHON)
    IF(ITK_CSWIG_JAVA)
      FIND_PACKAGE(Java)
    ENDIF(ITK_CSWIG_JAVA)
  ELSE(ITK_FOUND)
    MESSAGE("ITK not found.  Please set ITK_DIR")
  ENDIF(ITK_FOUND)
ENDIF(ITK_BINARY_DIR)
