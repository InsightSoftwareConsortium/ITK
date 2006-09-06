
# configure the test driver
FIND_PROGRAM(IMAGE_COMPARE ImageCompare)

IF(CMAKE_CONFIGURATION_TYPES)

  FOREACH(config ${CMAKE_CONFIGURATION_TYPES})

    # WrapITK can be installed, without configuration dirs, or in the build tree with the config dir
    SET(PYTHON_TEST_LIB_PATH "${WrapITK_DIR}/lib/${config}/" "${WrapITK_DIR}/lib/")
    SET(PYTHON_TEST_PYTHON_PATH "${WrapITK_DIR}/Python/${config}/" "${WrapITK_DIR}/Python/" "${PROJECT_BINARY_DIR}/lib/${config}/")
    SET(PYTHON_TEST_WRAPITK_PYTHON_PATH "${PROJECT_BINARY_DIR}/Python/")

    CONFIGURE_FILE("${WRAP_ITK_CMAKE_DIR}/Python/Tests/pythonTestDriver.py.in"
                "${CMAKE_CURRENT_BINARY_DIR}/${config}/pythonTestDriver.py"
                @ONLY IMMEDIATE)

  ENDFOREACH(config)

ELSE(CMAKE_CONFIGURATION_TYPES)

  SET(PYTHON_TEST_LIB_PATH "${WrapITK_DIR}/lib/")
  SET(PYTHON_TEST_PYTHON_PATH "${WrapITK_DIR}/Python/")
  SET(PYTHON_TEST_WRAPITK_PYTHON_PATH "${PROJECT_BINARY_DIR}/Python/")

  CONFIGURE_FILE("${WRAP_ITK_CMAKE_DIR}/Python/Tests/pythonTestDriver.py.in"
              "${CMAKE_CURRENT_BINARY_DIR}/pythonTestDriver.py"
              @ONLY IMMEDIATE)

ENDIF(CMAKE_CONFIGURATION_TYPES)


SET(PYTHON_DRIVER "${PYTHON_EXECUTABLE}" "${CMAKE_CURRENT_BINARY_DIR}/\${CTEST_CONFIGURATION_TYPE}/pythonTestDriver.py")



  ADD_TEST(PythonFFTWImageFilter
    ${PYTHON_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/FFTWImageFilter.py
    ${PROJECT_SOURCE_DIR}/images/cthead1.png
    FFTWImageFilter.png
    --compare FFTWImageFilter.png ${PROJECT_SOURCE_DIR}/images/cthead1.png
  )

# some tests will fail if dim=2 and unsigned short are not wrapped
INTERSECTION(WRAP_2 2 "${WRAP_ITK_DIMS}")
IF(WRAP_unsigned_short AND WRAP_2)
ENDIF(WRAP_unsigned_short AND WRAP_2)


