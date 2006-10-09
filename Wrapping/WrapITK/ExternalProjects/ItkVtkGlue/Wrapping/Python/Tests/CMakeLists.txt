
# configure the test driver
SET(PYTHON_DRIVER ${PYTHON_DRIVER}
  --add-before-env PYTHONPATH "${LIBRARY_OUTPUT_PATH}/\${CTEST_CONFIGURATION_TYPE}"
  --add-before-env WRAPITK_PYTHON_PATH "${PROJECT_BINARY_DIR}/Python/\${CTEST_CONFIGURATION_TYPE}"
)


ADD_TEST(SimpleItkVtkPipeline
  ${PYTHON_DRIVER}
  ${CMAKE_CURRENT_SOURCE_DIR}/simpleItkVtkPipeline.py
  ${PROJECT_SOURCE_DIR}/images/cthead1.png
  SimpleItkVtkPipeline.png
  --compare SimpleItkVtkPipeline.png ${PROJECT_SOURCE_DIR}/images/cthead1.png
)

# some tests will fail if dim=2 and unsigned short are not wrapped
INTERSECTION(WRAP_2 2 "${WRAP_ITK_DIMS}")
IF(WRAP_unsigned_short AND WRAP_2)
ENDIF(WRAP_unsigned_short AND WRAP_2)


