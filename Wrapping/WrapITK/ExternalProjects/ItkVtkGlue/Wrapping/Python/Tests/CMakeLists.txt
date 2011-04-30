
# configure the test driver
set(PYTHON_DRIVER ${PYTHON_DRIVER}
  --add-before-env PYTHONPATH "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/\${CTEST_CONFIGURATION_TYPE}"
  --add-before-env WRAPITK_PYTHON_PATH "${PROJECT_BINARY_DIR}/Python/\${CTEST_CONFIGURATION_TYPE}"
  "${PYTHON_EXECUTABLE}"
)


itk_add_test(NAME SimpleItkVtkPipeline
  COMMAND ${PYTHON_DRIVER}
  ${CMAKE_CURRENT_SOURCE_DIR}/simpleItkVtkPipeline.py
  ${PROJECT_SOURCE_DIR}/images/cthead1.png
  SimpleItkVtkPipeline.png
  --compare SimpleItkVtkPipeline.png ${PROJECT_SOURCE_DIR}/images/cthead1.png
)

# some tests will fail if dim=2 and unsigned short are not wrapped
INTERSECTION(WRAP_2 2 "${WRAP_ITK_DIMS}")
if(WRAP_unsigned_short AND WRAP_2)
endif(WRAP_unsigned_short AND WRAP_2)
