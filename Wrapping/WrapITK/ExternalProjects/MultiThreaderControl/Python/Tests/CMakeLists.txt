
# configure the test driver
find_program(IMAGE_COMPARE ImageCompare)
set(PYTHON_DRIVER ${PYTHON_DRIVER}  --add-before-env PYTHONPATH "${LIBRARY_OUTPUT_PATH}/\${CTEST_CONFIGURATION_TYPE}"
  --add-before-env WRAPITK_PYTHON_PATH "${PROJECT_BINARY_DIR}/Python/\${CTEST_CONFIGURATION_TYPE}"
)


add_test(PythonMultithreader
  ${PYTHON_DRIVER}
  ${CMAKE_CURRENT_SOURCE_DIR}/multithreader.py
)


