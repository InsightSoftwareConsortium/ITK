set(TESTING_OUTPUT_PATH "${CMAKE_CURRENT_BINARY_DIR}/Output")
make_directory(${TESTING_OUTPUT_PATH})
set(DATA_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../../test/Input)

itk_python_add_test(
  NAME MinimalPathExtraction_Real_DSA_01_02_RegularStepGradientDescent_Python
  COMMAND DigitalSubtractionAngiographyVesselPath.py
    ${TESTING_OUTPUT_PATH}/Real-DSA-01-02-RegularStepGradientDescentPython.png
    ${DATA_DIR}/Real-DSA-01-Speed-02.mhd
    ${DATA_DIR}/Real-DSA-01.path
    3.0
    4000
    0.25
    0.5
)
