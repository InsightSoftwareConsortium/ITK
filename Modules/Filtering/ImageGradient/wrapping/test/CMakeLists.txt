set(test_input_dir ${itk-module_SOURCE_DIR}/test/Input)

list(FIND ITK_WRAP_IMAGE_DIMS 2 wrap_2_index)
if(ITK_WRAP_float AND wrap_2_index GREATER -1)
  itk_python_add_test(NAME PythonGradientMagnitudeRecursiveGaussianImageFilterTest
    TEST_DRIVER_ARGS
      --compare ${ITK_TEST_OUTPUT_DIR}/GradientMagnitudeRecursiveGaussianImageFilterTest.png
              DATA{Baseline/PythonGradientMagnitudeRecursiveGaussianImageFilterTest.png}
    COMMAND GradientMagnitudeRecursiveGaussianImageFilterTest.py
      DATA{${test_input_dir}/BrainProtonDensitySlice.png}
      ${ITK_TEST_OUTPUT_DIR}/GradientMagnitudeRecursiveGaussianImageFilterTest.png
      5
    )
endif()
