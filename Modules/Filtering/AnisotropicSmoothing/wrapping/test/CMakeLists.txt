set(test_input_dir ${itk-module_SOURCE_DIR}/test/Input)

list(FIND ITK_WRAP_IMAGE_DIMS 2 wrap_2_index)
if(ITK_WRAP_PYTHON AND ITK_WRAP_float AND wrap_2_index GREATER -1)
  itk_python_add_test(NAME PythonCurvatureAnisotropicDiffusionImageFilterTest
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/CurvatureAnisotropicDiffusionImageFilterTest.py
      DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
      ${ITK_TEST_OUTPUT_DIR}/CurvatureAnisotropicDiffusionImageFilterTest.png
      5 0.125 3
    )
  itk_python_add_test(NAME PythonSmoothingRecursiveGaussianImageFilterTest
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/SmoothingRecursiveGaussianImageFilterTest.py
      DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
      ${ITK_TEST_OUTPUT_DIR}/SmoothingRecursiveGaussianImageFilterTest.png
      3
    )
  itk_python_add_test(NAME PythonGradientAnisotropicDiffusionImageFilterTest
    TEST_DRIVER_ARGS
      --compare ${ITK_TEST_OUTPUT_DIR}/PythonGradientAnisotropicDiffusionImageFilterTest.png
                DATA{Baseline/PythonGradientAnisotropicDiffusionImageFilterTest.png}
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/GradientAnisotropicDiffusionImageFilterTest.py
      DATA{${test_input_dir}/BrainProtonDensitySlice.png}
      ${ITK_TEST_OUTPUT_DIR}/PythonGradientAnisotropicDiffusionImageFilterTest.png
      5 0.125 1.0
    )
endif()
