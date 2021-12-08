set(test_input_dir ${itk-module_SOURCE_DIR}/test/Input)
set(test_baseline_dir ${itk-module_SOURCE_DIR}/test/Baseline)

list(FIND ITK_WRAP_IMAGE_DIMS 2 wrap_2_index)
if(ITK_WRAP_PYTHON AND ITK_WRAP_float AND wrap_2_index GREATER -1)
  itk_python_add_test(NAME PythonFastMarchingLeftVentricleTest
    TEST_DRIVER_ARGS
      --compare ${ITK_TEST_OUTPUT_DIR}/FastMarchingLeftVentricleTest.png
                DATA{${test_baseline_dir}/FastMarchingLeftVentricleTest.png}
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/FastMarchingImageFilterTest.py
    DATA{${test_input_dir}/BrainProtonDensitySlice.png}
      ${ITK_TEST_OUTPUT_DIR}/FastMarchingLeftVentricleTest.png
      81 114 1.0 -0.5 3.0 100 100
    )

  itk_python_add_test(NAME PythonFastMarchingRightVentricleTest
    TEST_DRIVER_ARGS
      --compare ${ITK_TEST_OUTPUT_DIR}/FastMarchingRightVentricleTest.png
                DATA{${test_baseline_dir}/FastMarchingRightVentricleTest.png}
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/FastMarchingImageFilterTest.py
      DATA{${test_input_dir}/BrainProtonDensitySlice.png}
      ${ITK_TEST_OUTPUT_DIR}/FastMarchingRightVentricleTest.png
      99 114 1.0 -0.5 3.0 100 100
    )

  itk_python_add_test(NAME PythonFastMarchingWhiteMatterTest
    TEST_DRIVER_ARGS
      --compare ${ITK_TEST_OUTPUT_DIR}/FastMarchingWhiteMatterTest.png
                DATA{${test_baseline_dir}/FastMarchingWhiteMatterTest.png}
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/FastMarchingImageFilterTest.py
      DATA{${test_input_dir}/BrainProtonDensitySlice.png}
      ${ITK_TEST_OUTPUT_DIR}/FastMarchingWhiteMatterTest.png
      56 92 1.0 -0.3 2.0 200 100
    )

  itk_python_add_test(NAME PythonFastMarchingGrayMatterTest
    TEST_DRIVER_ARGS
      --compare ${ITK_TEST_OUTPUT_DIR}/FastMarchingGrayMatterTest.png
                DATA{${test_baseline_dir}/FastMarchingGrayMatterTest.png}
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/FastMarchingImageFilterTest.py
      DATA{${test_input_dir}/BrainProtonDensitySlice.png}
      ${ITK_TEST_OUTPUT_DIR}/FastMarchingGrayMatterTest.png
      40 90 .5 -0.3 2.0 200 100
    )
endif()
