set(test_input_dir ${itk-module_SOURCE_DIR}/test/Input)
set(test_baseline_dir ${itk-module_SOURCE_DIR}/test/Baseline)

list(FIND ITK_WRAP_IMAGE_DIMS 2 wrap_2_index)
if(ITK_WRAP_float AND wrap_2_index GREATER -1)
  itk_python_add_test(NAME PythonWatershedSegmentation1Test
    TEST_DRIVER_ARGS
      --compare ${ITK_TEST_OUTPUT_DIR}/PythonWatershedSegmentation1Test.png
                DATA{${test_baseline_dir}/PythonWatershedSegmentation1Test.png}
    COMMAND WatershedSegmentation1Test.py
      DATA{${test_input_dir}/VisibleWomanEyeSlice.png}
      ${ITK_TEST_OUTPUT_DIR}/PythonWatershedSegmentation1Test.png
    )
endif()
