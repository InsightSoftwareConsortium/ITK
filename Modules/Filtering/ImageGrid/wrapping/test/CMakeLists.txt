set(test_input_dir ${itk-module_SOURCE_DIR}/test/Input)

# some tests will fail if dim=2 and unsigned short are not wrapped
list(FIND ITK_WRAP_IMAGE_DIMS 2 wrap_2_index)
if(ITK_WRAP_PYTHON AND ITK_WRAP_unsigned_char AND wrap_2_index GREATER -1)
  itk_python_add_test(NAME PythonResampleImageFilterTest1
    TEST_DRIVER_ARGS
    --compare ${ITK_TEST_OUTPUT_DIR}/PythonResampleImageFilterTest1.png
              DATA{Baseline/PythonResampleImageFilterTest1.png}
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/ResampleImageFilterTest.py
      DATA{${test_input_dir}/BrainProtonDensitySlice.png}
      ${ITK_TEST_OUTPUT_DIR}/PythonResampleImageFilterTest1.png
      0
    )

  itk_python_add_test(NAME PythonResampleImageFilterTest2
    TEST_DRIVER_ARGS
      --compare ${ITK_TEST_OUTPUT_DIR}/PythonResampleImageFilterTest2.png
                DATA{Baseline/PythonResampleImageFilterTest2.png}
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/ResampleImageFilterTest.py
      DATA{${test_input_dir}/BrainProtonDensitySlice.png}
      ${ITK_TEST_OUTPUT_DIR}/PythonResampleImageFilterTest2.png
      1
    )

  itk_python_add_test(NAME PythonResampleImageFilterTest3
    TEST_DRIVER_ARGS
      --compare ${ITK_TEST_OUTPUT_DIR}/PythonResampleImageFilterTest3.png
      DATA{Baseline/PythonResampleImageFilterTest3.png}
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/ResampleImageFilterTest.py
      DATA{${test_input_dir}/BrainProtonDensitySlice.png}
      ${ITK_TEST_OUTPUT_DIR}/PythonResampleImageFilterTest3.png
      2
    )

  itk_python_add_test(NAME PythonResampleImageFilterTest4
    TEST_DRIVER_ARGS
      --compare ${ITK_TEST_OUTPUT_DIR}/PythonResampleImageFilterTest4.png
                DATA{Baseline/PythonResampleImageFilterTest4.png}
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/ResampleImageFilterTest.py
      DATA{${test_input_dir}/BrainProtonDensitySlice.png}
      ${ITK_TEST_OUTPUT_DIR}/PythonResampleImageFilterTest4.png
      3
    )
endif()

# let's make sure 3D uchar images are wrapped
list(FIND ITK_WRAP_IMAGE_DIMS 3 wrap_3_index)
if(ITK_WRAP_PYTHON AND ITK_WRAP_unsigned_char AND wrap_2_index GREATER -1)
  itk_python_add_test(NAME OrientImageFilterTest
    TEST_DRIVER_ARGS
      --compare DATA{Baseline/OrientImageFilterTest.mha}
                ${ITK_TEST_OUTPUT_DIR}/OrientImageFilterTest.mha
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/OrientImageFilterTest.py
      DATA{${ITK_DATA_ROOT}/Baseline/Filtering/SubsampleVolume.mha}
      ${ITK_TEST_OUTPUT_DIR}/OrientImageFilterTest.mha
    )
endif()
