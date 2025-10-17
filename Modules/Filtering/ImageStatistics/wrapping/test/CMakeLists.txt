set(test_input_dir ${itk-module_SOURCE_DIR}/test/Input)

# let's make sure 3D uchar images are wrapped
list(FIND ITK_WRAP_IMAGE_DIMS 3 wrap_3_index)
if(ITK_WRAP_PYTHON AND ITK_WRAP_unsigned_char AND wrap_3_index GREATER -1)
  itk_python_add_test(NAME LabelOverlapMeasuresImageFilterTest
    TEST_DRIVER_ARGS
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/itkLabelOverlapMeasuresImageFilterTest.py
      DATA{${test_input_dir}/DzZ_T1.seg.nrrd}
      DATA{${test_input_dir}/DzZ_Seeds.seg.nrrd}
    )
endif()
