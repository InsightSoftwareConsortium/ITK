set(test_input_dir ${itk-module_SOURCE_DIR}/test/Input)
set(test_baseline_dir ${itk-module_SOURCE_DIR}/test/Baseline)

# let's make sure 3D uchar images are wrapped
list(
  FIND
  ITK_WRAP_IMAGE_DIMS
  3
  wrap_3_index)
if(ITK_WRAP_PYTHON
   AND ITK_WRAP_unsigned_char
   AND wrap_3_index GREATER -1)

  itk_python_add_test(
    NAME
    BinaryFillholeImageFilterTestPython2D
    TEST_DRIVER_ARGS
    --compare
    DATA{${test_baseline_dir}/itkBinaryFillholeImageFilterTest1.png}
    ${ITK_TEST_OUTPUT_DIR}/itkBinaryFillholeImageFilterTest2D.png
    COMMAND
    ${CMAKE_CURRENT_SOURCE_DIR}/BinaryFillholeImageFilterTest.py
    DATA{${ITK_DATA_ROOT}/Input/Spots.png}
    ${ITK_TEST_OUTPUT_DIR}/itkBinaryFillholeImageFilterTest2D.png
    1
    255)

  itk_python_add_test(
    NAME
    BinaryFillholeImageFilterTestPython0
    TEST_DRIVER_ARGS
    --compare
    DATA{${test_baseline_dir}/fillhole_test_3D_0.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/itkBinaryFillholeImageFilterTest3D_0.nrrd
    COMMAND
    ${CMAKE_CURRENT_SOURCE_DIR}/BinaryFillholeImageFilterTest.py
    DATA{${test_input_dir}/fillhole_test_3D.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/itkBinaryFillholeImageFilterTest3D_0.nrrd
    0
    1)

  itk_python_add_test(
    NAME
    BinaryFillholeImageFilterTestPython1
    TEST_DRIVER_ARGS
    --compare
    DATA{${test_baseline_dir}/fillhole_test_3D_1.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/itkBinaryFillholeImageFilterTest3D_1.nrrd
    COMMAND
    ${CMAKE_CURRENT_SOURCE_DIR}/BinaryFillholeImageFilterTest.py
    DATA{${test_input_dir}/fillhole_test_3D.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/itkBinaryFillholeImageFilterTest3D_1.nrrd
    1
    1)

endif()
