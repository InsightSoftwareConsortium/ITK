set(test_input_dir ${itk-module_SOURCE_DIR}/test/Input)


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
            OrientImageFilterTest
            TEST_DRIVER_ARGS
            --compare
            DATA{Baseline/OrientImageFilterTest.mha}
            ${ITK_TEST_OUTPUT_DIR}/OrientImageFilterTest.mha
            COMMAND
            ${CMAKE_CURRENT_SOURCE_DIR}/OrientImageFilterTest.py
            DATA{${ITK_DATA_ROOT}/Baseline/Filtering/SubsampleVolume.mha}
            ${ITK_TEST_OUTPUT_DIR}/OrientImageFilterTest.mha)

endif()
