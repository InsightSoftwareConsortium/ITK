list(FIND ITK_WRAP_IMAGE_DIMS 2 wrap_2_index)
if(ITK_WRAP_PYTHON AND ITK_WRAP_float AND wrap_2_index GREATER -1)
  itk_python_add_test(NAME PythonFFTConvolutionImageFilterTest
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/FFTConvolutionImageFilterTest.py
            DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
            DATA{${ITK_DATA_ROOT}/Input/sobel_x.nii.gz}
            ${ITK_TEST_OUTPUT_DIR}/PythonFFTConvolutionImageFilterTest.nrrd
  )
endif()
