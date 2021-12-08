# some tests will fail if dim=2 and unsigned short are not wrapped
list(FIND ITK_WRAP_IMAGE_DIMS 2 wrap_2_index)
if(ITK_WRAP_PYTHON AND ITK_WRAP_unsigned_char AND wrap_2_index GREATER -1)
  itk_python_add_test(NAME itkGPUMeanImageFilterPythonTest2D
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/itkGPUMeanImageFilterTest.py
      DATA{../../test/Input/BrainProtonDensitySlice.png}
      ${ITK_TEST_OUTPUT_DIR}/itkGPUMeanImageFilterPythonTest2D.mha
      2
    )
endif()
