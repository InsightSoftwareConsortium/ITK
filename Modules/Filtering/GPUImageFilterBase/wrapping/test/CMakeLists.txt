itk_python_add_test(NAME itkGPUNeighborhoodOperatorImageFilterPythonTest2D
  TEST_DRIVER_ARGS
  COMMAND itkGPUNeighborhoodOperatorImageFilterTest.py
    ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
    ${ITK_TEST_OUTPUT_DIR}/gpuNeighborhoodOperatorImageFilterTest2DPython.mha 2
  )
