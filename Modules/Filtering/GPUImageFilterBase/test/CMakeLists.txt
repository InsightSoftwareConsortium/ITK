if(ITK_USE_GPU)
  itk_module_test()

  set(ITKGPUImageFilterBase-tests itkGPUNeighborhoodOperatorImageFilterTest.cxx)

  createtestdriver(ITKGPUImageFilterBase "${ITKGPUImageFilterBase-Test_LIBRARIES}" "${ITKGPUImageFilterBase-tests}")

  itk_add_test(
    NAME itkGPUNeighborhoodOperatorImageFilterTest2D
    COMMAND
      ITKGPUImageFilterBaseTestDriver
      itkGPUNeighborhoodOperatorImageFilterTest
      DATA{Input/BrainProtonDensitySlice.png}
      ${ITK_TEST_OUTPUT_DIR}/gpuNeighborhoodOperatorImageFilterTest2D.mha
      2
    ITK_REMOVE_TEMPORARY_TEST_FILES
      ${ITK_TEST_OUTPUT_DIR}/gpuNeighborhoodOperatorImageFilterTest2D.mha
  )

  itk_add_test(
    NAME itkGPUNeighborhoodOperatorImageFilterTest3D
    COMMAND
      ITKGPUImageFilterBaseTestDriver
      itkGPUNeighborhoodOperatorImageFilterTest
      DATA{Input/HeadMRVolume.mha}
      ${ITK_TEST_OUTPUT_DIR}/gpuNeighborhoodOperatorImageFilterTest3D.mha
      3
    ITK_REMOVE_TEMPORARY_TEST_FILES
      ${ITK_TEST_OUTPUT_DIR}/gpuNeighborhoodOperatorImageFilterTest3D.mha
  )
endif()
