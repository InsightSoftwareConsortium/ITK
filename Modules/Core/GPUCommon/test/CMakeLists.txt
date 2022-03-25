if (ITK_USE_GPU)

itk_module_test()

set(ITKGPUCommon-tests
 itkGPUImageTest.cxx
 itkGPUReductionTest.cxx
 )

CreateTestDriver(ITKGPUCommon "${ITKGPUCommon-Test_LIBRARIES}" "${ITKGPUCommon-tests}")
itk_add_test(NAME itkGPUImageTest COMMAND ITKGPUCommonTestDriver itkGPUImageTest)
itk_add_test(NAME itkGPUReductionTest COMMAND ITKGPUCommonTestDriver itkGPUReductionTest)

endif()
