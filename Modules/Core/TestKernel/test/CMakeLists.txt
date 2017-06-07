itk_module_test()
set(ITKTestKernelTests
itkTestingExtractSliceImageFilterTest.cxx
itkTestingStretchIntensityImageFilterTest.cxx
)

CreateTestDriver(ITKTestKernel "${ITKTestKernel-Test_LIBRARIES}" "${ITKTestKernelTests}")

set(BASELINE "${ITK_DATA_ROOT}/Baseline/Common")
set(TEMP ${ITK_TEST_OUTPUT_DIR})

itk_add_test(NAME itkTestingExtractSliceImageFilterTest
  COMMAND ITKTestKernelTestDriver itkTestingExtractSliceImageFilterTest)

itk_add_test(NAME itkTestingStretchIntensityImageFilterTest
  COMMAND ITKTestKernelTestDriver itkTestingStretchIntensityImageFilterTest
  )

set(ITKTestKernelGTests
  itkGoogleTest.cxx
  itkGoogleTestFixture.cxx
)


CreateGoogleTestDriver(ITKTestKernel "${ITKTestKernel-Test_LIBRARIES}" "${ITKTestKernelGTests}")


