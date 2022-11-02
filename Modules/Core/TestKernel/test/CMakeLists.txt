itk_module_test()
set(ITKTestKernelTests
itkRandomImageSourceAttributesTest.cxx
itkRandomImageSourceValuesTest.cxx
itkTestingExtractSliceImageFilterTest.cxx
itkTestingStretchIntensityImageFilterTest.cxx
)

CreateTestDriver(ITKTestKernel "${ITKTestKernel-Test_LIBRARIES}" "${ITKTestKernelTests}")

set(BASELINE "${ITK_DATA_ROOT}/Baseline/Common")
set(TEMP ${ITK_TEST_OUTPUT_DIR})

itk_add_test(NAME itkRandomImageSourceAttributesTest
  COMMAND ITKTestKernelTestDriver itkRandomImageSourceAttributesTest)

itk_add_test(NAME itkRandomImageSourceValuesTest
  COMMAND ITKTestKernelTestDriver
  --compare DATA{Baseline/itkRandomImageSourceValuesTest.mha} ${ITK_TEST_OUTPUT_DIR}/itkRandomImageSourceValuesTest.mha
  itkRandomImageSourceValuesTest ${ITK_TEST_OUTPUT_DIR}/itkRandomImageSourceValuesTest.mha)

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


