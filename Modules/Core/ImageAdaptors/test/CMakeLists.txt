itk_module_test()
set(ITKImageAdaptorsTests
itkImageAdaptorTest.cxx
itkRGBToVectorImageAdaptorTest.cxx
itkVectorImageTest.cxx
)

CreateTestDriver(ITKImageAdaptors  "${ITKImageAdaptors-Test_LIBRARIES}" "${ITKImageAdaptorsTests}")

itk_add_test(NAME itkImageAdaptorTest
      COMMAND ITKImageAdaptorsTestDriver itkImageAdaptorTest)
itk_add_test(NAME itkRGBToVectorImageAdaptorTest
      COMMAND ITKImageAdaptorsTestDriver itkRGBToVectorImageAdaptorTest)
itk_add_test(NAME itkVectorImageTest
      COMMAND ITKImageAdaptorsTestDriver itkVectorImageTest
              ${ITK_TEST_OUTPUT_DIR}/VectorImage.nrrd ${ITK_TEST_OUTPUT_DIR}/VectorImage.mhd)
