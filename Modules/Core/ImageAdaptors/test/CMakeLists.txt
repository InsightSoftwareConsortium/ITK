itk_module_test()
set(
  ITKImageAdaptorsTests
  itkComplexConjugateImageAdaptorTest.cxx
  itkImageAdaptorTest.cxx
  itkRGBToVectorImageAdaptorTest.cxx
  itkVectorImageTest.cxx
  itkVectorImageToImageAdaptorTest.cxx
)

createtestdriver(ITKImageAdaptors "${ITKImageAdaptors-Test_LIBRARIES}" "${ITKImageAdaptorsTests}")

itk_add_test(
  NAME itkImageAdaptorTest
  COMMAND
    ITKImageAdaptorsTestDriver
    itkImageAdaptorTest
)
itk_add_test(
  NAME itkRGBToVectorImageAdaptorTest
  COMMAND
    ITKImageAdaptorsTestDriver
    itkRGBToVectorImageAdaptorTest
)
itk_add_test(
  NAME itkVectorImageTest
  COMMAND
    ITKImageAdaptorsTestDriver
    itkVectorImageTest
    ${ITK_TEST_OUTPUT_DIR}/VectorImage.nrrd
    ${ITK_TEST_OUTPUT_DIR}/VectorImage.mhd
)
itk_add_test(
  NAME itkComplexConjugateImageAdaptorTest
  COMMAND
    ITKImageAdaptorsTestDriver
    itkComplexConjugateImageAdaptorTest
)
itk_add_test(
  NAME itkVectorImageToImageAdaptorTest
  COMMAND
    ITKImageAdaptorsTestDriver
    itkVectorImageToImageAdaptorTest
)

set(
  ITKImageAdaptorsGTests
  itkImageAdaptorGTest.cxx
  itkNthElementPixelAccessorGTest.cxx
)

creategoogletestdriver(ITKImageAdaptors "${ITKImageAdaptors-Test_LIBRARIES}" "${ITKImageAdaptorsGTests}")
