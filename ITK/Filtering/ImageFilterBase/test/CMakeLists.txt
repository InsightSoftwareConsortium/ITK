itk_module_test()
set(ITK-ImageFilterBaseTests
itkImageFilterBaseHeaderTest.cxx
itkNeighborhoodOperatorImageFilterTest.cxx
itkImageToImageFilterTest.cxx
itkVectorNeighborhoodOperatorImageFilterTest.cxx
itkGaussianImageSourceTest.cxx
)

CreateTestDriver(ITK-ImageFilterBase  "${ITK-ImageFilterBase-Test_LIBRARIES}" "${ITK-ImageFilterBaseTests}")

add_test(NAME itkImageFilterBaseHeaderTest
      COMMAND ITK-ImageFilterBaseTestDriver itkImageFilterBaseHeaderTest)
add_test(NAME itkNeighborhoodOperatorImageFilterTest
      COMMAND ITK-ImageFilterBaseTestDriver itkNeighborhoodOperatorImageFilterTest)
add_test(NAME itkImageToImageFilterTest
      COMMAND ITK-ImageFilterBaseTestDriver itkImageToImageFilterTest)
add_test(NAME itkVectorNeighborhoodOperatorImageFilterTest
      COMMAND ITK-ImageFilterBaseTestDriver itkVectorNeighborhoodOperatorImageFilterTest)
add_test(NAME itkGaussianImageSourceTest
      COMMAND ITK-ImageFilterBaseTestDriver itkGaussianImageSourceTest)
