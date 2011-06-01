itk_module_test()
set(ITK-ImageFilterBaseTests
itkImageFilterBaseHeaderTest.cxx
itkNeighborhoodOperatorImageFilterTest.cxx
itkImageToImageFilterTest.cxx
itkVectorNeighborhoodOperatorImageFilterTest.cxx
itkGaussianImageSourceTest.cxx
itkMaskNeighborhoodOperatorImageFilterTest.cxx
)

CreateTestDriver(ITK-ImageFilterBase  "${ITK-ImageFilterBase-Test_LIBRARIES}" "${ITK-ImageFilterBaseTests}")

itk_add_test(NAME itkImageFilterBaseHeaderTest
      COMMAND ITK-ImageFilterBaseTestDriver itkImageFilterBaseHeaderTest)
itk_add_test(NAME itkNeighborhoodOperatorImageFilterTest
      COMMAND ITK-ImageFilterBaseTestDriver itkNeighborhoodOperatorImageFilterTest)
itk_add_test(NAME itkImageToImageFilterTest
      COMMAND ITK-ImageFilterBaseTestDriver itkImageToImageFilterTest)
itk_add_test(NAME itkVectorNeighborhoodOperatorImageFilterTest
      COMMAND ITK-ImageFilterBaseTestDriver itkVectorNeighborhoodOperatorImageFilterTest)
itk_add_test(NAME itkGaussianImageSourceTest
      COMMAND ITK-ImageFilterBaseTestDriver itkGaussianImageSourceTest)
itk_add_test(NAME itkMaskNeighborhoodOperatorImageFilterTest
      COMMAND ITK-ImageFilterBaseTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/MaskNeighborhoodOperatorImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/MaskNeighborhoodOperatorImageFilterTest.png
    itkMaskNeighborhoodOperatorImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/MaskNeighborhoodOperatorImageFilterTest.png)
