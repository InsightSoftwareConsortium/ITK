itk_module_test()
set(ITK-SmoothingTests
itkDiscreteGaussianImageFilterTest2.cxx
itkSmoothingRecursiveGaussianImageFilterTest.cxx
itkMeanImageFilterTest.cxx
itkDiscreteGaussianImageFilterTest.cxx
itkMedianImageFilterTest.cxx
itkSmoothingHeaderTest.cxx
)

CreateTestDriver(ITK-Smoothing  "${ITK-Smoothing-Test_LIBRARIES}" "${ITK-SmoothingTests}")

itk_add_test(NAME itkSmoothingHeaderTest
      COMMAND ITK-SmoothingTestDriver itkSmoothingHeaderTest)
itk_add_test(NAME itkDiscreteGaussianImageFilterTest2
      COMMAND ITK-SmoothingTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/DiscreteGaussianImageFilterTest2_OutputA.mha
              ${ITK_TEST_OUTPUT_DIR}/DiscreteGaussianImageFilterTest2_OutputA.mha
    itkDiscreteGaussianImageFilterTest2 2 3 ${ITK_DATA_ROOT}/Input/RGBTestImage.tif 3.5 ${ITK_TEST_OUTPUT_DIR}/DiscreteGaussianImageFilterTest2_OutputA.mha)
itk_add_test(NAME itkSmoothingRecursiveGaussianImageFilterTest
      COMMAND ITK-SmoothingTestDriver itkSmoothingRecursiveGaussianImageFilterTest)
itk_add_test(NAME itkMeanImageFilterTest
      COMMAND ITK-SmoothingTestDriver itkMeanImageFilterTest)
itk_add_test(NAME itkDiscreteGaussianImageFilterTest
      COMMAND ITK-SmoothingTestDriver itkDiscreteGaussianImageFilterTest)
itk_add_test(NAME itkMedianImageFilterTest
      COMMAND ITK-SmoothingTestDriver itkMedianImageFilterTest)
