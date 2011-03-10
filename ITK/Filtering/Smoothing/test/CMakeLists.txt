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

add_test(NAME itkSmoothingHeaderTest
      COMMAND ITK-SmoothingTestDriver itkSmoothingHeaderTest)
add_test(NAME itkDiscreteGaussianImageFilterTest2
      COMMAND ITK-SmoothingTestDriver
    --compare ${ITK_TEST_OUTPUT_DIR}/DiscreteGaussianImageFilterTest2_OutputA.mha
              ${ITK_DATA_ROOT}/Baseline/BasicFilters/DiscreteGaussianImageFilterTest2_OutputA.mha
    itkDiscreteGaussianImageFilterTest2 2 3 ${ITK_DATA_ROOT}/Input/RGBTestImage.tif 3.5 ${ITK_TEST_OUTPUT_DIR}/DiscreteGaussianImageFilterTest2_OutputA.mha)
add_test(NAME itkSmoothingRecursiveGaussianImageFilterTest
      COMMAND ITK-SmoothingTestDriver itkSmoothingRecursiveGaussianImageFilterTest)
add_test(NAME itkMeanImageFilterTest
      COMMAND ITK-SmoothingTestDriver itkMeanImageFilterTest)
add_test(NAME itkDiscreteGaussianImageFilterTest
      COMMAND ITK-SmoothingTestDriver itkDiscreteGaussianImageFilterTest)
add_test(NAME itkMedianImageFilterTest
      COMMAND ITK-SmoothingTestDriver itkMedianImageFilterTest)
