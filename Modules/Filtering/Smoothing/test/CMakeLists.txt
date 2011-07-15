itk_module_test()
set(ITKSmoothingTests
itkDiscreteGaussianImageFilterTest2.cxx
itkSmoothingRecursiveGaussianImageFilterTest.cxx
itkMeanImageFilterTest.cxx
itkDiscreteGaussianImageFilterTest.cxx
itkMedianImageFilterTest.cxx
itkSmoothingHeaderTest.cxx
itkRecursiveGaussianImageFiltersOnTensorsTest.cxx
itkRecursiveGaussianImageFiltersOnVectorImageTest.cxx
itkRecursiveGaussianImageFiltersTest.cxx
itkRecursiveGaussianScaleSpaceTest1.cxx
)

CreateTestDriver(ITKSmoothing  "${ITKSmoothing-Test_LIBRARIES}" "${ITKSmoothingTests}")

itk_add_test(NAME itkSmoothingHeaderTest
      COMMAND ITKSmoothingTestDriver itkSmoothingHeaderTest)
itk_add_test(NAME itkDiscreteGaussianImageFilterTest2
      COMMAND ITKSmoothingTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/DiscreteGaussianImageFilterTest2_OutputA.mha
              ${ITK_TEST_OUTPUT_DIR}/DiscreteGaussianImageFilterTest2_OutputA.mha
    itkDiscreteGaussianImageFilterTest2 2 3 ${ITK_DATA_ROOT}/Input/RGBTestImage.tif 3.5 ${ITK_TEST_OUTPUT_DIR}/DiscreteGaussianImageFilterTest2_OutputA.mha)
itk_add_test(NAME itkSmoothingRecursiveGaussianImageFilterTest
      COMMAND ITKSmoothingTestDriver itkSmoothingRecursiveGaussianImageFilterTest)
itk_add_test(NAME itkMeanImageFilterTest
      COMMAND ITKSmoothingTestDriver itkMeanImageFilterTest)
itk_add_test(NAME itkDiscreteGaussianImageFilterTest
      COMMAND ITKSmoothingTestDriver itkDiscreteGaussianImageFilterTest)
itk_add_test(NAME itkMedianImageFilterTest
      COMMAND ITKSmoothingTestDriver itkMedianImageFilterTest)
itk_add_test(NAME itkRecursiveGaussianImageFiltersOnTensorsTest
      COMMAND ITKSmoothingTestDriver itkRecursiveGaussianImageFiltersOnTensorsTest)
itk_add_test(NAME itkRecursiveGaussianImageFiltersOnVectorImageTest
      COMMAND ITKSmoothingTestDriver itkRecursiveGaussianImageFiltersOnVectorImageTest)
itk_add_test(NAME itkRecursiveGaussianImageFiltersTest
      COMMAND ITKSmoothingTestDriver itkRecursiveGaussianImageFiltersTest)
itk_add_test(NAME itkRecursiveGaussianScaleSpaceTest1
      COMMAND ITKSmoothingTestDriver
              itkRecursiveGaussianScaleSpaceTest1)
