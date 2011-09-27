itk_module_test()
set(ITKImageGradientTests
itkGradientImageFilterTest.cxx
itkVectorGradientMagnitudeImageFilterTest1.cxx
itkVectorGradientMagnitudeImageFilterTest2.cxx
itkGradientMagnitudeImageFilterTest.cxx
itkVectorGradientMagnitudeImageFilterTest3.cxx
itkGradientMagnitudeRecursiveGaussianFilterTest.cxx
itkGradientRecursiveGaussianFilterTest.cxx
itkGradientRecursiveGaussianFilterTest2.cxx
itkDifferenceOfGaussiansGradientTest.cxx
)

CreateTestDriver(ITKImageGradient  "${ITKImageGradient-Test_LIBRARIES}" "${ITKImageGradientTests}")

itk_add_test(NAME itkGradientImageFilterTest
      COMMAND ITKImageGradientTestDriver itkGradientImageFilterTest)
itk_add_test(NAME itkVectorGradientMagnitudeImageFilterTest1a
      COMMAND ITKImageGradientTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorGradientMagnitudeImageFilterTest1a.png
              ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest1a.png
    itkVectorGradientMagnitudeImageFilterTest1 ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest1a.png 0)
itk_add_test(NAME itkVectorGradientMagnitudeImageFilterTest1b
      COMMAND ITKImageGradientTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorGradientMagnitudeImageFilterTest1b.png
              ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest1b.png
    itkVectorGradientMagnitudeImageFilterTest1 ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest1b.png 1)
itk_add_test(NAME itkVectorGradientMagnitudeImageFilterTest2
      COMMAND ITKImageGradientTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorGradientMagnitudeImageFilterTest2.png
              ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest2.png
    itkVectorGradientMagnitudeImageFilterTest2 ${ITK_DATA_ROOT}/Input/VHFColor.mhd ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest2.png 0 5)
itk_add_test(NAME itkVectorGradientMagnitudeImageFilterTest2b
      COMMAND ITKImageGradientTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorGradientMagnitudeImageFilterTest2b.png
              ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest2b.png
    itkVectorGradientMagnitudeImageFilterTest2 ${ITK_DATA_ROOT}/Input/VHFColor.mhd ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest2b.png 1 5)
itk_add_test(NAME itkGradientMagnitudeImageFilterTest
      COMMAND ITKImageGradientTestDriver itkGradientMagnitudeImageFilterTest)
itk_add_test(NAME itkVectorGrtadientMagnitudeImageFilterTest3
      COMMAND ITKImageGradientTestDriver itkVectorGradientMagnitudeImageFilterTest3
              ${ITK_DATA_ROOT}/Input/VHFColor.mhd ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest3.mha 1)
itk_add_test(NAME itkGradientMagnitudeRecursiveGaussianFilterTest
      COMMAND ITKImageGradientTestDriver itkGradientMagnitudeRecursiveGaussianFilterTest)
itk_add_test(NAME itkGradientRecursiveGaussianFilterTest
      COMMAND ITKImageGradientTestDriver itkGradientRecursiveGaussianFilterTest)
itk_add_test(NAME itkGradientRecursiveGaussianFilterTest2
      COMMAND ITKImageGradientTestDriver itkGradientRecursiveGaussianFilterTest2)
itk_add_test(NAME itkDifferenceOfGaussiansGradientTest
      COMMAND ITKImageGradientTestDriver itkDifferenceOfGaussiansGradientTest)
