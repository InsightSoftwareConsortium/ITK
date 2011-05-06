itk_module_test()
set(ITK-ImageGradientTests
itkImageGradientHeaderTest.cxx
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

CreateTestDriver(ITK-ImageGradient  "${ITK-ImageGradient-Test_LIBRARIES}" "${ITK-ImageGradientTests}")

itk_add_test(NAME itkImageGradientHeaderTest
      COMMAND ITK-ImageGradientTestDriver itkImageGradientHeaderTest)
itk_add_test(NAME itkGradientImageFilterTest
      COMMAND ITK-ImageGradientTestDriver itkGradientImageFilterTest)
itk_add_test(NAME itkVectorGradientMagnitudeImageFilterTest1a
      COMMAND ITK-ImageGradientTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorGradientMagnitudeImageFilterTest1a.png
              ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest1a.png
    itkVectorGradientMagnitudeImageFilterTest1 ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest1a.png 0)
itk_add_test(NAME itkVectorGradientMagnitudeImageFilterTest1b
      COMMAND ITK-ImageGradientTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorGradientMagnitudeImageFilterTest1b.png
              ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest1b.png
    itkVectorGradientMagnitudeImageFilterTest1 ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest1b.png 1)
itk_add_test(NAME itkVectorGradientMagnitudeImageFilterTest2
      COMMAND ITK-ImageGradientTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorGradientMagnitudeImageFilterTest2.png
              ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest2.png
    itkVectorGradientMagnitudeImageFilterTest2 ${ITK_DATA_ROOT}/Input/VHFColor.mhd ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest2.png 0 5)
itk_add_test(NAME itkVectorGradientMagnitudeImageFilterTest2b
      COMMAND ITK-ImageGradientTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorGradientMagnitudeImageFilterTest2b.png
              ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest2b.png
    itkVectorGradientMagnitudeImageFilterTest2 ${ITK_DATA_ROOT}/Input/VHFColor.mhd ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest2b.png 1 5)
itk_add_test(NAME itkGradientMagnitudeImageFilterTest
      COMMAND ITK-ImageGradientTestDriver itkGradientMagnitudeImageFilterTest)
itk_add_test(NAME itkVectorGrtadientMagnitudeImageFilterTest3
      COMMAND ITK-ImageGradientTestDriver itkVectorGradientMagnitudeImageFilterTest3
              ${ITK_DATA_ROOT}/Input/VHFColor.mhd ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest3.mha 1)
itk_add_test(NAME itkGradientMagnitudeRecursiveGaussianFilterTest
      COMMAND ITK-ImageGradientTestDriver itkGradientMagnitudeRecursiveGaussianFilterTest)
itk_add_test(NAME itkGradientRecursiveGaussianFilterTest
      COMMAND ITK-ImageGradientTestDriver itkGradientRecursiveGaussianFilterTest)
itk_add_test(NAME itkGradientRecursiveGaussianFilterTest2
      COMMAND ITK-ImageGradientTestDriver itkGradientRecursiveGaussianFilterTest2)
itk_add_test(NAME itkDifferenceOfGaussiansGradientTest
      COMMAND ITK-ImageGradientTestDriver itkDifferenceOfGaussiansGradientTest)
