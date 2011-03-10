itk_module_test()
set(ITK-ImageGradientTests
itkImageGradientHeaderTest.cxx
itkGradientImageFilterTest.cxx
itkGradientToMagnitudeImageFilterTest.cxx
itkVectorGradientMagnitudeImageFilterTest1.cxx
itkVectorGradientMagnitudeImageFilterTest2.cxx
itkGradientMagnitudeImageFilterTest.cxx
)

CreateTestDriver(ITK-ImageGradient  "${ITK-ImageGradient-Test_LIBRARIES}" "${ITK-ImageGradientTests}")

add_test(NAME itkImageGradientHeaderTest
      COMMAND ITK-ImageGradientTestDriver itkImageGradientHeaderTest)
add_test(NAME itkGradientImageFilterTest
      COMMAND ITK-ImageGradientTestDriver itkGradientImageFilterTest)
add_test(NAME itkGradientToMagnitudeImageFilterTest
      COMMAND ITK-ImageGradientTestDriver itkGradientToMagnitudeImageFilterTest)
add_test(NAME itkVectorGradientMagnitudeImageFilterTest1a
      COMMAND ITK-ImageGradientTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorGradientMagnitudeImageFilterTest1a.png
              ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest1a.png
    itkVectorGradientMagnitudeImageFilterTest1 ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest1a.png 0)
add_test(NAME itkVectorGradientMagnitudeImageFilterTest1b
      COMMAND ITK-ImageGradientTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorGradientMagnitudeImageFilterTest1b.png
              ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest1b.png
    itkVectorGradientMagnitudeImageFilterTest1 ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest1b.png 1)
add_test(NAME itkVectorGradientMagnitudeImageFilterTest2
      COMMAND ITK-ImageGradientTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorGradientMagnitudeImageFilterTest2.png
              ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest2.png
    itkVectorGradientMagnitudeImageFilterTest2 ${ITK_DATA_ROOT}/Input/VHFColor.mhd ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest2.png 0 5)
add_test(NAME itkVectorGradientMagnitudeImageFilterTest2b
      COMMAND ITK-ImageGradientTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorGradientMagnitudeImageFilterTest2b.png
              ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest2b.png
    itkVectorGradientMagnitudeImageFilterTest2 ${ITK_DATA_ROOT}/Input/VHFColor.mhd ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest2b.png 1 5)
add_test(NAME itkGradientMagnitudeImageFilterTest
      COMMAND ITK-ImageGradientTestDriver itkGradientMagnitudeImageFilterTest)
