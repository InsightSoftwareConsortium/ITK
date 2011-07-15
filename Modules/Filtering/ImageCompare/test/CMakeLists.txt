itk_module_test()
set(ITKImageCompareTests
itkImageCompareHeaderTest.cxx
itkAbsoluteValueDifferenceImageFilterTest.cxx
itkSquaredDifferenceImageFilterTest.cxx
itkCheckerBoardImageFilterTest.cxx
itkConstrainedValueDifferenceImageFilterTest.cxx
itkSimilarityIndexImageFilterTest.cxx
itkSTAPLEImageFilterTest.cxx
itkDifferenceImageFilterTest.cxx
)

CreateTestDriver(ITKImageCompare  "${ITKImageCompare-Test_LIBRARIES}" "${ITKImageCompareTests}")

itk_add_test(NAME itkImageCompareHeaderTest
      COMMAND ITKImageCompareTestDriver itkImageCompareHeaderTest)
itk_add_test(NAME itkAbsoluteValueDifferenceImageFilterTest
      COMMAND ITKImageCompareTestDriver itkAbsoluteValueDifferenceImageFilterTest)
itk_add_test(NAME itkSquaredDifferenceImageFilterTest
      COMMAND ITKImageCompareTestDriver itkSquaredDifferenceImageFilterTest)
itk_add_test(NAME itkCheckerBoardImageFilterTest
      COMMAND ITKImageCompareTestDriver itkCheckerBoardImageFilterTest)
itk_add_test(NAME itkConstrainedValueDifferenceImageFilterTest
      COMMAND ITKImageCompareTestDriver itkConstrainedValueDifferenceImageFilterTest)
itk_add_test(NAME itkSimilarityIndexImageFilterTest
      COMMAND ITKImageCompareTestDriver itkSimilarityIndexImageFilterTest)
itk_add_test(NAME itkSTAPLEImageFilterTest
      COMMAND ITKImageCompareTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/STAPLEImageFilterTest.mha
              ${ITK_TEST_OUTPUT_DIR}/STAPLEImageFilterTest.mha
    itkSTAPLEImageFilterTest 2 ${ITK_TEST_OUTPUT_DIR}/STAPLEImageFilterTest.mha 255 0.5 ${ITK_DATA_ROOT}/Input/STAPLE1.png ${ITK_DATA_ROOT}/Input/STAPLE2.png ${ITK_DATA_ROOT}/Input/STAPLE3.png ${ITK_DATA_ROOT}/Input/STAPLE4.png)
itk_add_test(NAME itkDifferenceImageFilterTest
      COMMAND ITKImageCompareTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Common/itkDifferenceImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/itkDifferenceImageFilterTest.png
    itkDifferenceImageFilterTest ${ITK_DATA_ROOT}/Input/cake_easy.png ${ITK_DATA_ROOT}/Input/cake_hard.png ${ITK_TEST_OUTPUT_DIR}/itkDifferenceImageFilterTest.png 10 1)
