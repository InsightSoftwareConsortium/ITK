itk_module_test()
set(ITK-ImageCompareTests
itkImageCompareHeaderTest.cxx
itkAbsoluteValueDifferenceImageFilterTest.cxx
itkSquaredDifferenceImageFilterTest.cxx
itkCheckerBoardImageFilterTest.cxx
itkConstrainedValueDifferenceImageFilterTest.cxx
itkSimilarityIndexImageFilterTest.cxx
itkSTAPLEImageFilterTest.cxx
itkDifferenceImageFilterTest.cxx
)

CreateTestDriver(ITK-ImageCompare  "${ITK-ImageCompare-Test_LIBRARIES}" "${ITK-ImageCompareTests}")

itk_add_test(NAME itkImageCompareHeaderTest
      COMMAND ITK-ImageCompareTestDriver itkImageCompareHeaderTest)
itk_add_test(NAME itkAbsoluteValueDifferenceImageFilterTest
      COMMAND ITK-ImageCompareTestDriver itkAbsoluteValueDifferenceImageFilterTest)
itk_add_test(NAME itkSquaredDifferenceImageFilterTest
      COMMAND ITK-ImageCompareTestDriver itkSquaredDifferenceImageFilterTest)
itk_add_test(NAME itkCheckerBoardImageFilterTest
      COMMAND ITK-ImageCompareTestDriver itkCheckerBoardImageFilterTest)
itk_add_test(NAME itkConstrainedValueDifferenceImageFilterTest
      COMMAND ITK-ImageCompareTestDriver itkConstrainedValueDifferenceImageFilterTest)
itk_add_test(NAME itkSimilarityIndexImageFilterTest
      COMMAND ITK-ImageCompareTestDriver itkSimilarityIndexImageFilterTest)
itk_add_test(NAME itkSTAPLEImageFilterTest
      COMMAND ITK-ImageCompareTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/STAPLEImageFilterTest.mha
              ${ITK_TEST_OUTPUT_DIR}/STAPLEImageFilterTest.mha
    itkSTAPLEImageFilterTest 2 ${ITK_TEST_OUTPUT_DIR}/STAPLEImageFilterTest.mha 255 0.5 ${ITK_DATA_ROOT}/Input/STAPLE1.png ${ITK_DATA_ROOT}/Input/STAPLE2.png ${ITK_DATA_ROOT}/Input/STAPLE3.png ${ITK_DATA_ROOT}/Input/STAPLE4.png)
itk_add_test(NAME itkDifferenceImageFilterTest
      COMMAND ITK-ImageCompareTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Common/itkDifferenceImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/itkDifferenceImageFilterTest.png
    itkDifferenceImageFilterTest ${ITK_DATA_ROOT}/Input/cake_easy.png ${ITK_DATA_ROOT}/Input/cake_hard.png ${ITK_TEST_OUTPUT_DIR}/itkDifferenceImageFilterTest.png 10 1)
