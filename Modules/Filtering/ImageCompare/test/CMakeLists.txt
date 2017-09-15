itk_module_test()
set(ITKImageCompareTests
itkAbsoluteValueDifferenceImageFilterTest.cxx
itkSquaredDifferenceImageFilterTest.cxx
itkCheckerBoardImageFilterTest.cxx
itkConstrainedValueDifferenceImageFilterTest.cxx
itkSimilarityIndexImageFilterTest.cxx
itkSTAPLEImageFilterTest.cxx
itkTestingComparisonImageFilterTest.cxx
)

CreateTestDriver(ITKImageCompare  "${ITKImageCompare-Test_LIBRARIES}" "${ITKImageCompareTests}")

itk_add_test(NAME itkAbsoluteValueDifferenceImageFilterTest
      COMMAND ITKImageCompareTestDriver itkAbsoluteValueDifferenceImageFilterTest)
itk_add_test(NAME itkSquaredDifferenceImageFilterTest
      COMMAND ITKImageCompareTestDriver itkSquaredDifferenceImageFilterTest)
itk_add_test(NAME itkCheckerBoardImageFilterTest
      COMMAND ITKImageCompareTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkCheckerBoardImageFilterTest.png
              277faa96444775070dcc8f6be2d71eb2
    itkCheckerBoardImageFilterTest ${ITK_TEST_OUTPUT_DIR}/itkCheckerBoardImageFilterTest.png)
itk_add_test(NAME itkConstrainedValueDifferenceImageFilterTest
      COMMAND ITKImageCompareTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkConstrainedValueDifferenceImageFilterTest.png
              87028680dd2bbc12f57b141f7272d295
    itkConstrainedValueDifferenceImageFilterTest ${ITK_TEST_OUTPUT_DIR}/itkConstrainedValueDifferenceImageFilterTest.png)
itk_add_test(NAME itkSimilarityIndexImageFilterTest
      COMMAND ITKImageCompareTestDriver itkSimilarityIndexImageFilterTest)
itk_add_test(NAME itkSTAPLEImageFilterTest
      COMMAND ITKImageCompareTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Algorithms/STAPLEImageFilterTest.mha}
              ${ITK_TEST_OUTPUT_DIR}/STAPLEImageFilterTest.mha
    itkSTAPLEImageFilterTest 2 ${ITK_TEST_OUTPUT_DIR}/STAPLEImageFilterTest.mha 255 0.5 DATA{${ITK_DATA_ROOT}/Input/STAPLE1.png} DATA{${ITK_DATA_ROOT}/Input/STAPLE2.png} DATA{${ITK_DATA_ROOT}/Input/STAPLE3.png} DATA{${ITK_DATA_ROOT}/Input/STAPLE4.png})
itk_add_test(NAME itkTestingComparisonImageFilterTest
      COMMAND ITKImageCompareTestDriver
      --compare DATA{Input//itkTestingComparisonImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkTestingComparisonImageFilterTest.png
    itkTestingComparisonImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cake_easy.png} DATA{${ITK_DATA_ROOT}/Input/cake_hard.png} ${ITK_TEST_OUTPUT_DIR}/itkTestingComparisonImageFilterTest.png 10 1 1647)
