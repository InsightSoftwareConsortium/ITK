itk_module_test()
set(ITKDistanceMapTests
itkDanielssonDistanceMapImageFilterTest.cxx
itkContourMeanDistanceImageFilterTest.cxx
itkDistanceMapHeaderTest.cxx
itkContourDirectedMeanDistanceImageFilterTest.cxx
itkFastChamferDistanceImageFilterTest.cxx
itkHausdorffDistanceImageFilterTest.cxx
itkReflectiveImageRegionIteratorTest.cxx
itkSignedMaurerDistanceMapImageFilterTest.cxx
itkSignedDanielssonDistanceMapImageFilterTest.cxx
itkApproximateSignedDistanceMapImageFilterTest.cxx
itkIsoContourDistanceImageFilterTest.cxx
)

CreateTestDriver(ITKDistanceMap  "${ITKDistanceMap-Test_LIBRARIES}" "${ITKDistanceMapTests}")

itk_add_test(NAME itkDistanceMapHeaderTest
      COMMAND ITKDistanceMapTestDriver itkDistanceMapHeaderTest)
itk_add_test(NAME itkDanielssonDistanceMapImageFilterTest
      COMMAND ITKDistanceMapTestDriver itkDanielssonDistanceMapImageFilterTest)
itk_add_test(NAME itkContourMeanDistanceImageFilterTest
      COMMAND ITKDistanceMapTestDriver itkContourMeanDistanceImageFilterTest)
itk_add_test(NAME itkContourDirectedMeanDistanceImageFilterTest
      COMMAND ITKDistanceMapTestDriver itkContourDirectedMeanDistanceImageFilterTest)
itk_add_test(NAME itkFastChamferDistanceImageFilterTest1
      COMMAND ITKDistanceMapTestDriver itkFastChamferDistanceImageFilterTest 1)
itk_add_test(NAME itkFastChamferDistanceImageFilterTest2
      COMMAND ITKDistanceMapTestDriver itkFastChamferDistanceImageFilterTest 2)
itk_add_test(NAME itkFastChamferDistanceImageFilterTest3
      COMMAND ITKDistanceMapTestDriver itkFastChamferDistanceImageFilterTest 3)
itk_add_test(NAME itkFastChamferDistanceImageFilterTest4
      COMMAND ITKDistanceMapTestDriver itkFastChamferDistanceImageFilterTest 4)
itk_add_test(NAME itkHausdorffDistanceImageFilterTest
      COMMAND ITKDistanceMapTestDriver itkHausdorffDistanceImageFilterTest)
itk_add_test(NAME itkReflectiveImageRegionIteratorTest
      COMMAND ITKDistanceMapTestDriver itkReflectiveImageRegionIteratorTest)
itk_add_test(NAME itkSignedMaurerDistanceMapImageFilterTest1
      COMMAND ITKDistanceMapTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkSignedMaurerDistanceMapImageFilterTest1.mhd
              ${ITK_TEST_OUTPUT_DIR}/itkSignedMaurerDistanceMapImageFilterTest1.mhd
    itkSignedMaurerDistanceMapImageFilterTest ${ITK_DATA_ROOT}/Input/SquareBinary201.png ${ITK_TEST_OUTPUT_DIR}/itkSignedMaurerDistanceMapImageFilterTest1.mhd)
itk_add_test(NAME itkSignedMaurerDistanceMapImageFilterTest2
      COMMAND ITKDistanceMapTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkSignedMaurerDistanceMapImageFilterTest2.mhd
              ${ITK_TEST_OUTPUT_DIR}/itkSignedMaurerDistanceMapImageFilterTest2.mhd
    itkSignedMaurerDistanceMapImageFilterTest ${ITK_DATA_ROOT}/Input/BrainSliceBinary.png ${ITK_TEST_OUTPUT_DIR}/itkSignedMaurerDistanceMapImageFilterTest2.mhd)
itk_add_test(NAME itkSignedMaurerDistanceMapImageFilterTest3
      COMMAND ITKDistanceMapTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkSignedMaurerDistanceMapImageFilterTest3.mhd
              ${ITK_TEST_OUTPUT_DIR}/itkSignedMaurerDistanceMapImageFilterTest3.mhd
    itkSignedMaurerDistanceMapImageFilterTest ${ITK_DATA_ROOT}/Input/LungSliceBinary.png ${ITK_TEST_OUTPUT_DIR}/itkSignedMaurerDistanceMapImageFilterTest3.mhd)
itk_add_test(NAME itkSignedDanielssonDistanceMapImageFilterTest
      COMMAND ITKDistanceMapTestDriver itkSignedDanielssonDistanceMapImageFilterTest)
itk_add_test(NAME itkApproximateSignedDistanceMapImageFilterTest
      COMMAND ITKDistanceMapTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkApproximateSignedDistanceMapImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/itkApproximateSignedDistanceMapImageFilterTest.png
    itkApproximateSignedDistanceMapImageFilterTest ${ITK_TEST_OUTPUT_DIR}/itkApproximateSignedDistanceMapImageFilterTest.png)
itk_add_test(NAME itkIsoContourDistanceImageFilterTest
      COMMAND ITKDistanceMapTestDriver itkIsoContourDistanceImageFilterTest)
