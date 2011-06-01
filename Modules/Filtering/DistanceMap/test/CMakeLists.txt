itk_module_test()
set(ITK-DistanceMapTests
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

CreateTestDriver(ITK-DistanceMap  "${ITK-DistanceMap-Test_LIBRARIES}" "${ITK-DistanceMapTests}")

itk_add_test(NAME itkDistanceMapHeaderTest
      COMMAND ITK-DistanceMapTestDriver itkDistanceMapHeaderTest)
itk_add_test(NAME itkDanielssonDistanceMapImageFilterTest
      COMMAND ITK-DistanceMapTestDriver itkDanielssonDistanceMapImageFilterTest)
itk_add_test(NAME itkContourMeanDistanceImageFilterTest
      COMMAND ITK-DistanceMapTestDriver itkContourMeanDistanceImageFilterTest)
itk_add_test(NAME itkContourDirectedMeanDistanceImageFilterTest
      COMMAND ITK-DistanceMapTestDriver itkContourDirectedMeanDistanceImageFilterTest)
itk_add_test(NAME itkFastChamferDistanceImageFilterTest
      COMMAND ITK-DistanceMapTestDriver itkFastChamferDistanceImageFilterTest)
itk_add_test(NAME itkHausdorffDistanceImageFilterTest
      COMMAND ITK-DistanceMapTestDriver itkHausdorffDistanceImageFilterTest)
itk_add_test(NAME itkReflectiveImageRegionIteratorTest
      COMMAND ITK-DistanceMapTestDriver itkReflectiveImageRegionIteratorTest)
itk_add_test(NAME itkSignedMaurerDistanceMapImageFilterTest1
      COMMAND ITK-DistanceMapTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkSignedMaurerDistanceMapImageFilterTest1.mhd
              ${ITK_TEST_OUTPUT_DIR}/itkSignedMaurerDistanceMapImageFilterTest1.mhd
    itkSignedMaurerDistanceMapImageFilterTest ${ITK_DATA_ROOT}/Input/SquareBinary201.png ${ITK_TEST_OUTPUT_DIR}/itkSignedMaurerDistanceMapImageFilterTest1.mhd)
itk_add_test(NAME itkSignedMaurerDistanceMapImageFilterTest2
      COMMAND ITK-DistanceMapTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkSignedMaurerDistanceMapImageFilterTest2.mhd
              ${ITK_TEST_OUTPUT_DIR}/itkSignedMaurerDistanceMapImageFilterTest2.mhd
    itkSignedMaurerDistanceMapImageFilterTest ${ITK_DATA_ROOT}/Input/BrainSliceBinary.png ${ITK_TEST_OUTPUT_DIR}/itkSignedMaurerDistanceMapImageFilterTest2.mhd)
itk_add_test(NAME itkSignedMaurerDistanceMapImageFilterTest3
      COMMAND ITK-DistanceMapTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkSignedMaurerDistanceMapImageFilterTest3.mhd
              ${ITK_TEST_OUTPUT_DIR}/itkSignedMaurerDistanceMapImageFilterTest3.mhd
    itkSignedMaurerDistanceMapImageFilterTest ${ITK_DATA_ROOT}/Input/LungSliceBinary.png ${ITK_TEST_OUTPUT_DIR}/itkSignedMaurerDistanceMapImageFilterTest3.mhd)
itk_add_test(NAME itkSignedDanielssonDistanceMapImageFilterTest
      COMMAND ITK-DistanceMapTestDriver itkSignedDanielssonDistanceMapImageFilterTest)
itk_add_test(NAME itkApproximateSignedDistanceMapImageFilterTest
      COMMAND ITK-DistanceMapTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkApproximateSignedDistanceMapImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/itkApproximateSignedDistanceMapImageFilterTest.png
    itkApproximateSignedDistanceMapImageFilterTest ${ITK_TEST_OUTPUT_DIR}/itkApproximateSignedDistanceMapImageFilterTest.png)
itk_add_test(NAME itkIsoContourDistanceImageFilterTest
      COMMAND ITK-DistanceMapTestDriver itkIsoContourDistanceImageFilterTest)
