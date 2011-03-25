itk_module_test()
set(ITK-LevelSetsTests
itkThresholdSegmentationLevelSetImageFilterTest.cxx
itkVectorThresholdSegmentationLevelSetImageFilterTest.cxx
itkAnisotropicFourthOrderLevelSetImageFilterTest.cxx
itkReinitializeLevelSetImageFilterTest.cxx
itkLevelSetVelocityNeighborhoodExtractorTest.cxx
itkFastMarchingExtensionImageFilterTest.cxx
itkFastMarchingTest.cxx
itkFastMarchingTest2.cxx
itkFastMarchingUpwindGradientTest.cxx
itkIsotropicFourthOrderLevelSetImageFilterTest.cxx
itkGeodesicActiveContourLevelSetImageFilterTest.cxx
itkGeodesicActiveContourShapePriorLevelSetImageFilterTest_2.cxx
itkParallelSparseFieldLevelSetImageFilterTest.cxx
itkShapeDetectionLevelSetImageFilterTest.cxx
itkNarrowBandThresholdSegmentationLevelSetImageFilterTest.cxx
itkNarrowBandCurvesLevelSetImageFilterTest.cxx
itkCollidingFrontsImageFilterTest.cxx
itkGeodesicActiveContourShapePriorLevelSetImageFilterTest.cxx
itkLevelSetsHeaderTest.cxx
itkLevelSetFunctionTest.cxx
itkExtensionVelocitiesImageFilterTest.cxx
itkCannySegmentationLevelSetImageFilterTest.cxx
itkLevelSetNeighborhoodExtractorTest.cxx
itkShapePriorMAPCostFunctionTest.cxx
itkImplicitManifoldNormalVectorFilterTest.cxx
itkSparseFieldFourthOrderLevelSetImageFilterTest.cxx
itkLaplacianSegmentationLevelSetImageFilterTest.cxx
itkShapePriorSegmentationLevelSetFunctionTest.cxx
itkGeodesicActiveContourLevelSetImageFilterZeroSigmaTest.cxx
itkUnsharpMaskLevelSetImageFilterTest.cxx
itkCurvesLevelSetImageFilterTest.cxx
itkCurvesLevelSetImageFilterZeroSigmaTest.cxx
)

CreateTestDriver(ITK-LevelSets  "${ITK-LevelSets-Test_LIBRARIES}" "${ITK-LevelSetsTests}")

add_test(NAME itkLevelSetsHeaderTest
      COMMAND ITK-LevelSetsTestDriver itkLevelSetsHeaderTest)
add_test(NAME itkThresholdSegmentationLevelSetImageFilterTest
      COMMAND ITK-LevelSetsTestDriver itkThresholdSegmentationLevelSetImageFilterTest)
add_test(NAME itkVectorThresholdSegmentationLevelSetImageFilterTest
      COMMAND ITK-LevelSetsTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/VectorThresholdSegmentationLevelSetImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/VectorThresholdSegmentationLevelSetImageFilterTest.png
    itkVectorThresholdSegmentationLevelSetImageFilterTest ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSliceInitialLevelSet.png ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png ${ITK_TEST_OUTPUT_DIR}/VectorThresholdSegmentationLevelSetImageFilterTest.png 5.0 50)
add_test(NAME itkAnisotropicFourthOrderLevelSetImageFilterTest
      COMMAND ITK-LevelSetsTestDriver itkAnisotropicFourthOrderLevelSetImageFilterTest)
add_test(NAME itkReinitializeLevelSetImageFilterTest
      COMMAND ITK-LevelSetsTestDriver itkReinitializeLevelSetImageFilterTest)
add_test(NAME itkLevelSetVelocityNeighborhoodExtractorTest
      COMMAND ITK-LevelSetsTestDriver itkLevelSetVelocityNeighborhoodExtractorTest)
add_test(NAME itkFastMarchingExtensionImageFilterTest
      COMMAND ITK-LevelSetsTestDriver itkFastMarchingExtensionImageFilterTest)
add_test(NAME itkFastMarchingTest
      COMMAND ITK-LevelSetsTestDriver itkFastMarchingTest)
add_test(NAME itkFastMarchingTest2
      COMMAND ITK-LevelSetsTestDriver itkFastMarchingTest2)
add_test(NAME itkFastMarchingUpwindGradientTest
      COMMAND ITK-LevelSetsTestDriver itkFastMarchingUpwindGradientTest)
add_test(NAME itkIsotropicFourthOrderLevelSetImageFilterTest
      COMMAND ITK-LevelSetsTestDriver itkIsotropicFourthOrderLevelSetImageFilterTest)
add_test(NAME itkGeodesicActiveContourLevelSetImageFilterTest
      COMMAND ITK-LevelSetsTestDriver itkGeodesicActiveContourLevelSetImageFilterTest)
add_test(NAME itkGeodesicActiveContourShapePriorLevelSetImageFilterTest_2
      COMMAND ITK-LevelSetsTestDriver itkGeodesicActiveContourShapePriorLevelSetImageFilterTest_2)
add_test(NAME itkParallelSparseFieldLevelSetImageFilterTest
      COMMAND ITK-LevelSetsTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ParallelSparseFieldLevelSetImageFilterTest.mha
              ${ITK_TEST_OUTPUT_DIR}/ParallelSparseFieldLevelSetImageFilterTest.mha
    itkParallelSparseFieldLevelSetImageFilterTest ${ITK_TEST_OUTPUT_DIR}/ParallelSparseFieldLevelSetImageFilterTest.mha)
add_test(NAME itkShapeDetectionLevelSetImageFilterTest
      COMMAND ITK-LevelSetsTestDriver itkShapeDetectionLevelSetImageFilterTest)
add_test(NAME itkNarrowBandThresholdSegmentationLevelSetImageFilterTest
      COMMAND ITK-LevelSetsTestDriver itkNarrowBandThresholdSegmentationLevelSetImageFilterTest)
add_test(NAME itkNarrowBandCurvesLevelSetImageFilterTest1
      COMMAND ITK-LevelSetsTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/itkNarrowBandCurvesLevelSetImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/itkNarrowBandCurvesLevelSetImageFilterTest.png
    itkNarrowBandCurvesLevelSetImageFilterTest ${ITK_TEST_OUTPUT_DIR}/itkNarrowBandCurvesLevelSetImageFilterTest.png)
add_test(NAME itkCollidingFrontsImageFilterTest
      COMMAND ITK-LevelSetsTestDriver itkCollidingFrontsImageFilterTest)
add_test(NAME itkGeodesicActiveContourShapePriorLevelSetImageFilterTest
      COMMAND ITK-LevelSetsTestDriver itkGeodesicActiveContourShapePriorLevelSetImageFilterTest)
add_test(NAME itkLevelSetFunctionTest
      COMMAND ITK-LevelSetsTestDriver itkLevelSetFunctionTest)
add_test(NAME itkExtensionVelocitiesImageFilterTest
      COMMAND ITK-LevelSetsTestDriver itkExtensionVelocitiesImageFilterTest)
add_test(NAME itkCannySegmentationLevelSetImageFilterTest
      COMMAND ITK-LevelSetsTestDriver itkCannySegmentationLevelSetImageFilterTest)
add_test(NAME itkLevelSetNeighborhoodExtractorTest
      COMMAND ITK-LevelSetsTestDriver itkLevelSetNeighborhoodExtractorTest)
add_test(NAME itkShapePriorMAPCostFunctionTest
      COMMAND ITK-LevelSetsTestDriver itkShapePriorMAPCostFunctionTest)
add_test(NAME itkImplicitManifoldNormalVectorFilterTest
      COMMAND ITK-LevelSetsTestDriver itkImplicitManifoldNormalVectorFilterTest)
add_test(NAME itkSparseFieldFourthOrderLevelSetImageFilterTest
      COMMAND ITK-LevelSetsTestDriver itkSparseFieldFourthOrderLevelSetImageFilterTest)
add_test(NAME itkLaplacianSegmentationLevelSetImageFilterTest
      COMMAND ITK-LevelSetsTestDriver itkLaplacianSegmentationLevelSetImageFilterTest)
add_test(NAME itkShapePriorSegmentationLevelSetFunctionTest
      COMMAND ITK-LevelSetsTestDriver itkShapePriorSegmentationLevelSetFunctionTest)
add_test(NAME itkGeodesicActiveContourLevelSetImageFilterZeroSigmaTest
      COMMAND ITK-LevelSetsTestDriver itkGeodesicActiveContourLevelSetImageFilterZeroSigmaTest)
add_test(NAME itkUnsharpMaskLevelSetImageFilterTest
      COMMAND ITK-LevelSetsTestDriver itkUnsharpMaskLevelSetImageFilterTest)
add_test(NAME itkCurvesLevelSetImageFilterTest
      COMMAND ITK-LevelSetsTestDriver itkCurvesLevelSetImageFilterTest)
add_test(NAME itkCurvesLevelSetImageFilterZeroSigmaTest
      COMMAND ITK-LevelSetsTestDriver itkCurvesLevelSetImageFilterZeroSigmaTest)
