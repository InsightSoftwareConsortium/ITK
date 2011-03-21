itk_module_test()
set(ITK-IntegratedTestTests
itkAccumulateImageFilterTest.cxx
itkAdaptiveHistogramEqualizationImageFilterTest.cxx
itkAdaptorComparisonTest.cxx
itkBasicArchitectureTest.cxx
itkBayesianClassifierImageFilterTest.cxx
itkBinaryProjectionImageFilterTest.cxx
itkBinaryThresholdImageFilterTest2.cxx
itkBinaryThresholdProjectionImageFilterTest.cxx
itkBinaryThresholdSpatialFunctionTest.cxx
itkBloxCoreAtomTest.cxx
itkBoundaryConditionTest.cxx
itkBSplineInterpolateImageFunctionTest.cxx
itkBSplineResampleImageFilterTest.cxx
itkBSplineResampleImageFunctionTest.cxx
itkByteSwapTest.cxx
itkChiSquareDistributionTest.cxx
itkConfidenceConnectedImageFilterTest.cxx
itkConnectedComponentImageFilterTooManyObjectsTest.cxx
itkConnectedThresholdImageFilterTest.cxx
itkConstantPadImageTest.cxx
itkConvertBufferTest.cxx
itkConvertBufferTest2.cxx
itkCovarianceSampleFilterTest.cxx
itkCovarianceSampleFilterTest2.cxx
itkCovarianceSampleFilterTest3.cxx
itkCovariantVectorGeometryTest.cxx
itkCurvatureFlowTest.cxx
itkCurvesLevelSetImageFilterTest.cxx
itkCurvesLevelSetImageFilterZeroSigmaTest.cxx
itkCyclicReferences.cxx
itkDataTypeTest.cxx
itkDecisionRuleTest.cxx
itkDecoratorTest.cxx
itkDenseFrequencyContainer2Test.cxx
itkDicomImageIODirection2DTest.cxx
itkDifferenceOfGaussiansGradientTest.cxx
itkDistanceMetricTest.cxx
itkDistanceMetricTest2.cxx
itkDistanceToCentroidMembershipFunctionTest.cxx
itkDoubleThresholdImageFilterTest.cxx
itkDynamicMeshTest.cxx
itkDynamicQuadEdgeMeshTest.cxx
itkEuclideanDistanceMetricTest.cxx
itkEuclideanSquareDistanceMetricTest.cxx
itkExpectationMaximizationMixtureModelEstimatorTest.cxx
itkExpImageFilterAndAdaptorTest.cxx
itkExtractImage3Dto2DTest.cxx
itkExtractImageTest.cxx
itkExtractMeshConnectedRegionsTest.cxx
itkExtractOrthogonalSwath2DImageFilterTest.cxx
itkFastMarchingTest.cxx
itkFastMarchingTest2.cxx
itkFastMarchingUpwindGradientTest.cxx
#itkFileFreeImageIO.cxx
#itkFileFreeImageIOFactory.cxx
itkFilterDispatchTest.cxx
itkFilterImageAddTest.cxx
itkFloodFilledSpatialFunctionTest.cxx
itkFloodFillIteratorTest.cxx
itkGaussianDistributionTest.cxx
itkGaussianMembershipFunctionTest.cxx
itkGaussianMixtureModelComponentTest.cxx
itkGEImageIOTest.cxx
itkGeodesicActiveContourLevelSetImageFilterZeroSigmaTest.cxx
itkGetAverageSliceImageFilterTest.cxx
itkGibbsTest.cxx
itkGiplImageIOTest.cxx
itkGradientAnisotropicDiffusionImageFilterTest2.cxx
itkGradientMagnitudeRecursiveGaussianFilterTest.cxx
itkGradientRecursiveGaussianFilterTest.cxx
itkGradientRecursiveGaussianFilterTest2.cxx
itkGrayscaleGeodesicErodeDilateImageFilterTest.cxx
itkHashTableTest.cxx
itkHConvexConcaveImageFilterTest.cxx
itkHessianRecursiveGaussianFilterScaleSpaceTest.cxx
itkHessianRecursiveGaussianFilterTest.cxx

itkHMaximaMinimaImageFilterTest.cxx
itkHoughTransform2DCirclesImageTest.cxx
itkHoughTransform2DLinesImageTest.cxx
itkImageAdaptorPipeLineTest.cxx
itkImageClassifierFilterTest.cxx
itkImageComputeOffsetAndIndexTest.cxx
itkImageDuplicatorTest.cxx
itkImageIteratorsForwardBackwardTest.cxx
itkImageLinearIteratorTest.cxx
itkImageMomentsTest.cxx
itkImageRandomIteratorTest.cxx
itkImageRandomIteratorTest2.cxx
itkImageRandomNonRepeatingIteratorWithIndexTest.cxx
itkImageRandomNonRepeatingIteratorWithIndexTest2.cxx
itkImageSeriesReaderDimensionsTest.cxx
itkImageSeriesWriterTest.cxx
itkImageSliceIteratorTest.cxx
itkImageToHistogramFilterTest.cxx
itkImageToHistogramFilterTest2.cxx
itkImageToHistogramFilterTest3.cxx
itkImageToListSampleAdaptorTest.cxx
itkImageToListSampleAdaptorTest2.cxx
itkImageToListSampleFilterTest.cxx
itkImageToListSampleFilterTest2.cxx
itkImageToListSampleFilterTest3.cxx
itkImportContainerTest.cxx
itkImportImageTest.cxx
itkInterpolateTest.cxx
itkIOPluginTest.cxx
itkIsoContourDistanceImageFilterTest.cxx
itkIsolatedWatershedImageFilterTest.cxx
itkIteratorTests.cxx
itkJoinSeriesImageFilterStreamingTest.cxx
itkJointDomainImageToListSampleAdaptorTest.cxx
itkKdTreeBasedKmeansEstimatorTest.cxx
itkKdTreeGeneratorTest.cxx
itkKdTreeTest1.cxx
itkKdTreeTest2.cxx
itkKdTreeTestSamplePoints.cxx
itkKmeansModelEstimatorTest.cxx
itkLaplacianRecursiveGaussianImageFilterTest.cxx
itkLargeImageWriteConvertReadTest.cxx
itkLargeImageWriteReadTest.cxx
itkListSampleTest.cxx
itkMahalanobisDistanceMetricTest.cxx
itkManhattanDistanceMetricTest.cxx
itkMaskConnectedComponentImageFilterTest.cxx
itkMaskNeighborhoodOperatorImageFilterTest.cxx
itkMathCastWithRangeCheckTest.cxx
itkMathematicalMorphologyImageFilterTest.cxx
itkMathRoundProfileTest1.cxx
itkMathRoundTest.cxx
itkMathRoundTest2.cxx
itkMatrixImageWriteReadTest.cxx
itkMaximumDecisionRuleTest.cxx
itkMaximumRatioDecisionRuleTest.cxx
itkMeanReciprocalSquareDifferenceImageMetricTest.cxx
itkMeanSampleFilterTest.cxx
itkMeanSampleFilterTest2.cxx
itkMeanSampleFilterTest3.cxx
itkMeanSquaresImageMetricTest.cxx
itkMeasurementVectorTraitsTest.cxx
itkMedialNodeCorrespondencesTest.cxx
itkMembershipFunctionBaseTest.cxx
itkMembershipFunctionBaseTest2.cxx
itkMembershipSampleTest1.cxx
itkMembershipSampleTest2.cxx
itkMembershipSampleTest3.cxx
itkMembershipSampleTest4.cxx
itkMemoryLeakTest.cxx
itkMeshFstreamTest.cxx
itkMeshSourceGraftOutputTest.cxx
itkMeshSpatialObjectIOTest.cxx
itkMinimumDecisionRuleTest.cxx
itkMirrorPadImageTest.cxx
itkMixtureModelComponentBaseTest.cxx
itkModifiedTimeTest.cxx
itkModulusImageFilterTest.cxx
itkMRIBiasFieldCorrectionFilterTest.cxx
itkMutualInformationMetricTest.cxx
itkNeighborhoodSamplerTest1.cxx
itkNewTest.cxx
itkNoiseImageFilterTest.cxx
itkNormalizedCorrelationImageFilterTest.cxx
itkNormalizedCorrelationImageMetricTest.cxx
itkNormalVariateGeneratorTest1.cxx
itkNumericsPrintTest.cxx
itkNumericsTest.cxx
itkOptimizersHierarchyTest.cxx
itkOrientedImage2DTest.cxx
itkOrientedImage3DTest.cxx
itkOrientedImageAdaptorTest.cxx
itkOrientedImageProfileTest1.cxx
itkOrientedImageProfileTest2.cxx
itkOrientedImageProfileTest3.cxx
itkOrientImageFilterTest.cxx
itkOrientImageFilterTest2.cxx
itkOrthogonalSwath2DPathFilterTest.cxx
itkPixelAccessTest.cxx
itkPointGeometryTest.cxx
itkPointSetToImageRegistrationTest_1.cxx
itkPointSetToListSampleAdaptorTest.cxx
itkPointSetToPointSetRegistrationTest.cxx
itkPolylineMask2DImageFilterTest.cxx
itkPolylineMaskImageFilterTest.cxx
itkPriorityQueueTest.cxx
itkProbabilityDistributionTest.cxx
itkProjectionImageFilterTest.cxx
itkPromoteDimensionImageTest.cxx
itkPushPopTileImageFilterTest.cxx
itkRandomVariateGeneratorBaseTest.cxx
itkReadWriteImageWithDictionaryTest.cxx
itkReadWriteSpatialObjectTest.cxx
itkRecursiveGaussianImageFiltersOnTensorsTest.cxx
itkRecursiveGaussianImageFiltersOnVectorImageTest.cxx
itkRecursiveGaussianImageFiltersTest.cxx
itkRecursiveGaussianScaleSpaceTest1.cxx
itkRegionGrow2DTest.cxx
itkReleaseDataFilterTest.cxx
itkRemoveBoundaryObjectsTest.cxx
itkRemoveBoundaryObjectsTest2.cxx
itkResampleImageTest.cxx
itkResampleImageTest2.cxx
itkResamplePhasedArray3DSpecialCoordinatesImageTest.cxx
itkRGBInterpolateImageFunctionTest.cxx
itkSampleClassifierFilterTest1.cxx
itkSampleClassifierFilterTest2.cxx
itkSampleClassifierFilterTest3.cxx
itkSampleClassifierFilterTest4.cxx
itkSampleClassifierFilterTest5.cxx
itkSampleClassifierFilterTest6.cxx
itkSampleClassifierFilterTest7.cxx
itkSampleTest.cxx
itkSampleTest2.cxx
itkSampleTest3.cxx
itkSampleTest4.cxx
itkSampleToHistogramFilterTest.cxx
itkSampleToHistogramFilterTest2.cxx
itkSampleToHistogramFilterTest3.cxx
itkSampleToHistogramFilterTest4.cxx
itkSampleToHistogramFilterTest5.cxx
itkSampleToHistogramFilterTest6.cxx
itkSampleToHistogramFilterTest7.cxx
itkSampleToSubsampleFilterTest1.cxx
itkScalarImageKmeansImageFilter3DTest.cxx
itkScalarImageToCooccurrenceListSampleFilterTest.cxx
itkScalarImageToCooccurrenceMatrixFilterTest.cxx
itkScalarImageToCooccurrenceMatrixFilterTest2.cxx
itkScalarImageToTextureFeaturesFilterTest.cxx
itkShrinkImagePreserveObjectPhysicalLocations.cxx
itkShrinkImageStreamingTest.cxx
itkShrinkImageTest.cxx
itkSimpleFilterWatcherTest.cxx
itkSimplexMeshWithFloatCoordRepTest.cxx
itkSparseFrequencyContainer2Test.cxx
itkSparseImageTest.cxx
itkSpatialObjectToImageRegistrationTest.cxx
itkStandardDeviationPerComponentSampleFilterTest.cxx
itkStatisticsAlgorithmTest.cxx
itkStatisticsAlgorithmTest2.cxx
itkStatisticsTypesTest.cxx
itkStatisticsPrintTest.cxx
itkSTLThreadTest.cxx
itkStreamingImageFilterTest.cxx
itkStreamingImageFilterTest2.cxx
itkStreamingImageFilterTest3.cxx
itkSubsampleTest.cxx
itkSubsampleTest2.cxx
itkSubsampleTest3.cxx
itkSupervisedImageClassifierTest.cxx
itkSymmetricSecondRankTensorImageReadTest.cxx
itkSymmetricSecondRankTensorImageWriteReadTest.cxx
itkTDistributionTest.cxx
itkThreadDefsTest.cxx
itkTimeProbesTest.cxx
itkTopHatImageFilterTest.cxx
itkTriangleCellTest.cxx
itkTriangleMeshToSimplexMeshFilter2Test.cxx
itkUnsharpMaskLevelSetImageFilterTest.cxx
itkVectorConfidenceConnectedImageFilterTest.cxx
itkVectorGeometryTest.cxx
itkVectorGradientMagnitudeImageFilterTest3.cxx
itkVectorImageReadWriteTest.cxx
itkVectorImageTest.cxx
itkVNLRoundProfileTest1.cxx
itkVoronoiPartitioningImageFilterTest.cxx
itkVTKImageIO2Test2.cxx
itkVTKImageIOTest2.cxx
itkVTKPolyDataIOQuadEdgeMeshTest.cxx
itkVTKPolyDataReaderQuadEdgeMeshTest.cxx
itkWarpImageFilterTest.cxx
itkWarpVectorImageFilterTest.cxx
itkWatershedImageFilterTest.cxx
itkWeightedCentroidKdTreeGeneratorTest1.cxx
itkWeightedCentroidKdTreeGeneratorTest8.cxx
itkWeightedCentroidKdTreeGeneratorTest9.cxx
itkWeightedCovarianceSampleFilterTest.cxx
itkWeightedCovarianceSampleFilterTest2.cxx
itkWeightedMeanSampleFilterTest.cxx
itkWrapPadImageTest.cxx
itkZeroFluxBoundaryConditionTest.cxx
NNetClassifierTest1.cxx
NNetClassifierTest2.cxx
NNetClassifierTest3.cxx
NNetClassifierTest4.cxx
QPropXORTest1.cxx
RBFTest1.cxx
XORTest1.cxx
XORTest2.cxx
itkCommonPrintTest.cxx
itkCellInterfaceTest.cxx
itkIOPrintTest.cxx
itkBasicFiltersPrintTest.cxx
itkNeuralNetworksPrintTest.cxx
itkSpatialObjectPrintTest.cxx
)

CreateTestDriver_SupportBuildInIOFactories(ITK-IntegratedTest  "${ITK-IntegratedTest-Test_LIBRARIES}" "${ITK-IntegratedTestTests}")

# temporary solution for all print tests. Each module should have its own print test later.
add_executable(itkAlgorithmsPrintTest  itkAlgorithmsPrintTest.cxx)
target_link_libraries(itkAlgorithmsPrintTest ${ITK-IntegratedTest-Test_LIBRARIES})
add_test(NAME itkAlgorithmsPrintTest COMMAND itkAlgorithmsPrintTest)

add_executable(itkAlgorithmsPrintTest2  itkAlgorithmsPrintTest2.cxx)
target_link_libraries(itkAlgorithmsPrintTest2 ${ITK-IntegratedTest-Test_LIBRARIES})
add_test(NAME itkAlgorithmsPrintTest2 COMMAND itkAlgorithmsPrintTest)

add_executable(itkAlgorithmsPrintTest3  itkAlgorithmsPrintTest3.cxx)
target_link_libraries(itkAlgorithmsPrintTest3 ${ITK-IntegratedTest-Test_LIBRARIES})
add_test(NAME itkAlgorithmsPrintTest3 COMMAND itkAlgorithmsPrintTest)

add_executable(itkAlgorithmsPrintTest4  itkAlgorithmsPrintTest4.cxx)
target_link_libraries(itkAlgorithmsPrintTest4 ${ITK-IntegratedTest-Test_LIBRARIES})
add_test(NAME itkAlgorithmsPrintTest4 COMMAND itkAlgorithmsPrintTest)

#-------------------------------------------------------------------------
add_test(NAME itkStatisticsPrintTest
      COMMAND ITK-IntegratedTestTestDriver itkStatisticsPrintTest)
add_test(NAME itkSpatialObjectPrintTest
      COMMAND ITK-IntegratedTestTestDriver itkSpatialObjectPrintTest)
add_test(NAME itkIOPrintTest
      COMMAND ITK-IntegratedTestTestDriver itkIOPrintTest)
add_test(NAME itkCommonPrintTest
      COMMAND ITK-IntegratedTestTestDriver itkCommonPrintTest)
add_test(NAME itkCellInterfaceTest
      COMMAND ITK-IntegratedTestTestDriver itkCellInterfaceTest)
add_test(NAME itkBasicFiltersPrintTest
      COMMAND ITK-IntegratedTestTestDriver itkBasicFiltersPrintTest)
add_test(NAME itkAccumulateImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/AccumulateImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/AccumulateImageFilterTest.png
    itkAccumulateImageFilterTest ${ITK_DATA_ROOT}/Input/DicomSeries ${ITK_TEST_OUTPUT_DIR}/AccumulateImageFilterTest.png)
add_test(NAME itkAdaptiveHistogramEqualizationImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/AdaptiveHistogramEqualizationImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/AdaptiveHistogramEqualizationImageFilterTest.png
    itkAdaptiveHistogramEqualizationImageFilterTest ${ITK_DATA_ROOT}/Input/sf4.png ${ITK_TEST_OUTPUT_DIR}/AdaptiveHistogramEqualizationImageFilterTest.png 10 0.5 0.5)
add_test(NAME itkAdaptiveHistogramEqualizationImageFilterTest2
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/AdaptiveHistogramEqualizationImageFilterTest2.png
              ${ITK_TEST_OUTPUT_DIR}/AdaptiveHistogramEqualizationImageFilterTest2.png
    itkAdaptiveHistogramEqualizationImageFilterTest ${ITK_DATA_ROOT}/Input/sf4.png ${ITK_TEST_OUTPUT_DIR}/AdaptiveHistogramEqualizationImageFilterTest2.png 10 1.0 0.25)
add_test(NAME itkAdaptorComparisonTest
      COMMAND ITK-IntegratedTestTestDriver itkAdaptorComparisonTest)
add_test(NAME itkBasicArchitectureTest
      COMMAND ITK-IntegratedTestTestDriver itkBasicArchitectureTest)
add_test(NAME itkBayesianClassifierImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/itkBayesianClassifierImageFilterTestLabelMap.png
              ${ITK_TEST_OUTPUT_DIR}/itkBayesianClassifierImageFilterTestLabelMap.png
    itkBayesianClassifierImageFilterTest ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png ${ITK_TEST_OUTPUT_DIR}/itkBayesianClassifierImageFilterTestLabelMap.png 4 2)
add_test(NAME itkBinaryProjectionImageFilterTest1
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HeadMRVolumeBinaryProjection100.tif
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeBinaryProjection100.tif
    itkBinaryProjectionImageFilterTest ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeBinaryProjection100.tif 100 0)
add_test(NAME itkBinaryProjectionImageFilterTest2
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HeadMRVolumeBinaryProjection200.tif
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeBinaryProjection200.tif
    itkBinaryProjectionImageFilterTest ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeBinaryProjection200.tif 200 0)
add_test(NAME itkBinaryThresholdImageFilterTest2
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/BinaryThresholdImageFilterTest2.png
              ${ITK_TEST_OUTPUT_DIR}/BinaryThresholdImageFilterTest2.png
    itkBinaryThresholdImageFilterTest2 ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_DATA_ROOT}/Input/DicomSeries/Image0076.dcm ${ITK_TEST_OUTPUT_DIR}/BinaryThresholdImageFilterTest2.png)
add_test(NAME itkBinaryThresholdProjectionImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HeadMRVolumeBinaryThresholdProjection.png
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeBinaryThresholdProjection.png
    itkBinaryThresholdProjectionImageFilterTest ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeBinaryThresholdProjection.png 100 255 0)
add_test(NAME itkBinaryThresholdSpatialFunctionTest
      COMMAND ITK-IntegratedTestTestDriver itkBinaryThresholdSpatialFunctionTest)
add_test(NAME itkBloxCoreAtomTest
      COMMAND ITK-IntegratedTestTestDriver itkBloxCoreAtomTest)
add_test(NAME itkBoundaryConditionTest
      COMMAND ITK-IntegratedTestTestDriver itkBoundaryConditionTest)
add_test(NAME itkBSplineInterpolateImageFunctionTest
      COMMAND ITK-IntegratedTestTestDriver itkBSplineInterpolateImageFunctionTest)
add_test(NAME itkBSplineResampleImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkBSplineResampleImageFilterTest)
add_test(NAME itkBSplineResampleImageFunctionTest
      COMMAND ITK-IntegratedTestTestDriver itkBSplineResampleImageFunctionTest)
add_test(NAME itkByteSwapTest
      COMMAND ITK-IntegratedTestTestDriver itkByteSwapTest)
add_test(NAME itkChiSquareDistributionTest
      COMMAND ITK-IntegratedTestTestDriver itkChiSquareDistributionTest)
add_test(NAME itkConfidenceConnectedImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ConfidenceConnectedImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/ConfidenceConnectedImageFilterTest.png
    itkConfidenceConnectedImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/ConfidenceConnectedImageFilterTest.png 165 165)
add_test(NAME itkConnectedComponentImageFilterTooManyObjectsTest
      COMMAND ITK-IntegratedTestTestDriver itkConnectedComponentImageFilterTooManyObjectsTest)
add_test(NAME itkConnectedThresholdImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ConnectedThresholdImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/ConnectedThresholdImageFilterTest.png
    itkConnectedThresholdImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/ConnectedThresholdImageFilterTest.png 165 165 0 150)
add_test(NAME itkConnectedThresholdImageFilterTest2
      COMMAND ITK-IntegratedTestTestDriver
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ConnectedThresholdImageFilterTest2.png
            ${ITK_TEST_OUTPUT_DIR}/ConnectedThresholdImageFilterTest2.png
   itkConnectedThresholdImageFilterTest ${ITK_DATA_ROOT}/Input/8ConnectedImage.bmp
            ${ITK_TEST_OUTPUT_DIR}/ConnectedThresholdImageFilterTest2.png
            29 47 200 255 1)
add_test(NAME itkConstantPadImageTest
      COMMAND ITK-IntegratedTestTestDriver itkConstantPadImageTest)
add_test(NAME itkConvertBufferTest
      COMMAND ITK-IntegratedTestTestDriver itkConvertBufferTest)
add_test(NAME itkConvertBufferTest2
      COMMAND ITK-IntegratedTestTestDriver itkConvertBufferTest2)
add_test(NAME itkCovarianceSampleFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkCovarianceSampleFilterTest)
add_test(NAME itkCovarianceSampleFilterTest2
      COMMAND ITK-IntegratedTestTestDriver itkCovarianceSampleFilterTest2)
add_test(NAME itkCovarianceSampleFilterTest3
      COMMAND ITK-IntegratedTestTestDriver itkCovarianceSampleFilterTest3)
add_test(NAME itkCovariantVectorGeometryTest
      COMMAND ITK-IntegratedTestTestDriver itkCovariantVectorGeometryTest)
add_test(NAME itkCurvatureFlowTest
      COMMAND ITK-IntegratedTestTestDriver itkCurvatureFlowTest
              ${ITK_TEST_OUTPUT_DIR}/itkCurvatureFlowTest.vtk)
add_test(NAME itkCurvesLevelSetImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkCurvesLevelSetImageFilterTest)
add_test(NAME itkCurvesLevelSetImageFilterZeroSigmaTest
      COMMAND ITK-IntegratedTestTestDriver itkCurvesLevelSetImageFilterZeroSigmaTest)
add_test(NAME itkCyclicReferences
      COMMAND ITK-IntegratedTestTestDriver itkCyclicReferences)
add_test(NAME itkDataTypeTest
      COMMAND ITK-IntegratedTestTestDriver itkDataTypeTest)
add_test(NAME itkDecisionRuleTest
      COMMAND ITK-IntegratedTestTestDriver itkDecisionRuleTest)
add_test(NAME itkDecoratorTest
      COMMAND ITK-IntegratedTestTestDriver itkDecoratorTest)
add_test(NAME itkDenseFrequencyContainer2Test
      COMMAND ITK-IntegratedTestTestDriver itkDenseFrequencyContainer2Test)
add_test(NAME itkDicomImageIODirection2DTest
      COMMAND ITK-IntegratedTestTestDriver itkDicomImageIODirection2DTest
              ${ITK_TEST_OUTPUT_DIR}/itkDicomImage3DIODirection2DTest.dcm ${ITK_TEST_OUTPUT_DIR}/itkDicomImage3DIODirection2DTest.mhd ${ITK_TEST_OUTPUT_DIR}/itkDicomImage2DIODirection2DTest.mhd)
add_test(NAME itkDifferenceOfGaussiansGradientTest
      COMMAND ITK-IntegratedTestTestDriver itkDifferenceOfGaussiansGradientTest)
add_test(NAME itkDistanceMetricTest
      COMMAND ITK-IntegratedTestTestDriver itkDistanceMetricTest)
add_test(NAME itkDistanceMetricTest2
      COMMAND ITK-IntegratedTestTestDriver itkDistanceMetricTest2)
add_test(NAME itkDistanceToCentroidMembershipFunctionTest
      COMMAND ITK-IntegratedTestTestDriver itkDistanceToCentroidMembershipFunctionTest)
add_test(NAME itkDynamicMeshTest
      COMMAND ITK-IntegratedTestTestDriver itkDynamicMeshTest)
add_test(NAME itkDynamicQuadEdgeMeshTest
      COMMAND ITK-IntegratedTestTestDriver itkDynamicQuadEdgeMeshTest)
add_test(NAME itkEuclideanDistanceMetricTest
      COMMAND ITK-IntegratedTestTestDriver itkEuclideanDistanceMetricTest)
add_test(NAME itkEuclideanSquareDistanceMetricTest
      COMMAND ITK-IntegratedTestTestDriver itkEuclideanSquareDistanceMetricTest)
add_test(NAME itkExpectationMaximizationMixtureModelEstimatorTest
      COMMAND ITK-IntegratedTestTestDriver itkExpectationMaximizationMixtureModelEstimatorTest
              ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat)
add_test(NAME itkExpImageFilterAndAdaptorTest
      COMMAND ITK-IntegratedTestTestDriver itkExpImageFilterAndAdaptorTest)
add_test(NAME itkExtractImage3Dto2DTest
      COMMAND ITK-IntegratedTestTestDriver itkExtractImage3Dto2DTest)
add_test(NAME itkExtractImageTest
      COMMAND ITK-IntegratedTestTestDriver itkExtractImageTest)
add_test(NAME itkExtractMeshConnectedRegionsTest
      COMMAND ITK-IntegratedTestTestDriver itkExtractMeshConnectedRegionsTest)
add_test(NAME itkExtractOrthogonalSwath2DImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ExtractOrthogonalSwath2DImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/ExtractOrthogonalSwath2DImageFilterTest.png
    itkExtractOrthogonalSwath2DImageFilterTest ${ITK_TEST_OUTPUT_DIR}/ExtractOrthogonalSwath2DImageFilterTest.png)
add_test(NAME itkFastMarchingTest
      COMMAND ITK-IntegratedTestTestDriver itkFastMarchingTest)
add_test(NAME itkFastMarchingTest2
      COMMAND ITK-IntegratedTestTestDriver itkFastMarchingTest2)
add_test(NAME itkFastMarchingUpwindGradientTest
      COMMAND ITK-IntegratedTestTestDriver itkFastMarchingUpwindGradientTest)

add_test(NAME itkFilterDispatchTest
      COMMAND ITK-IntegratedTestTestDriver itkFilterDispatchTest)
add_test(NAME itkFloodFilledSpatialFunctionTest
      COMMAND ITK-IntegratedTestTestDriver itkFloodFilledSpatialFunctionTest)
add_test(NAME itkFloodFillIteratorTest
      COMMAND ITK-IntegratedTestTestDriver itkFloodFillIteratorTest)
add_test(NAME itkGaussianDistributionTest
      COMMAND ITK-IntegratedTestTestDriver itkGaussianDistributionTest)
add_test(NAME itkGaussianMembershipFunctionTest
      COMMAND ITK-IntegratedTestTestDriver itkGaussianMembershipFunctionTest)
add_test(NAME itkGaussianMixtureModelComponentTest
      COMMAND ITK-IntegratedTestTestDriver itkGaussianMixtureModelComponentTest
              ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat)
add_test(NAME itkGE4
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/19771.002.001.mha
              ${ITK_TEST_OUTPUT_DIR}/19771.002.001.mha
    itkGEImageIOTest ${ITK_TEST_OUTPUT_DIR} true GE4 ${ITK_DATA_ROOT}/Input/test_ge4/19771.002.001 ${ITK_TEST_OUTPUT_DIR}/19771.002.001.mha)
add_test(NAME itkGE5
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/113766.003.001.mha
              ${ITK_TEST_OUTPUT_DIR}/113766.003.001.mha
    itkGEImageIOTest ${ITK_TEST_OUTPUT_DIR} true GE5 ${ITK_DATA_ROOT}/Input/test_ge5/113766.003.001 ${ITK_TEST_OUTPUT_DIR}/113766.003.001.mha)
add_test(NAME itkGEAdw
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/I.001.mha
              ${ITK_TEST_OUTPUT_DIR}/I.001.mha
    itkGEImageIOTest ${ITK_TEST_OUTPUT_DIR} true GEAdw ${ITK_DATA_ROOT}/Input/test_geadw/I.001 ${ITK_TEST_OUTPUT_DIR}/I.001.mha)
add_test(NAME itkSiemens
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/3868-2-100.mha
              ${ITK_TEST_OUTPUT_DIR}/3868-2-100.mha
    itkGEImageIOTest ${ITK_TEST_OUTPUT_DIR} true Siemens ${ITK_DATA_ROOT}/Input/test_siemens/3868-2-100.ima ${ITK_TEST_OUTPUT_DIR}/3868-2-100.mha)
add_test(NAME itkGE4Fail
      COMMAND ITK-IntegratedTestTestDriver itkGEImageIOTest
              ${ITK_TEST_OUTPUT_DIR} false GE4 ${ITK_DATA_ROOT}/Input/test_ge5/113766.003.001)
add_test(NAME itkGE5Fail
      COMMAND ITK-IntegratedTestTestDriver itkGEImageIOTest
              ${ITK_TEST_OUTPUT_DIR} false GE5 ${ITK_DATA_ROOT}/Input/test_ge4/19771.002.001)
add_test(NAME itkGEAdwFail
      COMMAND ITK-IntegratedTestTestDriver itkGEImageIOTest
              ${ITK_TEST_OUTPUT_DIR} false GEAdw ${ITK_DATA_ROOT}/Input/test_siemens/3868-2-100.ima)
add_test(NAME itkSiemensFail
      COMMAND ITK-IntegratedTestTestDriver itkGEImageIOTest
              ${ITK_TEST_OUTPUT_DIR} false Siemens ${ITK_DATA_ROOT}/Input/test_geadw/I.001)
add_test(NAME itkGeodesicActiveContourLevelSetImageFilterZeroSigmaTest
      COMMAND ITK-IntegratedTestTestDriver itkGeodesicActiveContourLevelSetImageFilterZeroSigmaTest)
add_test(NAME itkGetAverageSliceImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/AccumulateImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/GetAverageSliceImageFilterTest.png
    itkGetAverageSliceImageFilterTest ${ITK_DATA_ROOT}/Input/DicomSeries ${ITK_TEST_OUTPUT_DIR}/GetAverageSliceImageFilterTest.png)
add_test(NAME itkGibbsTest
      COMMAND ITK-IntegratedTestTestDriver itkGibbsTest)
add_test(NAME itkGiplImageIOTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/ramp.mhd
              ${ITK_TEST_OUTPUT_DIR}/ramp.mhd
    itkGiplImageIOTest ${ITK_DATA_ROOT}/Input/ramp.gipl ${ITK_TEST_OUTPUT_DIR}/ramp.mhd)
add_test(NAME itkGiplImageIOGzTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/ramp.mhd
              ${ITK_TEST_OUTPUT_DIR}/ramp-gz.mhd
    itkGiplImageIOTest ${ITK_DATA_ROOT}/Input/ramp.gipl.gz ${ITK_TEST_OUTPUT_DIR}/ramp-gz.mhd)
add_test(NAME itkGiplImageIOTest2
      COMMAND ITK-IntegratedTestTestDriver itkGiplImageIOTest
              ${ITK_DATA_ROOT}/Input/ramp.gipl ${ITK_TEST_OUTPUT_DIR}/ramp.gipl)
add_test(NAME itkGradientAnisotropicDiffusionImageFilterTest2
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/GradientAnisotropicDiffusionImageFilterTest2.png
              ${ITK_TEST_OUTPUT_DIR}/GradientAnisotropicDiffusionImageFilterTest2.png
    itkGradientAnisotropicDiffusionImageFilterTest2 ${ITK_DATA_ROOT}/Input/cake_easy.png ${ITK_TEST_OUTPUT_DIR}/GradientAnisotropicDiffusionImageFilterTest2.png)
add_test(NAME itkGradientMagnitudeRecursiveGaussianFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkGradientMagnitudeRecursiveGaussianFilterTest)
add_test(NAME itkGradientRecursiveGaussianFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkGradientRecursiveGaussianFilterTest)
add_test(NAME itkGradientRecursiveGaussianFilterTest2
      COMMAND ITK-IntegratedTestTestDriver itkGradientRecursiveGaussianFilterTest2)
add_test(NAME itkGrayscaleGeodesicErodeDilateImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HMaximaMinimaImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleGeodesicErodeDilateImageFilterTest.png
    itkGrayscaleGeodesicErodeDilateImageFilterTest ${ITK_DATA_ROOT}/Input/cake_easy.png ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleGeodesicErodeDilateImageFilterTest.png 35)
add_test(NAME itkHashTableTest
      COMMAND ITK-IntegratedTestTestDriver itkHashTableTest)
add_test(NAME itkHConvexConcaveImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HConvexConcaveImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/HConvexConcaveImageFilterTest.png
    itkHConvexConcaveImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/HConvexConcaveImageFilterTest.png 50)
add_test(NAME itkHessianRecursiveGaussianFilterScaleSpaceTest
      COMMAND ITK-IntegratedTestTestDriver itkHessianRecursiveGaussianFilterScaleSpaceTest)
add_test(NAME itkHessianRecursiveGaussianFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkHessianRecursiveGaussianFilterTest)
add_test(NAME itkHMaximaMinimaImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HMaximaMinimaImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/HMaximaMinimaImageFilterTest.png
    itkHMaximaMinimaImageFilterTest ${ITK_DATA_ROOT}/Input/cake_easy.png ${ITK_TEST_OUTPUT_DIR}/HMaximaMinimaImageFilterTest.png 35)
add_test(NAME itkHMaximaMinimaImageFilterTest2
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HMaximaMinimaImageFilterTest2.png
              ${ITK_TEST_OUTPUT_DIR}/HMaximaMinimaImageFilterTest2.png
    itkHMaximaMinimaImageFilterTest ${ITK_DATA_ROOT}/Input/cake_hard.png ${ITK_TEST_OUTPUT_DIR}/HMaximaMinimaImageFilterTest2.png 35)
add_test(NAME itkHoughTransform2DCirclesImageTest
      COMMAND ITK-IntegratedTestTestDriver itkHoughTransform2DCirclesImageTest)
add_test(NAME itkHoughTransform2DLinesImageTest
      COMMAND ITK-IntegratedTestTestDriver itkHoughTransform2DLinesImageTest)
add_test(NAME itkImageAdaptorPipeLineTest
      COMMAND ITK-IntegratedTestTestDriver itkImageAdaptorPipeLineTest)
add_test(NAME itkImageClassifierFilterTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Statistics/ImageClassifierFilterTestClassifiedImageTest.png
              ${ITK_TEST_OUTPUT_DIR}/ImageClassifierFilterTestClassifiedImage.png
    itkImageClassifierFilterTest ${ITK_TEST_OUTPUT_DIR}/ImageClassifierFilterTestClassifiedImage.png)
add_test(NAME itkImageDuplicatorTest
      COMMAND ITK-IntegratedTestTestDriver itkImageDuplicatorTest)

add_test(NAME itkImageIteratorsForwardBackwardTest
      COMMAND ITK-IntegratedTestTestDriver itkImageIteratorsForwardBackwardTest)
add_test(NAME itkImageLinearIteratorTest
      COMMAND ITK-IntegratedTestTestDriver itkImageLinearIteratorTest)
add_test(NAME itkImageMomentsTest
      COMMAND ITK-IntegratedTestTestDriver itkImageMomentsTest)
add_test(NAME itkImageRandomIteratorTest
      COMMAND ITK-IntegratedTestTestDriver itkImageRandomIteratorTest)
add_test(NAME itkImageRandomIteratorTest2
      COMMAND ITK-IntegratedTestTestDriver itkImageRandomIteratorTest2
              ${ITK_TEST_OUTPUT_DIR}/itkImageRandomIteratorTest2Output.mha)
add_test(NAME itkImageRandomNonRepeatingIteratorWithIndexTest
      COMMAND ITK-IntegratedTestTestDriver itkImageRandomNonRepeatingIteratorWithIndexTest)
add_test(NAME itkImageRandomNonRepeatingIteratorWithIndexTest2
      COMMAND ITK-IntegratedTestTestDriver itkImageRandomNonRepeatingIteratorWithIndexTest2)
add_test(NAME itkImageSeriesReaderDimensionsTest1
      COMMAND ITK-IntegratedTestTestDriver itkImageSeriesReaderDimensionsTest
              ${ITK_DATA_ROOT}/Input/DicomSeries/Image0075.dcm ${ITK_DATA_ROOT}/Input/DicomSeries/Image0076.dcm ${ITK_DATA_ROOT}/Input/DicomSeries/Image0077.dcm)
add_test(NAME itkImageSeriesReaderDimensionsTest2
      COMMAND ITK-IntegratedTestTestDriver itkImageSeriesReaderDimensionsTest
              ${ITK_DATA_ROOT}/Input/cthead1.tif ${ITK_DATA_ROOT}/Input/cthead1.tif ${ITK_DATA_ROOT}/Input/cthead1.tif)
add_test(NAME itkImageSeriesWriterTest
      COMMAND ITK-IntegratedTestTestDriver itkImageSeriesWriterTest
              ${ITK_DATA_ROOT}/Input/DicomSeries ${ITK_TEST_OUTPUT_DIR} png)
add_test(NAME itkImageSliceIteratorTest
      COMMAND ITK-IntegratedTestTestDriver itkImageSliceIteratorTest)
add_test(NAME itkImageToHistogramFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkImageToHistogramFilterTest)
add_test(NAME itkImageToHistogramFilterTest2
      COMMAND ITK-IntegratedTestTestDriver itkImageToHistogramFilterTest2
              ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png ${ITK_TEST_OUTPUT_DIR}/itkImageToHistogramFilterTest2.txt)
add_test(NAME itkImageToHistogramFilterTest3
      COMMAND ITK-IntegratedTestTestDriver itkImageToHistogramFilterTest3
              ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkImageToHistogramFilterTest3.txt)
add_test(NAME itkImageToListSampleAdaptorTest
      COMMAND ITK-IntegratedTestTestDriver itkImageToListSampleAdaptorTest)
add_test(NAME itkImageToListSampleAdaptorTest2
      COMMAND ITK-IntegratedTestTestDriver itkImageToListSampleAdaptorTest2)
add_test(NAME itkImageToListSampleFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkImageToListSampleFilterTest)
add_test(NAME itkImageToListSampleFilterTest2
      COMMAND ITK-IntegratedTestTestDriver itkImageToListSampleFilterTest2)
add_test(NAME itkImageToListSampleFilterTest3
      COMMAND ITK-IntegratedTestTestDriver itkImageToListSampleFilterTest3)
add_test(NAME itkImportContainerTest
      COMMAND ITK-IntegratedTestTestDriver itkImportContainerTest)
add_test(NAME itkImportImageTest
      COMMAND ITK-IntegratedTestTestDriver itkImportImageTest)
add_test(NAME itkInterpolateTest
      COMMAND ITK-IntegratedTestTestDriver itkInterpolateTest)

if(ITK_BUILD_SHARED_LIBS)
  add_test(itkIOPluginTest ${TestDriver} itkIOPluginTest
    ${ITK_TEST_OUTPUT_DIR}
    "FileFreeIO::Size=128,256:Spacing=.5,.8:Origin=5,6:Direction=-1,0,0,-1"
    ${ITK_TEST_OUTPUT_DIR}/itkIOPluginTest.png
    )
  ## Create a library to test ITK IO plugins
  set(FileFreeTest_Source itkFileFreeImageIO.cxx itkFileFreeImageIOFactory.cxx)
  #set(LIBRARY_OUTPUT_PATH "${ITKTesting_BINARY_DIR}/Temporary")
  add_library(FileFreeTestLib SHARED ${FileFreeTest_Source})
  target_link_libraries(FileFreeTestLib ${ITK-IO-Base_LIBRARIES})
  #add_dependencies(IOCxxTests FileFreeTestLib)
endif(ITK_BUILD_SHARED_LIBS)


add_test(NAME itkIsoContourDistanceImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkIsoContourDistanceImageFilterTest)
add_test(NAME itkIsolatedWatershedImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/IsolatedWatershedImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/IsolatedWatershedImageFilterTest.png
    itkIsolatedWatershedImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/IsolatedWatershedImageFilterTest.png 113 84 120 99)
add_test(NAME itkIteratorTests
      COMMAND ITK-IntegratedTestTestDriver itkIteratorTests)
add_test(NAME itkJoinSeriesImageFilterStreamingTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd
              ${ITK_TEST_OUTPUT_DIR}/itkJoinSeriesImageFilterStreamingTest.mha
    itkJoinSeriesImageFilterStreamingTest ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/itkJoinSeriesImageFilterStreamingTest.mha)
add_test(NAME itkJointDomainImageToListSampleAdaptorTest
      COMMAND ITK-IntegratedTestTestDriver itkJointDomainImageToListSampleAdaptorTest)
add_test(NAME itkKdTreeBasedKmeansEstimatorTest
      COMMAND ITK-IntegratedTestTestDriver itkKdTreeBasedKmeansEstimatorTest
              ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat 1 28.54746 0.07)
add_test(NAME itkKdTreeGeneratorTest
      COMMAND ITK-IntegratedTestTestDriver itkKdTreeGeneratorTest
              ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat)
add_test(NAME itkKdTreeTest1
      COMMAND ITK-IntegratedTestTestDriver itkKdTreeTest1
              100 100 1 ${ITK_TEST_OUTPUT_DIR}/itkKdTreeTest1_100.dot)
add_test(NAME itkKdTreeTest4
      COMMAND ITK-IntegratedTestTestDriver itkKdTreeTest1
              1000 1000 1)
add_test(NAME itkKdTreeTest5
      COMMAND ITK-IntegratedTestTestDriver itkKdTreeTest1
              1000 1000 2)
add_test(NAME itkKdTreeTest6
      COMMAND ITK-IntegratedTestTestDriver itkKdTreeTest1
              1000 1000 3)
add_test(NAME itkKdTreeTest7
      COMMAND ITK-IntegratedTestTestDriver itkKdTreeTest1
              1000 1000 5)
add_test(NAME itkKdTreeTest8
      COMMAND ITK-IntegratedTestTestDriver itkKdTreeTest1
              1000 1000 10)
add_test(NAME itkKdTreeTest9
      COMMAND ITK-IntegratedTestTestDriver itkKdTreeTest1
              1000 1000 20)
add_test(NAME itkKdTreeTest10
      COMMAND ITK-IntegratedTestTestDriver itkKdTreeTest1
              1000 1000 100)
add_test(NAME itkKdTreeTest2
      COMMAND ITK-IntegratedTestTestDriver itkKdTreeTest2
              ${ITK_DATA_ROOT}/Input/pointsForKdTree.txt 1 ${ITK_TEST_OUTPUT_DIR}/itkKdTreeTest2.dot)
add_test(NAME itkKdTreeTest3
      COMMAND ITK-IntegratedTestTestDriver itkKdTreeTest2
              ${ITK_DATA_ROOT}/Input/pointsForKdTree2.txt 1 ${ITK_TEST_OUTPUT_DIR}/itkKdTreeTest3.dot)
add_test(NAME itkKdTreeTestSamplePoints
      COMMAND ITK-IntegratedTestTestDriver itkKdTreeTestSamplePoints)
add_test(NAME itkKmeansModelEstimatorTest
      COMMAND ITK-IntegratedTestTestDriver itkKmeansModelEstimatorTest)
add_test(NAME itkLaplacianRecursiveGaussianImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/LaplacianRecursiveGaussianImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/LaplacianRecursiveGaussianImageFilterTest.png
    itkLaplacianRecursiveGaussianImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/LaplacianRecursiveGaussianImageFilterTest.png)
add_test(NAME itkListSampleTest1
      COMMAND ITK-IntegratedTestTestDriver itkListSampleTest
              1)
add_test(NAME itkListSampleTest2
      COMMAND ITK-IntegratedTestTestDriver itkListSampleTest
              5)
add_test(NAME itkListSampleTest3
      COMMAND ITK-IntegratedTestTestDriver itkListSampleTest
              10)
add_test(NAME itkListSampleTest4
      COMMAND ITK-IntegratedTestTestDriver itkListSampleTest
              100)
add_test(NAME itkMahalanobisDistanceMetricTest
      COMMAND ITK-IntegratedTestTestDriver itkMahalanobisDistanceMetricTest)
add_test(NAME itkManhattanDistanceMetricTest
      COMMAND ITK-IntegratedTestTestDriver itkManhattanDistanceMetricTest)
add_test(NAME itkMaskConnectedComponentImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/MaskConnectedComponentImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/MaskConnectedComponentImageFilterTest.png
    itkMaskConnectedComponentImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/MaskConnectedComponentImageFilterTest.png 130 145)
add_test(NAME itkMaskNeighborhoodOperatorImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/MaskNeighborhoodOperatorImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/MaskNeighborhoodOperatorImageFilterTest.png
    itkMaskNeighborhoodOperatorImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/MaskNeighborhoodOperatorImageFilterTest.png)
add_test(NAME itkMathCastWithRangeCheckTest
      COMMAND ITK-IntegratedTestTestDriver itkMathCastWithRangeCheckTest)
add_test(NAME itkMathematicalMorphologyImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkMathematicalMorphologyImageFilterTest)
add_test(NAME itkMathRoundProfileTest1
      COMMAND ITK-IntegratedTestTestDriver itkMathRoundProfileTest1)
add_test(NAME itkMathRoundTest
      COMMAND ITK-IntegratedTestTestDriver itkMathRoundTest)
add_test(NAME itkMathRoundTest2
      COMMAND ITK-IntegratedTestTestDriver itkMathRoundTest2)
add_test(NAME itkMatrixImageWriteReadTest
      COMMAND ITK-IntegratedTestTestDriver itkMatrixImageWriteReadTest
              ${ITK_TEST_OUTPUT_DIR}/testMatrix1.mha)
add_test(NAME itkMaximumDecisionRuleTest
      COMMAND ITK-IntegratedTestTestDriver itkMaximumDecisionRuleTest)
add_test(NAME itkMaximumRatioDecisionRuleTest
      COMMAND ITK-IntegratedTestTestDriver itkMaximumRatioDecisionRuleTest)
add_test(NAME itkMeanReciprocalSquareDifferenceImageMetricTest
      COMMAND ITK-IntegratedTestTestDriver itkMeanReciprocalSquareDifferenceImageMetricTest)
add_test(NAME itkMeanSampleFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkMeanSampleFilterTest)
add_test(NAME itkMeanSampleFilterTest2
      COMMAND ITK-IntegratedTestTestDriver itkMeanSampleFilterTest2)
add_test(NAME itkMeanSampleFilterTest3
      COMMAND ITK-IntegratedTestTestDriver itkMeanSampleFilterTest3)
add_test(NAME itkMeanSquaresImageMetricTest
      COMMAND ITK-IntegratedTestTestDriver itkMeanSquaresImageMetricTest)
add_test(NAME itkMeasurementVectorTraitsTest
      COMMAND ITK-IntegratedTestTestDriver itkMeasurementVectorTraitsTest)
add_test(NAME itkMedialNodeCorrespondencesTest
      COMMAND ITK-IntegratedTestTestDriver itkMedialNodeCorrespondencesTest)
add_test(NAME itkMembershipFunctionBaseTest
      COMMAND ITK-IntegratedTestTestDriver itkMembershipFunctionBaseTest)
add_test(NAME itkMembershipFunctionBaseTest2
      COMMAND ITK-IntegratedTestTestDriver itkMembershipFunctionBaseTest2)
add_test(NAME itkMembershipSampleTest1
      COMMAND ITK-IntegratedTestTestDriver itkMembershipSampleTest1)
add_test(NAME itkMembershipSampleTest2
      COMMAND ITK-IntegratedTestTestDriver itkMembershipSampleTest2)
add_test(NAME itkMembershipSampleTest3
      COMMAND ITK-IntegratedTestTestDriver itkMembershipSampleTest3)
add_test(NAME itkMembershipSampleTest4
      COMMAND ITK-IntegratedTestTestDriver itkMembershipSampleTest4)
add_test(NAME itkMeshFstreamTest
      COMMAND ITK-IntegratedTestTestDriver itkMeshFstreamTest
              ${ITK_TEST_OUTPUT_DIR}/testMeshFstream.txt)
add_test(NAME itkMeshSourceGraftOutputTest
      COMMAND ITK-IntegratedTestTestDriver itkMeshSourceGraftOutputTest)
add_test(NAME itkMeshSpatialObjectIOTest
      COMMAND ITK-IntegratedTestTestDriver itkMeshSpatialObjectIOTest
              ${ITK_TEST_OUTPUT_DIR}/metameshIOTest.txt)
add_test(NAME itkMeshSpatialObjectIOTest1
      COMMAND ITK-IntegratedTestTestDriver itkMeshSpatialObjectIOTest
              ${ITK_TEST_OUTPUT_DIR}/metameshIOTest1.txt binary)
add_test(NAME itkMeshSpatialObjectIOTest2
      COMMAND ITK-IntegratedTestTestDriver itkMeshSpatialObjectIOTest
              ${ITK_TEST_OUTPUT_DIR}/metameshIOTest2.txt ${ITK_DATA_ROOT}/Input/metamesh.txt)
add_test(NAME itkMinimumDecisionRuleTest
      COMMAND ITK-IntegratedTestTestDriver itkMinimumDecisionRuleTest)
add_test(NAME itkMirrorPadImageTest
      COMMAND ITK-IntegratedTestTestDriver itkMirrorPadImageTest)
add_test(NAME itkMixtureModelComponentBaseTest
      COMMAND ITK-IntegratedTestTestDriver itkMixtureModelComponentBaseTest)
add_test(NAME itkModifiedTimeTest
      COMMAND ITK-IntegratedTestTestDriver itkModifiedTimeTest)
add_test(NAME itkModulusImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ModulusImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/ModulusImageFilterTest.png
    itkModulusImageFilterTest ${ITK_DATA_ROOT}/Input/Spots.png ${ITK_TEST_OUTPUT_DIR}/ModulusImageFilterTest.png)
add_test(NAME itkMRIBiasFieldCorrectionFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkMRIBiasFieldCorrectionFilterTest)
add_test(NAME itkMutualInformationMetricTest
      COMMAND ITK-IntegratedTestTestDriver itkMutualInformationMetricTest)
add_test(NAME itkNeighborhoodSamplerTest1
      COMMAND ITK-IntegratedTestTestDriver itkNeighborhoodSamplerTest1)
add_test(NAME itkNeuralNetworksPrintTest
      COMMAND ITK-IntegratedTestTestDriver itkNeuralNetworksPrintTest)
add_test(NAME itkNewTest
      COMMAND ITK-IntegratedTestTestDriver itkNewTest)
add_test(NAME itkNoiseImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkNoiseImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/itkNoiseImageFilterTest.png
    itkNoiseImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkNoiseImageFilterTest.png)
add_test(NAME itkNormalizedCorrelationImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/NormalizedCorrelationImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/NormalizedCorrelationImageFilterTest.png
    itkNormalizedCorrelationImageFilterTest ${ITK_DATA_ROOT}/Input/sf4.png ${ITK_DATA_ROOT}/Input/circle.png ${ITK_TEST_OUTPUT_DIR}/NormalizedCorrelationImageFilterTest.png)
add_test(NAME itkNormalizedCorrelationImageMetricTest
      COMMAND ITK-IntegratedTestTestDriver itkNormalizedCorrelationImageMetricTest)
add_test(NAME itkNormalVariateGeneratorTest1
      COMMAND ITK-IntegratedTestTestDriver itkNormalVariateGeneratorTest1)
add_test(NAME itkNumericsPrintTest
     COMMAND ITK-IntegratedTestTestDriver itkNumericsPrintTest)
add_test(NAME itkNumericsTest
      COMMAND ITK-IntegratedTestTestDriver itkNumericsTest)
add_test(NAME itkOptimizersHierarchyTest
      COMMAND ITK-IntegratedTestTestDriver itkOptimizersHierarchyTest)
add_test(NAME itkOrientedImage2DTest01
      COMMAND ITK-IntegratedTestTestDriver itkOrientedImage2DTest
              ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20DirectionPlus30.mhd 0.000 0.000 191.392 110.500 -128.500 222.569 4.000 -8.000 7.4641 -4.9282)
add_test(NAME itkOrientedImage3DTest01
      COMMAND ITK-IntegratedTestTestDriver itkOrientedImage3DTest
              ${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirection.mhd 0.000 0.000 0.000 166.277 -96.000 0.000 124.000 214.774 0.000 0.000 0.000 168.000 0.125 -4.125 9.000 -1.95425 -3.63485 9.000)
add_test(NAME itkOrientedImageAdaptorTest
      COMMAND ITK-IntegratedTestTestDriver itkOrientedImageAdaptorTest)
add_test(NAME itkOrientedImageProfileTest1
      COMMAND ITK-IntegratedTestTestDriver itkOrientedImageProfileTest1)
add_test(NAME itkOrientedImageProfileTest2
      COMMAND ITK-IntegratedTestTestDriver itkOrientedImageProfileTest2)
add_test(NAME itkOrientedImageProfileTest3
      COMMAND ITK-IntegratedTestTestDriver itkOrientedImageProfileTest3)
add_test(NAME itkOrientImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkOrientImageFilterTest)
add_test(NAME itkOrientImageFilterTest2
      COMMAND ITK-IntegratedTestTestDriver itkOrientImageFilterTest2)
add_test(NAME itkOrthogonalSwath2DPathFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkOrthogonalSwath2DPathFilterTest)
add_test(NAME itkPixelAccessTest
      COMMAND ITK-IntegratedTestTestDriver itkPixelAccessTest)
add_test(NAME itkPointGeometryTest
      COMMAND ITK-IntegratedTestTestDriver itkPointGeometryTest)
add_test(NAME itkPointSetToImageRegistrationTest_1
      COMMAND ITK-IntegratedTestTestDriver itkPointSetToImageRegistrationTest_1)
add_test(NAME itkPointSetToListSampleAdaptorTest
      COMMAND ITK-IntegratedTestTestDriver itkPointSetToListSampleAdaptorTest)
add_test(NAME itkPointSetToPointSetRegistrationTest
      COMMAND ITK-IntegratedTestTestDriver itkPointSetToPointSetRegistrationTest)
add_test(NAME itkPolylineMask2DImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkPolylineMask2DImageFilterTest
              ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkPolylineMask2DImageFilterTest.png)
add_test(NAME itkPolylineMaskImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkPolylineMaskImageFilterTest)
add_test(NAME itkPriorityQueueTest
      COMMAND ITK-IntegratedTestTestDriver itkPriorityQueueTest)
add_test(NAME itkProbabilityDistributionTest
      COMMAND ITK-IntegratedTestTestDriver itkProbabilityDistributionTest)
add_test(NAME itkProjectionImageFilterTest1
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HeadMRVolumeBinaryProjection100.tif
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeProjection100.tif
    itkProjectionImageFilterTest ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeProjection100.tif 100 0)
add_test(NAME itkPromoteDimensionImageTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/PromoteDimensionImageTest.mhd
              ${ITK_TEST_OUTPUT_DIR}/PromoteDimensionImageTest.mhd
    itkPromoteDimensionImageTest ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png ${ITK_TEST_OUTPUT_DIR}/PromoteDimensionImageTest.mhd)
add_test(NAME itkPushPopTileImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/PushPopTileImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/PushPopTileImageFilterTest.png
    itkPushPopTileImageFilterTest ${ITK_DATA_ROOT}/Input/Shapes.png ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png ${ITK_DATA_ROOT}/Input/cake_easy.png ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/PushPopTileImageFilterTest.png)
add_test(NAME itkRandomVariateGeneratorBaseTest
      COMMAND ITK-IntegratedTestTestDriver itkRandomVariateGeneratorBaseTest)
add_test(NAME itkReadWriteImageWithDictionaryTest
      COMMAND ITK-IntegratedTestTestDriver itkReadWriteImageWithDictionaryTest
              ${ITK_TEST_OUTPUT_DIR}/test.hdr)
add_test(NAME itkReadWriteImageWithDictionaryTest1
      COMMAND ITK-IntegratedTestTestDriver itkReadWriteImageWithDictionaryTest
              ${ITK_TEST_OUTPUT_DIR}/test.mha)
add_test(NAME itkReadWriteSpatialObjectTest
      COMMAND ITK-IntegratedTestTestDriver itkReadWriteSpatialObjectTest
              ${ITK_TEST_OUTPUT_DIR}/Objects.meta)
add_test(NAME itkReadWriteSpatialObjectTest1
      COMMAND ITK-IntegratedTestTestDriver itkReadWriteSpatialObjectTest
              ${ITK_TEST_OUTPUT_DIR}/Objects1.meta binary)
add_test(NAME itkReadWriteSpatialObjectTest2
      COMMAND ITK-IntegratedTestTestDriver itkReadWriteSpatialObjectTest
              ${ITK_TEST_OUTPUT_DIR}/Objects2.meta ${ITK_DATA_ROOT}/Input/SpatialObjects.meta)
add_test(NAME itkRecursiveGaussianImageFiltersOnTensorsTest
      COMMAND ITK-IntegratedTestTestDriver itkRecursiveGaussianImageFiltersOnTensorsTest)
add_test(NAME itkRecursiveGaussianImageFiltersOnVectorImageTest
      COMMAND ITK-IntegratedTestTestDriver itkRecursiveGaussianImageFiltersOnVectorImageTest)
add_test(NAME itkRecursiveGaussianImageFiltersTest
      COMMAND ITK-IntegratedTestTestDriver itkRecursiveGaussianImageFiltersTest)
add_test(NAME itkRegionGrow2DTest
      COMMAND ITK-IntegratedTestTestDriver itkRegionGrow2DTest)
add_test(NAME itkRemoveBoundaryObjectsTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/RemoveBoundaryObjectsTest.png
              ${ITK_TEST_OUTPUT_DIR}/RemoveBoundaryObjectsTest.png
    itkRemoveBoundaryObjectsTest ${ITK_DATA_ROOT}/Input/Spots.png ${ITK_TEST_OUTPUT_DIR}/RemoveBoundaryObjectsTest.png)
add_test(NAME itkRemoveBoundaryObjectsTest2
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/RemoveBoundaryObjectsTest2.png
              ${ITK_TEST_OUTPUT_DIR}/RemoveBoundaryObjectsTest2.png
    itkRemoveBoundaryObjectsTest2 ${ITK_DATA_ROOT}/Input/SpotsInverted.png ${ITK_TEST_OUTPUT_DIR}/RemoveBoundaryObjectsTest2.png)
add_test(NAME itkResampleImageTest
      COMMAND ITK-IntegratedTestTestDriver itkResampleImageTest)
add_test(NAME itkResampleImageTest2
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ResampleImageTest2.png
              ${ITK_TEST_OUTPUT_DIR}/ResampleImageTest2.png
    itkResampleImageTest2 ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_DATA_ROOT}/Input/circle.png ${ITK_TEST_OUTPUT_DIR}/ResampleImageTest2.png)
add_test(NAME itkResamplePhasedArray3DSpecialCoordinatesImageTest
      COMMAND ITK-IntegratedTestTestDriver itkResamplePhasedArray3DSpecialCoordinatesImageTest)
add_test(NAME itkRGBInterpolateImageFunctionTest
      COMMAND ITK-IntegratedTestTestDriver itkRGBInterpolateImageFunctionTest)
add_test(NAME itkSampleClassifierFilterTest1
      COMMAND ITK-IntegratedTestTestDriver itkSampleClassifierFilterTest1)
add_test(NAME itkSampleClassifierFilterTest2
      COMMAND ITK-IntegratedTestTestDriver itkSampleClassifierFilterTest2)
add_test(NAME itkSampleClassifierFilterTest3
      COMMAND ITK-IntegratedTestTestDriver itkSampleClassifierFilterTest3)
add_test(NAME itkSampleClassifierFilterTest4
      COMMAND ITK-IntegratedTestTestDriver itkSampleClassifierFilterTest4)
add_test(NAME itkSampleClassifierFilterTest5
      COMMAND ITK-IntegratedTestTestDriver itkSampleClassifierFilterTest5)
add_test(NAME itkSampleClassifierFilterTest6
      COMMAND ITK-IntegratedTestTestDriver itkSampleClassifierFilterTest6)
add_test(NAME itkSampleClassifierFilterTest7
      COMMAND ITK-IntegratedTestTestDriver itkSampleClassifierFilterTest7
              ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussian.dat ${ITK_DATA_ROOT}/Input/Statistics/TwoDimensionTwoGaussianTesting.dat)
add_test(NAME itkSampleTest
      COMMAND ITK-IntegratedTestTestDriver itkSampleTest)
add_test(NAME itkSampleTest2
      COMMAND ITK-IntegratedTestTestDriver itkSampleTest2)
add_test(NAME itkSampleTest3
      COMMAND ITK-IntegratedTestTestDriver itkSampleTest3)
add_test(NAME itkSampleTest4
      COMMAND ITK-IntegratedTestTestDriver itkSampleTest4)
add_test(NAME itkSampleToHistogramFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkSampleToHistogramFilterTest)
add_test(NAME itkSampleToHistogramFilterTest2
      COMMAND ITK-IntegratedTestTestDriver itkSampleToHistogramFilterTest2)
add_test(NAME itkSampleToHistogramFilterTest3
      COMMAND ITK-IntegratedTestTestDriver itkSampleToHistogramFilterTest3)
add_test(NAME itkSampleToHistogramFilterTest4
      COMMAND ITK-IntegratedTestTestDriver itkSampleToHistogramFilterTest4)
add_test(NAME itkSampleToHistogramFilterTest5
      COMMAND ITK-IntegratedTestTestDriver itkSampleToHistogramFilterTest5
              ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png)
add_test(NAME itkSampleToHistogramFilterTest6
      COMMAND ITK-IntegratedTestTestDriver itkSampleToHistogramFilterTest6)
add_test(NAME itkSampleToHistogramFilterTest7
      COMMAND ITK-IntegratedTestTestDriver itkSampleToHistogramFilterTest7)
add_test(NAME itkSampleToSubsampleFilterTest1
      COMMAND ITK-IntegratedTestTestDriver itkSampleToSubsampleFilterTest1)
add_test(NAME itkScalarImageKmeansImageFilter3DTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_SOURCE_DIR}/Examples/Data/KmeansTest_T1KmeansPrelimSegmentation.nii.gz
              ${ITK_TEST_OUTPUT_DIR}/KmeansTest_T1KmeansPrelimSegmentation.nii.gz
    itkScalarImageKmeansImageFilter3DTest ${ITK_SOURCE_DIR}/Examples/Data/KmeansTest_T1UCharRaw.nii.gz ${ITK_SOURCE_DIR}/Examples/Data/KmeansTest_T1RawSkullStrip.nii.gz ${ITK_TEST_OUTPUT_DIR}/KmeansTest_T1KmeansPrelimSegmentation.nii.gz)
add_test(NAME itkScalarImageToCooccurrenceListSampleFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkScalarImageToCooccurrenceListSampleFilterTest)
add_test(NAME itkScalarImageToCooccurrenceMatrixFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkScalarImageToCooccurrenceMatrixFilterTest)
add_test(NAME itkScalarImageToCooccurrenceMatrixFilterTest2
      COMMAND ITK-IntegratedTestTestDriver itkScalarImageToCooccurrenceMatrixFilterTest2)
add_test(NAME itkScalarImageToTextureFeaturesFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkScalarImageToTextureFeaturesFilterTest)
add_test(NAME itkShrinkImagePreserveObjectPhysicalLocations
      COMMAND ITK-IntegratedTestTestDriver itkShrinkImagePreserveObjectPhysicalLocations)
add_test(NAME itkShrinkImageStreamingTest
      COMMAND ITK-IntegratedTestTestDriver itkShrinkImageStreamingTest)
add_test(NAME itkShrinkImageTest
      COMMAND ITK-IntegratedTestTestDriver itkShrinkImageTest)
add_test(NAME itkSimpleFilterWatcherTest
      COMMAND ITK-IntegratedTestTestDriver itkSimpleFilterWatcherTest)
add_test(NAME itkSimplexMeshWithFloatCoordRepTest
      COMMAND ITK-IntegratedTestTestDriver itkSimplexMeshWithFloatCoordRepTest)
add_test(NAME itkSparseFrequencyContainer2Test
      COMMAND ITK-IntegratedTestTestDriver itkSparseFrequencyContainer2Test)
add_test(NAME itkSparseImageTest
      COMMAND ITK-IntegratedTestTestDriver itkSparseImageTest)
add_test(NAME itkSpatialObjectToImageRegistrationTest
      COMMAND ITK-IntegratedTestTestDriver itkSpatialObjectToImageRegistrationTest)
add_test(NAME itkStandardDeviationPerComponentSampleFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkStandardDeviationPerComponentSampleFilterTest)
add_test(NAME itkStatisticsAlgorithmTest
      COMMAND ITK-IntegratedTestTestDriver itkStatisticsAlgorithmTest)
add_test(NAME itkStatisticsAlgorithmTest2
      COMMAND ITK-IntegratedTestTestDriver itkStatisticsAlgorithmTest2)
add_test(NAME itkStatisticsTypesTest
      COMMAND ITK-IntegratedTestTestDriver itkStatisticsTypesTest)
add_test(NAME itkSTLThreadTest
      COMMAND ITK-IntegratedTestTestDriver itkSTLThreadTest)
add_test(NAME itkStreamingImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkStreamingImageFilterTest)
add_test(NAME itkStreamingImageFilterTest2
      COMMAND ITK-IntegratedTestTestDriver itkStreamingImageFilterTest2)
add_test(NAME itkStreamingImageFilterTest3_1
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Input/CellsFluorescence1.png
              ${ITK_TEST_OUTPUT_DIR}/itkStreamingImageFilterTest3_1.png
    itkStreamingImageFilterTest3 ${ITK_DATA_ROOT}/Input/CellsFluorescence1.png ${ITK_TEST_OUTPUT_DIR}/itkStreamingImageFilterTest3_1.png 10)
add_test(NAME itkStreamingImageFilterTest3_2
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Input/CellsFluorescence1.png
              ${ITK_TEST_OUTPUT_DIR}/itkStreamingImageFilterTest3_2.png
    itkStreamingImageFilterTest3 ${ITK_DATA_ROOT}/Input/CellsFluorescence1.png ${ITK_TEST_OUTPUT_DIR}/itkStreamingImageFilterTest3_2.png 1000)
add_test(NAME itkSubsampleTest
      COMMAND ITK-IntegratedTestTestDriver itkSubsampleTest)
add_test(NAME itkSubsampleTest2
      COMMAND ITK-IntegratedTestTestDriver itkSubsampleTest2)
add_test(NAME itkSubsampleTest3
      COMMAND ITK-IntegratedTestTestDriver itkSubsampleTest3)
add_test(NAME itkSupervisedImageClassifierTest
      COMMAND ITK-IntegratedTestTestDriver itkSupervisedImageClassifierTest)
add_test(NAME itkSymmetricSecondRankTensorImageReadTest
      COMMAND ITK-IntegratedTestTestDriver itkSymmetricSecondRankTensorImageReadTest
              ${ITK_TEST_OUTPUT_DIR}/testSymmetricTensor.nrrd)
add_test(NAME itkSymmetricSecondRankTensorImageWriteReadTest
      COMMAND ITK-IntegratedTestTestDriver itkSymmetricSecondRankTensorImageWriteReadTest
              ${ITK_TEST_OUTPUT_DIR}/testSymmetricTensorWriteRead.mha)
add_test(NAME itkTDistributionTest
      COMMAND ITK-IntegratedTestTestDriver itkTDistributionTest)
add_test(NAME itkThreadDefsTest
      COMMAND ITK-IntegratedTestTestDriver itkThreadDefsTest)
add_test(NAME itkTimeProbesTest
      COMMAND ITK-IntegratedTestTestDriver itkTimeProbesTest)
add_test(NAME itkBlackTopHatImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkBlackTopHatImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/itkBlackTopHatImageFilterTest.png
    itkTopHatImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkBlackTopHatImageFilterTest.png 0 3)
add_test(NAME itkWhiteTopHatImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkWhiteTopHatImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/itkWhiteTopHatImageFilterTest.png
    itkTopHatImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkWhiteTopHatImageFilterTest.png 1 3)
add_test(NAME itkTriangleCellTest
      COMMAND ITK-IntegratedTestTestDriver itkTriangleCellTest)
add_test(NAME itkTriangleMeshToSimplexMeshFilter2Test
      COMMAND ITK-IntegratedTestTestDriver itkTriangleMeshToSimplexMeshFilter2Test)

add_executable(itkUnicodeIOTest itkUnicodeIOTest.cxx)
add_test(NAME itkUnicodeIOTest COMMAND itkUnicodeIOTest)

add_test(NAME itkUnsharpMaskLevelSetImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkUnsharpMaskLevelSetImageFilterTest)
add_test(NAME itkVectorConfidenceConnectedImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorConfidenceConnectedImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/VectorConfidenceConnectedImageFilterTest.png
    itkVectorConfidenceConnectedImageFilterTest ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png ${ITK_TEST_OUTPUT_DIR}/VectorConfidenceConnectedImageFilterTest.png 72 44 23 67 5.0 6)
add_test(NAME itkVectorGeometryTest
      COMMAND ITK-IntegratedTestTestDriver itkVectorGeometryTest)
add_test(NAME itkVectorGradientMagnitudeImageFilterTest3
      COMMAND ITK-IntegratedTestTestDriver itkVectorGradientMagnitudeImageFilterTest3
              ${ITK_DATA_ROOT}/Input/VHFColor.mhd ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest3.mha 1)
add_test(NAME itkVectorImageReadWriteTest
      COMMAND ITK-IntegratedTestTestDriver itkVectorImageReadWriteTest
              ${ITK_TEST_OUTPUT_DIR}/VectorImageReadWriteTest.mhd)
add_test(NAME itkVectorImageReadWriteTest2
      COMMAND ITK-IntegratedTestTestDriver itkVectorImageReadWriteTest
              ${ITK_TEST_OUTPUT_DIR}/VectorImageReadWriteTest.nrrd)
add_test(NAME itkVectorImageTest
      COMMAND ITK-IntegratedTestTestDriver itkVectorImageTest
              ${ITK_TEST_OUTPUT_DIR}/VectorImage.nrrd ${ITK_TEST_OUTPUT_DIR}/VectorImage.mhd)
add_test(NAME itkVNLRoundProfileTest1
      COMMAND ITK-IntegratedTestTestDriver itkVNLRoundProfileTest1)
add_test(NAME itkVoronoiPartitioningImageFilterTest1
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/VoronoiPartioningImageFilterTest1.png
              ${ITK_TEST_OUTPUT_DIR}/VoronoiPartioningImageFilterTest1.png
    itkVoronoiPartitioningImageFilterTest ${ITK_DATA_ROOT}/Input/sf4.png ${ITK_TEST_OUTPUT_DIR}/VoronoiPartioningImageFilterTest1.png 1)
add_test(NAME itkVoronoiPartitioningImageFilterTest2
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/VoronoiPartioningImageFilterTest2.png
              ${ITK_TEST_OUTPUT_DIR}/VoronoiPartioningImageFilterTest2.png
    itkVoronoiPartitioningImageFilterTest ${ITK_DATA_ROOT}/Input/sf4.png ${ITK_TEST_OUTPUT_DIR}/VoronoiPartioningImageFilterTest2.png 0)
add_test(NAME itkVTKImageIO2Test2
      COMMAND ITK-IntegratedTestTestDriver itkVTKImageIO2Test2
              ${ITK_TEST_OUTPUT_DIR}/itkVTKImageIO2Test2.vtk)
add_test(NAME itkVTKImageIOTest2
      COMMAND ITK-IntegratedTestTestDriver itkVTKImageIOTest2
              ${ITK_DATA_ROOT}/Input/VHFColor.mhd ${ITK_TEST_OUTPUT_DIR}/testVector.vtk)
add_test(NAME itkVTKPolyDataIOQuadEdgeMeshTest
      COMMAND ITK-IntegratedTestTestDriver itkVTKPolyDataIOQuadEdgeMeshTest
              ${ITK_DATA_ROOT}/Input/genusZeroSurface01.vtk ${ITK_TEST_OUTPUT_DIR}/VTKPolyDataIOTest.vtk)
add_test(NAME itkVTKPolyDataReaderQuadEdgeMeshTest
      COMMAND ITK-IntegratedTestTestDriver itkVTKPolyDataReaderQuadEdgeMeshTest
              ${ITK_DATA_ROOT}/Input/genusZeroSurface01.vtk)
add_test(NAME itkWarpImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkWarpImageFilterTest)
add_test(NAME itkWarpVectorImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkWarpVectorImageFilterTest)
add_test(NAME itkWatershedImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkWatershedImageFilterTest)
add_test(NAME itkWeightedCentroidKdTreeGeneratorTest1
      COMMAND ITK-IntegratedTestTestDriver itkWeightedCentroidKdTreeGeneratorTest1
              1000 1000 1)
add_test(NAME itkWeightedCentroidKdTreeGeneratorTest2
      COMMAND ITK-IntegratedTestTestDriver itkWeightedCentroidKdTreeGeneratorTest1
              1000 1000 2)
add_test(NAME itkWeightedCentroidKdTreeGeneratorTest3
      COMMAND ITK-IntegratedTestTestDriver itkWeightedCentroidKdTreeGeneratorTest1
              1000 1000 3)
add_test(NAME itkWeightedCentroidKdTreeGeneratorTest4
      COMMAND ITK-IntegratedTestTestDriver itkWeightedCentroidKdTreeGeneratorTest1
              1000 1000 5)
add_test(NAME itkWeightedCentroidKdTreeGeneratorTest5
      COMMAND ITK-IntegratedTestTestDriver itkWeightedCentroidKdTreeGeneratorTest1
              1000 1000 10)
add_test(NAME itkWeightedCentroidKdTreeGeneratorTest6
      COMMAND ITK-IntegratedTestTestDriver itkWeightedCentroidKdTreeGeneratorTest1
              1000 1000 20)
add_test(NAME itkWeightedCentroidKdTreeGeneratorTest7
      COMMAND ITK-IntegratedTestTestDriver itkWeightedCentroidKdTreeGeneratorTest1
              1000 1000 100)
add_test(NAME itkWeightedCentroidKdTreeGeneratorTest8
      COMMAND ITK-IntegratedTestTestDriver itkWeightedCentroidKdTreeGeneratorTest8
              1000 1000 1)
add_test(NAME itkWeightedCentroidKdTreeGeneratorTest9
      COMMAND ITK-IntegratedTestTestDriver itkWeightedCentroidKdTreeGeneratorTest9
              1000 1000 1)
add_test(NAME itkWeightedCovarianceSampleFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkWeightedCovarianceSampleFilterTest)
add_test(NAME itkWeightedCovarianceSampleFilterTest2
      COMMAND ITK-IntegratedTestTestDriver itkWeightedCovarianceSampleFilterTest2)
add_test(NAME itkWeightedMeanSampleFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkWeightedMeanSampleFilterTest)
add_test(NAME itkWrapPadImageTest
      COMMAND ITK-IntegratedTestTestDriver itkWrapPadImageTest)
add_test(NAME itkZeroFluxBoundaryConditionTest
      COMMAND ITK-IntegratedTestTestDriver itkZeroFluxBoundaryConditionTest)
add_test(NAME NNetClassifierTest1
      COMMAND ITK-IntegratedTestTestDriver NNetClassifierTest1
              ${ITK_DATA_ROOT}/Input/NeuralNetworks/train.txt ${ITK_DATA_ROOT}/Input/NeuralNetworks/test.txt)
add_test(NAME NNetClassifierTest2
      COMMAND ITK-IntegratedTestTestDriver NNetClassifierTest2
              ${ITK_DATA_ROOT}/Input/NeuralNetworks/train.txt ${ITK_DATA_ROOT}/Input/NeuralNetworks/test.txt)
add_test(NAME NNetClassifierTest3
      COMMAND ITK-IntegratedTestTestDriver NNetClassifierTest3
              ${ITK_DATA_ROOT}/Input/NeuralNetworks/train.txt ${ITK_DATA_ROOT}/Input/NeuralNetworks/test.txt)
add_test(NAME NNetClassifierTest4
      COMMAND ITK-IntegratedTestTestDriver NNetClassifierTest4
              ${ITK_DATA_ROOT}/Input/NeuralNetworks/train.txt ${ITK_DATA_ROOT}/Input/NeuralNetworks/test.txt)
add_test(NAME QPropXORTest1
      COMMAND ITK-IntegratedTestTestDriver QPropXORTest1
              ${ITK_DATA_ROOT}/Input/NeuralNetworks/qpropxortest.txt)
add_test(NAME RBFTest1
      COMMAND ITK-IntegratedTestTestDriver RBFTest1
              ${ITK_DATA_ROOT}/Input/NeuralNetworks/rbf_multivar_gauss_train.txt ${ITK_DATA_ROOT}/Input/NeuralNetworks/rbf_multivar_gauss_test.txt)
add_test(NAME XORTest1
      COMMAND ITK-IntegratedTestTestDriver XORTest1
              ${ITK_DATA_ROOT}/Input/NeuralNetworks/xortest.txt)
add_test(NAME XORTest2
      COMMAND ITK-IntegratedTestTestDriver XORTest2
              ${ITK_DATA_ROOT}/Input/NeuralNetworks/xortest.txt)
add_test(NAME itkRecursiveGaussianScaleSpaceTest1
      COMMAND ITK-IntegratedTestTestDriver
              itkRecursiveGaussianScaleSpaceTest1)
add_test(NAME itkDoubleThresholdImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/DoubleThresholdImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/DoubleThresholdImageFilterTest.png itkDoubleThresholdImageFilterTest
            ${ITK_DATA_ROOT}/../../Examples/Data/BrainProtonDensitySlice.png
            ${ITK_TEST_OUTPUT_DIR}/DoubleThresholdImageFilterTest.png 217 246 255 255)
add_test(NAME itkDoubleThresholdImageFilterTest2
      COMMAND ITK-IntegratedTestTestDriver
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/DoubleThresholdImageFilterTest2.png
            ${ITK_TEST_OUTPUT_DIR}/DoubleThresholdImageFilterTest2.png itkDoubleThresholdImageFilterTest
            ${ITK_DATA_ROOT}/../../Examples/Data/BrainProtonDensitySlice.png
            ${ITK_TEST_OUTPUT_DIR}/DoubleThresholdImageFilterTest2.png 150 164 164 180)
