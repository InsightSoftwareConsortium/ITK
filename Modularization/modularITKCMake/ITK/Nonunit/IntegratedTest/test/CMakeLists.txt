itk_module_test()
set(ITK-IntegratedTestTests
itkAccumulateImageFilterTest.cxx
itkAdaptiveHistogramEqualizationImageFilterTest.cxx
itkAdaptorComparisonTest.cxx
#itkAlgorithmsPrintTest.cxx
#itkAlgorithmsPrintTest2.cxx
#itkAlgorithmsPrintTest3.cxx
#itkAlgorithmsPrintTest4.cxx
#itkAlgorithmsPrintTests.cxx
itkBasicArchitectureTest.cxx
#itkBasicFiltersPrintTest.cxx
#itkBasicFiltersPrintTest2.cxx
itkBayesianClassifierImageFilterTest.cxx
itkBinaryProjectionImageFilterTest.cxx
itkBinaryThresholdImageFilterTest2.cxx
itkBinaryThresholdProjectionImageFilterTest.cxx
itkBinaryThresholdSpatialFunctionTest.cxx
itkBloxCoreAtomTest.cxx
itkBoundaryConditionTest.cxx
itkBSplineDeformableTransformTest.cxx
itkBSplineDeformableTransformTest2.cxx
itkBSplineDeformableTransformTest3.cxx
itkBSplineInterpolateImageFunctionTest.cxx
itkBSplineResampleImageFilterTest.cxx
itkBSplineResampleImageFunctionTest.cxx
itkByteSwapTest.cxx
itkCenteredTransformInitializerTest.cxx
itkCenteredVersorTransformInitializerTest.cxx
itkChiSquareDistributionTest.cxx
#itkCommonPrintTest.cxx
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
#itkCurvatureRegistrationFilterTest.cxx
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
itkFEMElement2DMembraneTest.cxx
itkFEMElement2DQuadraticTriangularTest.cxx
itkFEMElement2DStrainTest.cxx
itkFEMElement3DMembraneTest.cxx
itkFEMElementTest.cxx
itkFEMExceptionTest.cxx
itkFEMGenerateMeshTest.cxx
itkFEMLinearSystemWrapperDenseVNLTest.cxx
itkFEMLinearSystemWrapperItpackTest.cxx
itkFEMLinearSystemWrapperItpackTest2.cxx
itkFEMLinearSystemWrapperVNLTest.cxx
itkFEMPArrayTest.cxx
#itkFFTTest.cxx
#itkFileFreeImageIO.cxx
#itkFileFreeImageIOFactory.cxx
itkFilterDispatchTest.cxx
itkFilterImageAddTest.cxx
itkFloodFilledSpatialFunctionTest.cxx
itkFloodFillIteratorTest.cxx
itkGaussianDistributionTest.cxx
itkGaussianMembershipFunctionTest.cxx
itkGaussianMixtureModelComponentTest.cxx
itkGDCMImageIOTest.cxx
itkGDCMImageIOTest2.cxx
itkGDCMSeriesReadImageWrite.cxx
itkGDCMSeriesStreamReadImageWrite.cxx
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
itkHistogramTest.cxx
itkHistogramToEntropyImageFilterTest1.cxx
itkHistogramToEntropyImageFilterTest2.cxx
itkHistogramToIntensityImageFilterTest1.cxx
itkHistogramToIntensityImageFilterTest2.cxx
itkHistogramToLogProbabilityImageFilterTest1.cxx
itkHistogramToLogProbabilityImageFilterTest2.cxx
itkHistogramToProbabilityImageFilterTest1.cxx
itkHistogramToProbabilityImageFilterTest2.cxx
itkHistogramToTextureFeaturesFilterTest.cxx
itkHMaximaMinimaImageFilterTest.cxx
itkHoughTransform2DCirclesImageTest.cxx
itkHoughTransform2DLinesImageTest.cxx
itkImageAdaptorPipeLineTest.cxx
itkImageClassifierFilterTest.cxx
itkImageComputeOffsetAndIndexTest.cxx
itkImageDuplicatorTest.cxx
itkImageFileReaderDimensionsTest.cxx
itkImageFileReaderStreamingTest.cxx
itkImageFileReaderStreamingTest2.cxx
itkImageFileWriterPastingTest1.cxx
itkImageFileWriterPastingTest2.cxx
itkImageFileWriterPastingTest3.cxx
itkImageFileWriterStreamingPastingCompressingTest1.cxx
itkImageFileWriterStreamingTest1.cxx
itkImageFileWriterStreamingTest2.cxx
itkImageFileWriterTest2.cxx
itkImageFileWriterUpdateLargestPossibleRegionTest.cxx
itkImageFillBufferTest.cxx
itkImageIOBaseTest.cxx
itkImageIODirection2DTest.cxx
itkImageIODirection3DTest.cxx
itkImageIOFileNameExtensionsTests.cxx
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
itkImageTransformTest.cxx
itkImportContainerTest.cxx
itkImportImageTest.cxx
itkInterpolateTest.cxx
itkIOPluginTest.cxx
#itkIOPrintTest.cxx
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
itkMetaImageStreamingIOTest.cxx
itkMetaImageStreamingWriterIOTest.cxx
itkMinimumDecisionRuleTest.cxx
itkMirrorPadImageTest.cxx
itkMixtureModelComponentBaseTest.cxx
itkModifiedTimeTest.cxx
itkModulusImageFilterTest.cxx
itkMRIBiasFieldCorrectionFilterTest.cxx
itkMutualInformationMetricTest.cxx
itkNeighborhoodSamplerTest1.cxx
#itkNeuralNetworksPrintTest.cxx
itkNewTest.cxx
itkNoiseImageFilterTest.cxx
itkNormalizedCorrelationImageFilterTest.cxx
itkNormalizedCorrelationImageMetricTest.cxx
itkNormalVariateGeneratorTest1.cxx
itkNrrdComplexImageReadTest.cxx
itkNrrdComplexImageReadWriteTest.cxx
itkNrrdCovariantVectorImageReadTest.cxx
itkNrrdCovariantVectorImageReadWriteTest.cxx
itkNrrdDiffusionTensor3DImageReadTensorDoubleWriteTensorDoubleTest.cxx
itkNrrdDiffusionTensor3DImageReadTest.cxx
itkNrrdDiffusionTensor3DImageReadWriteTest.cxx
itkNrrdImageReadWriteTest.cxx
itkNrrdRGBAImageReadWriteTest.cxx
itkNrrdRGBImageReadWriteTest.cxx
itkNrrdVectorImageReadTest.cxx
itkNrrdVectorImageReadWriteTest.cxx
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
itkQuadEdgeMeshAddFaceTest1.cxx
itkQuadEdgeMeshAddFaceTest2.cxx
itkQuadEdgeMeshBasicLayerTest.cxx
itkQuadEdgeMeshCellInterfaceTest.cxx
itkQuadEdgeMeshCountingCellsTest.cxx
itkQuadEdgeMeshDeleteEdgeTest.cxx
itkQuadEdgeMeshDeletePointAndReorderIDsTest.cxx
itkQuadEdgeMeshEulerOperatorCreateCenterVertexTest.cxx
itkQuadEdgeMeshEulerOperatorDeleteCenterVertexTest.cxx
itkQuadEdgeMeshEulerOperatorFlipTest.cxx
itkQuadEdgeMeshEulerOperatorJoinFacetTest.cxx
itkQuadEdgeMeshEulerOperatorJoinVertexTest.cxx
itkQuadEdgeMeshEulerOperatorSplitEdgeTest.cxx
itkQuadEdgeMeshEulerOperatorSplitFaceTest.cxx
itkQuadEdgeMeshEulerOperatorSplitVertexTest.cxx
itkQuadEdgeMeshIteratorTest.cxx
itkQuadEdgeMeshNoPointConstTest.cxx
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
#itkSparseFieldFourthOrderLevelSetImageFilterTest.cxx
itkSparseFrequencyContainer2Test.cxx
itkSparseImageTest.cxx
#itkSpatialObjectPrintTest.cxx
itkSpatialObjectToImageRegistrationTest.cxx
itkSplineKernelTransformTest.cxx
itkStandardDeviationPerComponentSampleFilterTest.cxx
itkStatisticsAlgorithmTest.cxx
itkStatisticsAlgorithmTest2.cxx
#itkStatisticsPrintTest.cxx
itkStatisticsTypesTest.cxx
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
itkTransformFileReaderWriterTest.cxx
itkTransformIOTest.cxx
itkTransformsSetParametersTest.cxx
itkTriangleMeshToSimplexMeshFilter2Test.cxx
#itkUnicodeIOTest.cxx
itkUnsharpMaskLevelSetImageFilterTest.cxx
itkVectorAnisotropicDiffusionImageFilterTest.cxx
itkVectorConfidenceConnectedImageFilterTest.cxx
itkVectorGeometryTest.cxx
itkVectorGradientMagnitudeImageFilterTest3.cxx
itkVectorImageReadWriteTest.cxx
itkVectorImageTest.cxx
itkVersorRigid3DTransformTest.cxx
itkVersorTransformTest.cxx
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
testMetaArray.cxx
testMetaBlob.cxx
testMetaCommand.cxx
testMetaGroup.cxx
testMetaImage.cxx
testMetaLandmark.cxx
testMetaLine.cxx
testMetaMesh.cxx
testMetaObject.cxx
testMetaScene.cxx
testMetaSurface.cxx
testMetaTube.cxx
testMetaUtils.cxx
XORTest1.cxx
XORTest2.cxx
)

CreateTestDriver_SupportBuildInIOFactories(ITK-IntegratedTest  "${ITK-IntegratedTest-Test_LIBRARIES}" "${ITK-IntegratedTestTests}")

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
#add_test(NAME itkAlgorithmsPrintTest
#      COMMAND ITK-IntegratedTestTestDriver itkAlgorithmsPrintTest)
#add_test(NAME itkAlgorithmsPrintTest2
#      COMMAND ITK-IntegratedTestTestDriver itkAlgorithmsPrintTest2)
#add_test(NAME itkAlgorithmsPrintTest3
#      COMMAND ITK-IntegratedTestTestDriver itkAlgorithmsPrintTest3)
#add_test(NAME itkAlgorithmsPrintTest4
#      COMMAND ITK-IntegratedTestTestDriver itkAlgorithmsPrintTest4)
add_test(NAME itkBasicArchitectureTest
      COMMAND ITK-IntegratedTestTestDriver itkBasicArchitectureTest)
#add_test(NAME itkBasicFiltersPrintTest
#      COMMAND ITK-IntegratedTestTestDriver itkBasicFiltersPrintTest)
#add_test(NAME itkBasicFiltersPrintTest2
#      COMMAND ITK-IntegratedTestTestDriver itkBasicFiltersPrintTest2)
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
add_test(NAME itkBSplineDeformableTransformTest
      COMMAND ITK-IntegratedTestTestDriver itkBSplineDeformableTransformTest)
add_test(NAME itkBSplineDeformableTransformTest2
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Common/itkBSplineDeformableTransformTest2PixelCentered.png
              ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest2PixelCentered.png
    itkBSplineDeformableTransformTest2 ${ITK_SOURCE_DIR}/Examples/Data/BSplineDisplacements5.txt ${ITK_SOURCE_DIR}/Examples/Data/DiagonalLines.png ${ITK_SOURCE_DIR}/Examples/Data/DiagonalLines.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest2PixelCentered.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest2DeformationFieldPixelCentered.mhd 1)
add_test(NAME itkBSplineDeformableTransformTest3
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Common/itkBSplineDeformableTransformTest3PixelCentered.png
              ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest3PixelCentered.png
    itkBSplineDeformableTransformTest2 ${ITK_SOURCE_DIR}/Examples/Data/BSplineDisplacements3.txt ${ITK_SOURCE_DIR}/Examples/Data/DiagonalLines.png ${ITK_SOURCE_DIR}/Examples/Data/DiagonalLines.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest3PixelCentered.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest3DeformationFieldPixelCentered.mhd 2)
add_test(NAME itkBSplineDeformableTransformTest4
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Common/itkBSplineDeformableTransformTest4PixelCentered.png
              ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest4PixelCentered.png
    itkBSplineDeformableTransformTest2 ${ITK_SOURCE_DIR}/Examples/Data/BSplineDisplacements1.txt ${ITK_SOURCE_DIR}/Examples/Data/DiagonalLines.png ${ITK_SOURCE_DIR}/Examples/Data/DiagonalLines.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest4PixelCentered.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest4DeformationFieldPixelCentered.mhd 3)
add_test(NAME itkBSplineDeformableTransformTest5
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Common/itkBSplineDeformableTransformTest5PixelCentered.png
              ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest5PixelCentered.png
    itkBSplineDeformableTransformTest3 ${ITK_SOURCE_DIR}/Examples/Data/BSplineDisplacements1.txt ${ITK_SOURCE_DIR}/Examples/Data/DiagonalLines.png ${ITK_SOURCE_DIR}/Examples/Data/DiagonalLines.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest5PixelCentered.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest5DeformationFieldPixelCentered.mhd 0)
add_test(NAME itkBSplineDeformableTransformTest6
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Common/itkBSplineDeformableTransformTest5PixelCentered.png
              ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest6PixelCentered.png
    itkBSplineDeformableTransformTest3 ${ITK_SOURCE_DIR}/Examples/Data/BSplineDisplacements1.txt ${ITK_SOURCE_DIR}/Examples/Data/DiagonalLines.png ${ITK_SOURCE_DIR}/Examples/Data/DiagonalLines.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest6PixelCentered.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest6DeformationFieldPixelCentered.mhd 1)
add_test(NAME itkBSplineDeformableTransformTest7
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Common/itkBSplineDeformableTransformTest5PixelCentered.png
              ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest7PixelCentered.png
    itkBSplineDeformableTransformTest3 ${ITK_SOURCE_DIR}/Examples/Data/BSplineDisplacements1.txt ${ITK_SOURCE_DIR}/Examples/Data/DiagonalLines.png ${ITK_SOURCE_DIR}/Examples/Data/DiagonalLines.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest7PixelCentered.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest7DeformationFieldPixelCentered.mhd 2)
add_test(NAME itkBSplineInterpolateImageFunctionTest
      COMMAND ITK-IntegratedTestTestDriver itkBSplineInterpolateImageFunctionTest)
add_test(NAME itkBSplineResampleImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkBSplineResampleImageFilterTest)
add_test(NAME itkBSplineResampleImageFunctionTest
      COMMAND ITK-IntegratedTestTestDriver itkBSplineResampleImageFunctionTest)
add_test(NAME itkByteSwapTest
      COMMAND ITK-IntegratedTestTestDriver itkByteSwapTest)
add_test(NAME itkCenteredTransformInitializerTest
      COMMAND ITK-IntegratedTestTestDriver itkCenteredTransformInitializerTest)
add_test(NAME itkCenteredVersorTransformInitializerTest
      COMMAND ITK-IntegratedTestTestDriver itkCenteredVersorTransformInitializerTest)
add_test(NAME itkChiSquareDistributionTest
      COMMAND ITK-IntegratedTestTestDriver itkChiSquareDistributionTest)
#add_test(NAME itkCommonPrintTest
#      COMMAND ITK-IntegratedTestTestDriver itkCommonPrintTest)
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
add_test(NAME itkFEMElement2DMembraneTest
      COMMAND ITK-IntegratedTestTestDriver itkFEMElement2DMembraneTest)
add_test(NAME itkFEMElement2DQuadraticTriangularTest
      COMMAND ITK-IntegratedTestTestDriver itkFEMElement2DQuadraticTriangularTest)
add_test(NAME itkFEMElement2DStrainTest
      COMMAND ITK-IntegratedTestTestDriver itkFEMElement2DStrainTest)
add_test(NAME itkFEMElement3DMembraneTest
      COMMAND ITK-IntegratedTestTestDriver itkFEMElement3DMembraneTest)
add_test(NAME itkFEMC0HexahedralElement-NodalLoads-BCs
      COMMAND ITK-IntegratedTestTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/hexa2.fem)
add_test(NAME itkFEMC0HexahedralElement-NoLoads
      COMMAND ITK-IntegratedTestTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/hexa3.fem)
add_test(NAME itkFEMC0HexahedralElement-GravityLoad-BCs
      COMMAND ITK-IntegratedTestTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/hexa4-grav.fem)
add_test(NAME itkFEMC0QuadElement-NodalLoads-BCs
      COMMAND ITK-IntegratedTestTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/quad2-small.fem)
add_test(NAME itkFEMC0QuadElement-Strain
      COMMAND ITK-IntegratedTestTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/quad2-strain.fem)
add_test(NAME itkFEMC0QuadElement-NoLoads
      COMMAND ITK-IntegratedTestTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/quad4.fem)
add_test(NAME itkFEMC0QuadElement-GravityLoad-BCs
      COMMAND ITK-IntegratedTestTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/quad6-grav.fem)
add_test(NAME itkFEMLoadLandmarkImplementation
      COMMAND ITK-IntegratedTestTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/quad-lm.fem)
add_test(NAME itkFEMC0TetrahedralElement-NodalLoads-BCs
      COMMAND ITK-IntegratedTestTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/tetra2.fem)
add_test(NAME itkFEMC0TetrahedralElement-NoLoads
      COMMAND ITK-IntegratedTestTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/tetra3.fem)
add_test(NAME itkFEMC0TetrahedralElement-GravityLoad-BCs
      COMMAND ITK-IntegratedTestTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/tetra4-grav.fem)
add_test(NAME itkFEMC0QuadElementTrapezoidalGeometry-NoLoads
      COMMAND ITK-IntegratedTestTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/trapezoid.fem)
add_test(NAME itkFEMC0TriangularElement-NodalLoads-BCs
      COMMAND ITK-IntegratedTestTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/tri2.fem)
add_test(NAME itkFEMC0TriangularElement-NoLoads
      COMMAND ITK-IntegratedTestTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/tri3.fem)
add_test(NAME itkFEMC0TriangularElement-Strain
      COMMAND ITK-IntegratedTestTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/tri3-e.fem)
add_test(NAME itkFEMC0TriangularElement-Quadratic
      COMMAND ITK-IntegratedTestTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/tri3-q.fem)
add_test(NAME itkFEMTruss
      COMMAND ITK-IntegratedTestTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/truss.fem)
add_test(NAME itkFEMExceptionTest
      COMMAND ITK-IntegratedTestTestDriver itkFEMExceptionTest)
add_test(NAME itkFEMGenerateMeshTest
      COMMAND ITK-IntegratedTestTestDriver itkFEMGenerateMeshTest)
add_test(NAME itkFEMLinearSystemWrapperDenseVNLTest
      COMMAND ITK-IntegratedTestTestDriver itkFEMLinearSystemWrapperDenseVNLTest)
add_test(NAME itkFEMLinearSystemWrapperItpackTest
      COMMAND ITK-IntegratedTestTestDriver itkFEMLinearSystemWrapperItpackTest)
add_test(NAME itkFEMLinearSystemWrapperItpackTest1
      COMMAND ITK-IntegratedTestTestDriver itkFEMLinearSystemWrapperItpackTest
              0)
add_test(NAME itkFEMLinearSystemWrapperItpackTest2
      COMMAND ITK-IntegratedTestTestDriver itkFEMLinearSystemWrapperItpackTest
              1)
add_test(NAME itkFEMLinearSystemWrapperItpackTest3
      COMMAND ITK-IntegratedTestTestDriver itkFEMLinearSystemWrapperItpackTest
              2)
add_test(NAME itkFEMLinearSystemWrapperItpackTest4
      COMMAND ITK-IntegratedTestTestDriver itkFEMLinearSystemWrapperItpackTest
              3)
add_test(NAME itkFEMLinearSystemWrapperItpackTest5
      COMMAND ITK-IntegratedTestTestDriver itkFEMLinearSystemWrapperItpackTest
              4)
add_test(NAME itkFEMLinearSystemWrapperItpackTest6
      COMMAND ITK-IntegratedTestTestDriver itkFEMLinearSystemWrapperItpackTest
              5)
add_test(NAME itkFEMLinearSystemWrapperItpackTest7
      COMMAND ITK-IntegratedTestTestDriver itkFEMLinearSystemWrapperItpackTest
              6)
add_test(NAME itkFEMLinearSystemWrapperItpackTest8
      COMMAND ITK-IntegratedTestTestDriver itkFEMLinearSystemWrapperItpackTest2
              0)
add_test(NAME itkFEMLinearSystemWrapperItpackTest9
      COMMAND ITK-IntegratedTestTestDriver itkFEMLinearSystemWrapperItpackTest2
              1)
add_test(NAME itkFEMLinearSystemWrapperItpackTest10
      COMMAND ITK-IntegratedTestTestDriver itkFEMLinearSystemWrapperItpackTest2
              2)
add_test(NAME itkFEMLinearSystemWrapperItpackTest11
      COMMAND ITK-IntegratedTestTestDriver itkFEMLinearSystemWrapperItpackTest2
              3)
add_test(NAME itkFEMLinearSystemWrapperItpackTest12
      COMMAND ITK-IntegratedTestTestDriver itkFEMLinearSystemWrapperItpackTest2
              4)
add_test(NAME itkFEMLinearSystemWrapperItpackTest13
      COMMAND ITK-IntegratedTestTestDriver itkFEMLinearSystemWrapperItpackTest2
              5)
add_test(NAME itkFEMLinearSystemWrapperItpackTest14
      COMMAND ITK-IntegratedTestTestDriver itkFEMLinearSystemWrapperItpackTest2
              6)
add_test(NAME itkFEMLinearSystemWrapperVNLTest
      COMMAND ITK-IntegratedTestTestDriver itkFEMLinearSystemWrapperVNLTest)
add_test(NAME itkFEMPArrayTest
      COMMAND ITK-IntegratedTestTestDriver itkFEMPArrayTest)
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
add_test(NAME itkGDCMImageIOTest1
      COMMAND ITK-IntegratedTestTestDriver itkGDCMImageIOTest
              ${ITK_DATA_ROOT}/Input/dicom-sc_cs-1.dcm ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest.dcm ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest.png ${ITK_TEST_OUTPUT_DIR}/itkGDCMRescaleImageIOTest.dcm)
add_test(NAME itkGDCMImageIOTest2
      COMMAND ITK-IntegratedTestTestDriver itkGDCMImageIOTest
              ${ITK_DATA_ROOT}/Input/itkGDCMImageIOTest.dcm ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest2.dcm ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest2.png ${ITK_TEST_OUTPUT_DIR}/itkGDCMRescaleImageIOTest2.dcm)
add_test(NAME itkGDCMImageIOTest3
      COMMAND ITK-IntegratedTestTestDriver itkGDCMImageIOTest
              ${ITK_DATA_ROOT}/Input/012345.002.050 ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest3.dcm ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest3.png ${ITK_TEST_OUTPUT_DIR}/itkGDCMRescaleImageIOTest3.dcm)
add_test(NAME itkGDCMImageIOTest4
      COMMAND ITK-IntegratedTestTestDriver itkGDCMImageIOTest
              ${ITK_DATA_ROOT}/Input/itkGDCMImageIOTest3.dcm ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest4.dcm ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest4.png ${ITK_TEST_OUTPUT_DIR}/itkGDCMRescaleImageIOTest4.dcm)
add_test(NAME itkGDCMImageIOTest5
      COMMAND ITK-IntegratedTestTestDriver itkGDCMImageIOTest2
              ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest5)
add_test(NAME itkGDCMSeriesReadImageWrite
      COMMAND ITK-IntegratedTestTestDriver itkGDCMSeriesReadImageWrite
              ${ITK_DATA_ROOT}/Input/DicomSeries ${ITK_TEST_OUTPUT_DIR}/itkGDCMSeriesReadImageWrite.vtk ${ITK_TEST_OUTPUT_DIR})
add_test(NAME itkGDCMSeriesStreamReadImageWrite1
      COMMAND ITK-IntegratedTestTestDriver itkGDCMSeriesStreamReadImageWrite
              ${ITK_DATA_ROOT}/Input/DicomSeries ${ITK_TEST_OUTPUT_DIR}/itkGDCMSeriesStreamReadImageWrite1.mhd 0.859375 0.85939 1.60016 0)
add_test(NAME itkGDCMSeriesStreamReadImageWrite2
      COMMAND ITK-IntegratedTestTestDriver itkGDCMSeriesStreamReadImageWrite
              ${ITK_DATA_ROOT}/Input/DicomSeries ${ITK_TEST_OUTPUT_DIR}/itkGDCMSeriesStreamReadImageWrite2.mhd 0.859375 0.85939 1.60016 1)
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
add_test(NAME itkHistogramTest
      COMMAND ITK-IntegratedTestTestDriver itkHistogramTest)
add_test(NAME itkHistogramToEntropyImageFilterTest1
      COMMAND ITK-IntegratedTestTestDriver itkHistogramToEntropyImageFilterTest1
              ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkHistogramToEntropyImageFilterTest1.mhd)
add_test(NAME itkHistogramToEntropyImageFilterTest2
      COMMAND ITK-IntegratedTestTestDriver itkHistogramToEntropyImageFilterTest2
              ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png ${ITK_SOURCE_DIR}/Examples/Data/BrainT1Slice.png ${ITK_TEST_OUTPUT_DIR}/itkHistogramToEntropyImageFilterTest2.mhd)
add_test(NAME itkHistogramToIntensityImageFilterTest1
      COMMAND ITK-IntegratedTestTestDriver itkHistogramToIntensityImageFilterTest1
              ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkHistogramToIntensityImageFilterTest1.mhd)
add_test(NAME itkHistogramToIntensityImageFilterTest2
      COMMAND ITK-IntegratedTestTestDriver itkHistogramToIntensityImageFilterTest2
              ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png ${ITK_SOURCE_DIR}/Examples/Data/BrainT1Slice.png ${ITK_TEST_OUTPUT_DIR}/itkHistogramToIntensityImageFilterTest2.mhd)
add_test(NAME itkHistogramToLogProbabilityImageFilterTest1
      COMMAND ITK-IntegratedTestTestDriver itkHistogramToLogProbabilityImageFilterTest1
              ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkHistogramToLogProbabilityImageFilterTest1.mhd)
add_test(NAME itkHistogramToLogProbabilityImageFilterTest2
      COMMAND ITK-IntegratedTestTestDriver itkHistogramToLogProbabilityImageFilterTest2
              ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png ${ITK_SOURCE_DIR}/Examples/Data/BrainT1Slice.png ${ITK_TEST_OUTPUT_DIR}/itkHistogramToLogProbabilityImageFilterTest2.mhd)
add_test(NAME itkHistogramToProbabilityImageFilterTest1
      COMMAND ITK-IntegratedTestTestDriver itkHistogramToProbabilityImageFilterTest1
              ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkHistogramToProbabilityImageFilterTest1.mhd)
add_test(NAME itkHistogramToProbabilityImageFilterTest2
      COMMAND ITK-IntegratedTestTestDriver itkHistogramToProbabilityImageFilterTest2
              ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png ${ITK_SOURCE_DIR}/Examples/Data/BrainT1Slice.png ${ITK_TEST_OUTPUT_DIR}/itkHistogramToProbabilityImageFilterTest2.mhd)
add_test(NAME itkHistogramToTextureFeaturesFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkHistogramToTextureFeaturesFilterTest)
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
add_test(NAME itkImageFileReaderDimensionsTest_MHD
      COMMAND ITK-IntegratedTestTestDriver itkImageFileReaderDimensionsTest
              ${ITK_DATA_ROOT}/Input/HeadMRVolume.mha ${ITK_TEST_OUTPUT_DIR} mha)
add_test(NAME itkImageFileReaderDimensionsTest_NRRD
      COMMAND ITK-IntegratedTestTestDriver itkImageFileReaderDimensionsTest
              ${ITK_DATA_ROOT}/Input/vol-ascii.nrrd ${ITK_TEST_OUTPUT_DIR} nrrd)
add_test(NAME itkImageFileReaderStreamingTest_1
      COMMAND ITK-IntegratedTestTestDriver itkImageFileReaderStreamingTest
              ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd 1 0)
add_test(NAME itkImageFileReaderStreamingTest_2
      COMMAND ITK-IntegratedTestTestDriver itkImageFileReaderStreamingTest
              ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd 0 1)
add_test(NAME itkImageFileReaderStreamingTest_3
      COMMAND ITK-IntegratedTestTestDriver itkImageFileReaderStreamingTest
              ${ITK_DATA_ROOT}/Input/vol-ascii.nrrd 0 0)
add_test(NAME itkImageFileReaderStreamingTest2_MHD
      COMMAND ITK-IntegratedTestTestDriver itkImageFileReaderStreamingTest2
              ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd)
add_test(NAME itkImageFileWriterPastingTest1
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/HeadMRVolume.mhd
              ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterPastingTest1_01.mha
    itkImageFileWriterPastingTest1 ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterPastingTest1_01.mha)
add_test(NAME itkImageFileWriterPastingTest2_5
      COMMAND ITK-IntegratedTestTestDriver itkImageFileWriterPastingTest2
              ${ITK_DATA_ROOT}/Input/HeadMRVolume.mha ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterPastingTest2_5.mha)
add_test(NAME itkImageFileWriterPastingTest2_6
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/HeadMRVolume.mhd
              ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterPastingTest2_6.mha
    itkImageFileWriterPastingTest2 ${ITK_DATA_ROOT}/Input/HeadMRVolume.mha ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterPastingTest2_6.mha ${ITK_DATA_ROOT}/Input/HeadMRVolume.mha)
add_test(NAME itkImageFileWriterPastingTest2_7
      COMMAND ITK-IntegratedTestTestDriver itkImageFileWriterPastingTest2
              ${ITK_DATA_ROOT}/Input/HeadMRVolume.mha ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterPastingTest2_7.mha ${ITK_DATA_ROOT}/Input/HeadMRVolumeCompressed.mha)
add_test(NAME itkImageFileWriterStreamingPastingCompressingTest_MHA
      COMMAND ITK-IntegratedTestTestDriver itkImageFileWriterStreamingPastingCompressingTest1
              ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreamingPastingCompressingTest mha 0 0 0 1 0 0 0 1)
add_test(NAME itkImageFileWriterStreamingPastingCompressingTest_NRRD
      COMMAND ITK-IntegratedTestTestDriver itkImageFileWriterStreamingPastingCompressingTest1
              ${ITK_DATA_ROOT}/Input/vol-ascii.nrrd ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreamingPastingCompressingTest nrrd 0 0 1 1 0 0 1 1)
add_test(NAME itkImageFileWriterStreamingPastingCompressingTest_NHDR
      COMMAND ITK-IntegratedTestTestDriver itkImageFileWriterStreamingPastingCompressingTest1
              ${ITK_DATA_ROOT}/Input/vol-ascii.nrrd ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreamingPastingCompressingTest nhdr 0 0 1 1 0 0 1 1)
add_test(NAME itkImageFileWriterStreamingPastingCompressingTest_VTK
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd
              ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreamingPastingCompressingTest000.vtk
    itkImageFileWriterStreamingPastingCompressingTest1 ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreamingPastingCompressingTest vtk 0 0 1 1 0 0 1 1)
add_test(NAME itkImageFileWriterStreamingPastingCompressingTest_DCM
      COMMAND ITK-IntegratedTestTestDriver itkImageFileWriterStreamingPastingCompressingTest1
              ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreamingPastingCompressingTest dcm 0 0 1 1 0 0 1 1)
add_test(NAME itkImageFileWriterStreamingPastingCompressingTest_IMG
      COMMAND ITK-IntegratedTestTestDriver itkImageFileWriterStreamingPastingCompressingTest1
              ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreamingPastingCompressingTest img 0 0 1 1 0 0 1 1)
add_test(NAME itkImageFileWriterStreamingTest1_1
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/HeadMRVolume.mhd
              ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreaming1_1.mha
    itkImageFileWriterStreamingTest1 ${ITK_DATA_ROOT}/Input/HeadMRVolume.mha ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreaming1_1.mha)
add_test(NAME itkImageFileWriterStreamingTest1_2
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/HeadMRVolume.mhd
              ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreaming1_2.mha
    itkImageFileWriterStreamingTest1 ${ITK_DATA_ROOT}/Input/HeadMRVolume.mha ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreaming1_2.mha ${ITK_DATA_ROOT}/Input/HeadMRVolumeCompressed.mha 0)
add_test(NAME itkImageFileWriterStreamingTest1_3
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/HeadMRVolume.mhd
              ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreaming1_3.mha
    itkImageFileWriterStreamingTest1 ${ITK_DATA_ROOT}/Input/HeadMRVolume.mha ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreaming1_3.mha ${ITK_DATA_ROOT}/Input/HeadMRVolume.mha 1)
add_test(NAME itkImageFileWriterStreamingTest2_4
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/HeadMRVolume.mhd
              ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreaming2_4.mha
    itkImageFileWriterStreamingTest2 ${ITK_DATA_ROOT}/Input/HeadMRVolume.mha ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreaming2_4.mha)
add_test(NAME itkImageFileWriterTest2_1
      COMMAND ITK-IntegratedTestTestDriver itkImageFileWriterTest2
              ${ITK_TEST_OUTPUT_DIR}/test.nrrd)
add_test(NAME itkImageFileWriterTest2_2
      COMMAND ITK-IntegratedTestTestDriver itkImageFileWriterTest2
              ${ITK_TEST_OUTPUT_DIR}/test.mha)
add_test(NAME itkImageFileWriterTest2_3
      COMMAND ITK-IntegratedTestTestDriver itkImageFileWriterTest2
              ${ITK_TEST_OUTPUT_DIR}/test.vtk)
add_test(NAME itkImageFileWriterUpdateLargestPossibleRegionTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Input/cthead1.png
              ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterUpdateLargestPossibleRegionTest.png
    itkImageFileWriterUpdateLargestPossibleRegionTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterUpdateLargestPossibleRegionTest.png)
add_test(NAME itkImageIOBaseTest
      COMMAND ITK-IntegratedTestTestDriver itkImageIOBaseTest)
add_test(NAME itkImageIODirection2DTest01
      COMMAND ITK-IntegratedTestTestDriver itkImageIODirection2DTest
              ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png 1.0 0.0 0.0 1.0 ${ITK_TEST_OUTPUT_DIR}/BrainProtonDensitySliceBorder20.mhd)
add_test(NAME itkImageIODirection2DTest02
      COMMAND ITK-IntegratedTestTestDriver itkImageIODirection2DTest
              ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png 1.0 0.0 0.0 1.0 ${ITK_TEST_OUTPUT_DIR}/BrainProtonDensitySliceShifted13x17y.mhd)
add_test(NAME itkImageIODirection2DTest03
      COMMAND ITK-IntegratedTestTestDriver itkImageIODirection2DTest
              ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20DirectionPlus30.mhd 0.8660254 -0.5 0.5 0.8660254 ${ITK_TEST_OUTPUT_DIR}/BrainProtonDensitySliceBorder20DirectionPlus30.mhd)
add_test(NAME itkImageIODirection2DTest04
      COMMAND ITK-IntegratedTestTestDriver itkImageIODirection2DTest
              ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17yDirectionPlus30.mhd 0.8660254 -0.5 0.5 0.8660254 ${ITK_TEST_OUTPUT_DIR}/BrainProtonDensitySliceShifted13x17yDirectionPlus30.mhd)
add_test(NAME itkImageIODirection2DTest05
      COMMAND ITK-IntegratedTestTestDriver itkImageIODirection2DTest
              ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20DirectionPlus30.mhd 0.8660254 -0.5 0.5 0.8660254 ${ITK_TEST_OUTPUT_DIR}/BrainT1SliceBorder20DirectionPlus30.mhd)
add_test(NAME itkImageIODirection2DTest06
      COMMAND ITK-IntegratedTestTestDriver itkImageIODirection2DTest
              ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.mhd 1.0 0.0 0.0 1.0 ${ITK_TEST_OUTPUT_DIR}/BrainProtonDensitySliceBorder20.nhdr)
add_test(NAME itkImageIODirection2DTest07
      COMMAND ITK-IntegratedTestTestDriver itkImageIODirection2DTest
              ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.mhd 1.0 0.0 0.0 1.0 ${ITK_TEST_OUTPUT_DIR}/BrainProtonDensitySliceShifted13x17y.nhdr)
add_test(NAME itkImageIODirection2DTest08
      COMMAND ITK-IntegratedTestTestDriver itkImageIODirection2DTest
              ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20DirectionPlus30.mhd 0.8660254 -0.5 0.5 0.8660254 ${ITK_TEST_OUTPUT_DIR}/BrainProtonDensitySliceBorder20DirectionPlus30.nhdr)
add_test(NAME itkImageIODirection2DTest09
      COMMAND ITK-IntegratedTestTestDriver itkImageIODirection2DTest
              ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17yDirectionPlus30.mhd 0.8660254 -0.5 0.5 0.8660254 ${ITK_TEST_OUTPUT_DIR}/BrainProtonDensitySliceShifted13x17yDirectionPlus30.nhdr)
add_test(NAME itkImageIODirection2DTest10
      COMMAND ITK-IntegratedTestTestDriver itkImageIODirection2DTest
              ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20DirectionPlus30.mhd 0.8660254 -0.5 0.5 0.8660254 ${ITK_TEST_OUTPUT_DIR}/BrainT1SliceBorder20DirectionPlus30.nhdr)
add_test(NAME itkImageIODirection2DTest11
      COMMAND ITK-IntegratedTestTestDriver itkImageIODirection2DTest
              ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.nhdr 1.0 0.0 0.0 1.0)
add_test(NAME itkImageIODirection2DTest12
      COMMAND ITK-IntegratedTestTestDriver itkImageIODirection2DTest
              ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.nhdr 1.0 0.0 0.0 1.0)
add_test(NAME itkImageIODirection2DTest13
      COMMAND ITK-IntegratedTestTestDriver itkImageIODirection2DTest
              ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20DirectionPlus30.nhdr 0.8660254 -0.5 0.5 0.8660254)
add_test(NAME itkImageIODirection2DTest14
      COMMAND ITK-IntegratedTestTestDriver itkImageIODirection2DTest
              ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17yDirectionPlus30.nhdr 0.8660254 -0.5 0.5 0.8660254)
add_test(NAME itkImageIODirection2DTest15
      COMMAND ITK-IntegratedTestTestDriver itkImageIODirection2DTest
              ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20DirectionPlus30.nhdr 0.8660254 -0.5 0.5 0.8660254)
add_test(NAME itkImageIODirection3DTest01
      COMMAND ITK-IntegratedTestTestDriver itkImageIODirection3DTest
              ${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirectionIdentity.mhd 1.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 1.0 ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeWithDirectionIdentity.mhd)
add_test(NAME itkImageIODirection3DTest02
      COMMAND ITK-IntegratedTestTestDriver itkImageIODirection3DTest
              ${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirection.mhd 0.8660254 0.5 0.0 -0.5 0.8660254 0.0 0.0 0.0 1.0 ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeWithDirection.mhd)
add_test(NAME itkImageIODirection3DTest03
      COMMAND ITK-IntegratedTestTestDriver itkImageIODirection3DTest
              ${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirectionIdentity.mhd 1.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 1.0 ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeWithDirectionIdentity.nhdr)
add_test(NAME itkImageIODirection3DTest04
      COMMAND ITK-IntegratedTestTestDriver itkImageIODirection3DTest
              ${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirection.mhd 0.8660254 0.5 0.0 -0.5 0.8660254 0.0 0.0 0.0 1.0 ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeWithDirection.nhdr)
add_test(NAME itkImageIODirection3DTest05
      COMMAND ITK-IntegratedTestTestDriver itkImageIODirection3DTest
              ${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirectionIdentity.nhdr 1.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 1.0)
add_test(NAME itkImageIODirection3DTest06
      COMMAND ITK-IntegratedTestTestDriver itkImageIODirection3DTest
              ${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirection.nhdr 0.8660254 0.5 0.0 -0.5 0.8660254 0.0 0.0 0.0 1.0)
add_test(NAME itkImageIODirection3DTest07
      COMMAND ITK-IntegratedTestTestDriver itkImageIODirection3DTest
              ${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirection001.mhd 1.0 0.0 0.0 0.0 -1.0 0.0 0.0 0.0 1.0 ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeWithDirection001.nhdr)
add_test(NAME itkImageIODirection3DTest08
      COMMAND ITK-IntegratedTestTestDriver itkImageIODirection3DTest
              ${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirection002.mhd 1.0 0.0 0.0 0.0 0.0 01.0 0.0 -1.0 0.0 ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeWithDirection002.nhdr)
add_test(NAME itkImageIODirection3DTest09
      COMMAND ITK-IntegratedTestTestDriver itkImageIODirection3DTest
              ${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirection003.mhd 0.0 -1.0 0.0 0.0 0.0 1.0 1.0 0.0 0.0 ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeWithDirection003.nhdr)
add_test(NAME itkImageIOFileNameExtensionsTests
      COMMAND ITK-IntegratedTestTestDriver itkImageIOFileNameExtensionsTests)
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
add_test(NAME itkImageTransformTest
      COMMAND ITK-IntegratedTestTestDriver itkImageTransformTest)
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
  target_link_libraries(FileFreeTestLib ITKIO)
  #add_dependencies(IOCxxTests FileFreeTestLib)
endif(ITK_BUILD_SHARED_LIBS)


#add_test(NAME itkIOPrintTest
#      COMMAND ITK-IntegratedTestTestDriver itkIOPrintTest)
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
add_test(NAME itkMetaImageStreamingIOTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/HeadMRVolume.mhd
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeStreamed.mhd
    itkMetaImageStreamingIOTest ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeStreamed.mhd)
add_test(NAME itkMetaImageCompressedStreamingIOTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/HeadMRVolume.mhd
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeCompressedStreamed.mha
    itkMetaImageStreamingIOTest ${ITK_DATA_ROOT}/Input/HeadMRVolumeCompressed.mha ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeCompressedStreamed.mha)
add_test(NAME itkMetaImageStreamingWriterIOTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/HeadMRVolume.mhd
              ${ITK_TEST_OUTPUT_DIR}/MetaImageStreamingWriterIOTest.mha
    itkMetaImageStreamingWriterIOTest ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/MetaImageStreamingWriterIOTest.mha)
add_test(NAME itkMetaImageStreamingWriterIOTest2
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Input/mri3D.mhd
              ${ITK_TEST_OUTPUT_DIR}/mri3DWriteStreamed.mha
    itkMetaImageStreamingWriterIOTest ${ITK_DATA_ROOT}/Input/mri3D.mhd ${ITK_TEST_OUTPUT_DIR}/mri3DWriteStreamed.mha)
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
#add_test(NAME itkNeuralNetworksPrintTest
 #     COMMAND ITK-IntegratedTestTestDriver itkNeuralNetworksPrintTest)
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
add_test(NAME itkNrrdComplexImageReadTest
      COMMAND ITK-IntegratedTestTestDriver itkNrrdComplexImageReadTest
              ${ITK_DATA_ROOT}/Input/mini-complex-slow.nrrd)
add_test(NAME itkNrrdComplexImageReadWriteTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/mini-complex-fast.nrrd
              ${ITK_TEST_OUTPUT_DIR}/mini-complex.nrrd
    itkNrrdComplexImageReadWriteTest ${ITK_DATA_ROOT}/Input/mini-complex-slow.nrrd ${ITK_TEST_OUTPUT_DIR}/mini-complex.nrrd)
add_test(NAME itkNrrdCovariantVectorImageReadTest
      COMMAND ITK-IntegratedTestTestDriver itkNrrdCovariantVectorImageReadTest
              ${ITK_DATA_ROOT}/Input/mini-covector-fast.nrrd)
add_test(NAME itkNrrdCovariantVectorImageReadWriteTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/mini-covector.nrrd
              ${ITK_TEST_OUTPUT_DIR}/mini-covector.nrrd
    itkNrrdCovariantVectorImageReadWriteTest ${ITK_DATA_ROOT}/Input/mini-covector-slow.nrrd ${ITK_TEST_OUTPUT_DIR}/mini-covector.nrrd)
add_test(NAME itkNrrdDiffusionTensor3DImageReadTensorDoubleWriteTensorDouble
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/small-tensors-double.nrrd
              ${ITK_TEST_OUTPUT_DIR}/small-tensors-double.nrrd
    itkNrrdDiffusionTensor3DImageReadTensorDoubleWriteTensorDoubleTest ${ITK_DATA_ROOT}/Input/small-tensors.nhdr ${ITK_TEST_OUTPUT_DIR}/small-tensors-double.nrrd)
add_test(NAME itkNrrdDiffusionTensor3DImageReadTest1
      COMMAND ITK-IntegratedTestTestDriver itkNrrdDiffusionTensor3DImageReadTest
              ${ITK_DATA_ROOT}/Input/mini-ten-mask.nrrd)
add_test(NAME itkNrrdDiffusionTensor3DImageReadTest2
      COMMAND ITK-IntegratedTestTestDriver itkNrrdDiffusionTensor3DImageReadTest
              ${ITK_DATA_ROOT}/Input/mini-ten-nomask-fast.nrrd)
add_test(NAME itkNrrdDiffusionTensor3DImageReadTest3
      COMMAND ITK-IntegratedTestTestDriver itkNrrdDiffusionTensor3DImageReadTest
              ${ITK_DATA_ROOT}/Input/mini-ten-nomask-slow.nrrd)
add_test(NAME itkNrrdDiffusionTensor3DImageReadWriteTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/mini-ten.nrrd
              ${ITK_TEST_OUTPUT_DIR}/mini-ten.nrrd
    itkNrrdDiffusionTensor3DImageReadWriteTest ${ITK_DATA_ROOT}/Input/mini-ten-nomask-fast.nrrd ${ITK_TEST_OUTPUT_DIR}/mini-ten.nrrd)
add_test(NAME itkNrrdDiffusionTensor3DImageReadWriteTest4
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/small-tensors.nhdr
              ${ITK_TEST_OUTPUT_DIR}/small-tensors.nhdr
    itkNrrdDiffusionTensor3DImageReadWriteTest ${ITK_DATA_ROOT}/Input/small-tensors.nhdr ${ITK_TEST_OUTPUT_DIR}/small-tensors.nhdr)
add_test(NAME itkNrrdImageReadWriteTest1
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/NrrdImageReadWriteTest1.nrrd
              ${ITK_TEST_OUTPUT_DIR}/box.nrrd
    itkNrrdImageReadWriteTest ${ITK_DATA_ROOT}/Input/box.nhdr ${ITK_TEST_OUTPUT_DIR}/box.nrrd)
add_test(NAME itkNrrdImageReadWriteTest2
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/vol-ascii.nrrd
              ${ITK_TEST_OUTPUT_DIR}/vol2.nrrd
    itkNrrdImageReadWriteTest ${ITK_DATA_ROOT}/Input/vol-ascii.nrrd ${ITK_TEST_OUTPUT_DIR}/vol2.nrrd)
add_test(NAME itkNrrdImageReadWriteTest3
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/vol-ascii.nrrd
              ${ITK_TEST_OUTPUT_DIR}/vol3.nrrd
    itkNrrdImageReadWriteTest ${ITK_DATA_ROOT}/Input/vol-raw-big.nrrd ${ITK_TEST_OUTPUT_DIR}/vol3.nrrd)
add_test(NAME itkNrrdImageReadWriteTest4
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/vol-ascii.nrrd
              ${ITK_TEST_OUTPUT_DIR}/vol4.nrrd
    itkNrrdImageReadWriteTest ${ITK_DATA_ROOT}/Input/vol-raw-little.nrrd ${ITK_TEST_OUTPUT_DIR}/vol4.nrrd)
add_test(NAME itkNrrdImageReadWriteTest5
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/vol-ascii.nrrd
              ${ITK_TEST_OUTPUT_DIR}/vol5.nrrd
    itkNrrdImageReadWriteTest ${ITK_DATA_ROOT}/Input/vol-gzip-big.nrrd ${ITK_TEST_OUTPUT_DIR}/vol5.nrrd)
add_test(NAME itkNrrdImageReadWriteTest6
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/vol-ascii.nrrd
              ${ITK_TEST_OUTPUT_DIR}/vol6.nrrd
    itkNrrdImageReadWriteTest ${ITK_DATA_ROOT}/Input/vol-gzip-little.nrrd ${ITK_TEST_OUTPUT_DIR}/vol6.nrrd)
add_test(NAME itkNrrdImageReadWriteTest7
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/vol-ascii.nrrd
              ${ITK_TEST_OUTPUT_DIR}/vol7.nrrd
    itkNrrdImageReadWriteTest ${ITK_DATA_ROOT}/Input/vol-ascii.nhdr ${ITK_TEST_OUTPUT_DIR}/vol7.nrrd)
add_test(NAME itkNrrdImageReadWriteTest8
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/vol-ascii.nrrd
              ${ITK_TEST_OUTPUT_DIR}/vol8.nrrd
    itkNrrdImageReadWriteTest ${ITK_DATA_ROOT}/Input/vol-raw-big.nhdr ${ITK_TEST_OUTPUT_DIR}/vol8.nrrd)
add_test(NAME itkNrrdImageReadWriteTest9
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/vol-ascii.nrrd
              ${ITK_TEST_OUTPUT_DIR}/vol9.nrrd
    itkNrrdImageReadWriteTest ${ITK_DATA_ROOT}/Input/vol-raw-little.nhdr ${ITK_TEST_OUTPUT_DIR}/vol9.nrrd)
add_test(NAME itkNrrdImageReadWriteTest10
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/vol-ascii.nrrd
              ${ITK_TEST_OUTPUT_DIR}/vol10.nrrd
    itkNrrdImageReadWriteTest ${ITK_DATA_ROOT}/Input/vol-gzip-big.nhdr ${ITK_TEST_OUTPUT_DIR}/vol10.nrrd)
add_test(NAME itkNrrdImageReadWriteTest11
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/vol-ascii.nrrd
              ${ITK_TEST_OUTPUT_DIR}/vol11.nrrd
    itkNrrdImageReadWriteTest ${ITK_DATA_ROOT}/Input/vol-gzip-little.nhdr ${ITK_TEST_OUTPUT_DIR}/vol11.nrrd)
add_test(NAME itkNrrdRGBAImageReadWriteTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/NrrdRGBAImageReadWriteTest.png
              ${ITK_TEST_OUTPUT_DIR}/NrrdRGBAImageReadWriteTest.png
    itkNrrdRGBAImageReadWriteTest ${ITK_DATA_ROOT}/Input/testrgba.nrrd ${ITK_TEST_OUTPUT_DIR}/NrrdRGBAImageReadWriteTest.png)
add_test(NAME itkNrrdRGBImageReadWriteTest0
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/NrrdRGBImageReadWriteTest0.png
              ${ITK_TEST_OUTPUT_DIR}/NrrdRGBImageReadWriteTest0.png
    itkNrrdRGBImageReadWriteTest ${ITK_DATA_ROOT}/Input/testrgb-0.nhdr ${ITK_TEST_OUTPUT_DIR}/NrrdRGBImageReadWriteTest0.png)
add_test(NAME itkNrrdRGBImageReadWriteTest1
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/NrrdRGBImageReadWriteTest1.png
              ${ITK_TEST_OUTPUT_DIR}/NrrdRGBImageReadWriteTest1.png
    itkNrrdRGBImageReadWriteTest ${ITK_DATA_ROOT}/Input/testrgb-1.nhdr ${ITK_TEST_OUTPUT_DIR}/NrrdRGBImageReadWriteTest1.png)
add_test(NAME itkNrrdRGBImageReadWriteTest2
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/NrrdRGBImageReadWriteTest2.png
              ${ITK_TEST_OUTPUT_DIR}/NrrdRGBImageReadWriteTest2.png
    itkNrrdRGBImageReadWriteTest ${ITK_DATA_ROOT}/Input/testrgb-2.nhdr ${ITK_TEST_OUTPUT_DIR}/NrrdRGBImageReadWriteTest2.png)
add_test(NAME itkNrrdVectorImageReadTest
      COMMAND ITK-IntegratedTestTestDriver itkNrrdVectorImageReadTest
              ${ITK_DATA_ROOT}/Input/mini-vector-fast.nrrd)
add_test(NAME itkNrrdVectorImageReadWriteTest
      COMMAND ITK-IntegratedTestTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/mini-vector.nrrd
              ${ITK_TEST_OUTPUT_DIR}/mini-vector.nrrd
    itkNrrdVectorImageReadWriteTest ${ITK_DATA_ROOT}/Input/mini-vector-slow.nrrd ${ITK_TEST_OUTPUT_DIR}/mini-vector.nrrd)
#add_test(NAME itkNumericsPrintTest
 #     COMMAND ITK-IntegratedTestTestDriver itkNumericsPrintTest)
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
add_test(NAME itkQuadEdgeMeshAddFaceTest1_0
      COMMAND ITK-IntegratedTestTestDriver itkQuadEdgeMeshAddFaceTest1
              0)
add_test(NAME itkQuadEdgeMeshAddFaceTest1_1
      COMMAND ITK-IntegratedTestTestDriver itkQuadEdgeMeshAddFaceTest1
              1)
add_test(NAME itkQuadEdgeMeshAddFaceTest1_2
      COMMAND ITK-IntegratedTestTestDriver itkQuadEdgeMeshAddFaceTest1
              2)
add_test(NAME itkQuadEdgeMeshAddFaceTest1_3
      COMMAND ITK-IntegratedTestTestDriver itkQuadEdgeMeshAddFaceTest1
              3)
add_test(NAME itkQuadEdgeMeshAddFaceTest1_4
      COMMAND ITK-IntegratedTestTestDriver itkQuadEdgeMeshAddFaceTest1
              4)
add_test(NAME itkQuadEdgeMeshAddFaceTest1_5
      COMMAND ITK-IntegratedTestTestDriver itkQuadEdgeMeshAddFaceTest1
              5)
add_test(NAME itkQuadEdgeMeshAddFaceTest2
      COMMAND ITK-IntegratedTestTestDriver itkQuadEdgeMeshAddFaceTest2)
add_test(NAME itkQuadEdgeMeshCellInterfaceTest
      COMMAND ITK-IntegratedTestTestDriver itkQuadEdgeMeshCellInterfaceTest)
add_test(NAME itkQuadEdgeMeshCountingCellsTest
      COMMAND ITK-IntegratedTestTestDriver itkQuadEdgeMeshCountingCellsTest)
add_test(NAME itkQuadEdgeMeshDeleteEdgeTest
      COMMAND ITK-IntegratedTestTestDriver itkQuadEdgeMeshDeleteEdgeTest)
add_test(NAME itkQuadEdgeMeshDeletePointAndReorderIDsTest
      COMMAND ITK-IntegratedTestTestDriver itkQuadEdgeMeshDeletePointAndReorderIDsTest)
add_test(NAME itkQuadEdgeMeshEulerOperatorCreateCenterVertexTest
      COMMAND ITK-IntegratedTestTestDriver itkQuadEdgeMeshEulerOperatorCreateCenterVertexTest)
add_test(NAME itkQuadEdgeMeshEulerOperatorDeleteCenterVertexTest
      COMMAND ITK-IntegratedTestTestDriver itkQuadEdgeMeshEulerOperatorDeleteCenterVertexTest)
add_test(NAME itkQuadEdgeMeshEulerOperatorFlipTest
      COMMAND ITK-IntegratedTestTestDriver itkQuadEdgeMeshEulerOperatorFlipTest)
add_test(NAME itkQuadEdgeMeshEulerOperatorJoinFacetTest
      COMMAND ITK-IntegratedTestTestDriver itkQuadEdgeMeshEulerOperatorJoinFacetTest)
add_test(NAME itkQuadEdgeMeshEulerOperatorJoinVertexTest0
      COMMAND ITK-IntegratedTestTestDriver itkQuadEdgeMeshEulerOperatorJoinVertexTest
              0)
add_test(NAME itkQuadEdgeMeshEulerOperatorJoinVertexTest1
      COMMAND ITK-IntegratedTestTestDriver itkQuadEdgeMeshEulerOperatorJoinVertexTest
              1)
add_test(NAME itkQuadEdgeMeshEulerOperatorJoinVertexTest2
      COMMAND ITK-IntegratedTestTestDriver itkQuadEdgeMeshEulerOperatorJoinVertexTest
              2)
add_test(NAME itkQuadEdgeMeshEulerOperatorJoinVertexTest3
      COMMAND ITK-IntegratedTestTestDriver itkQuadEdgeMeshEulerOperatorJoinVertexTest
              3)
add_test(NAME itkQuadEdgeMeshEulerOperatorJoinVertexTest4
      COMMAND ITK-IntegratedTestTestDriver itkQuadEdgeMeshEulerOperatorJoinVertexTest
              4)
add_test(NAME itkQuadEdgeMeshEulerOperatorSplitEdgeTest
      COMMAND ITK-IntegratedTestTestDriver itkQuadEdgeMeshEulerOperatorSplitEdgeTest)
add_test(NAME itkQuadEdgeMeshEulerOperatorSplitVertexTest
      COMMAND ITK-IntegratedTestTestDriver itkQuadEdgeMeshEulerOperatorSplitVertexTest)
add_test(NAME itkQuadEdgeMeshIteratorTest
      COMMAND ITK-IntegratedTestTestDriver itkQuadEdgeMeshIteratorTest)
add_test(NAME itkQuadEdgeMeshNoPointConstTest
      COMMAND ITK-IntegratedTestTestDriver itkQuadEdgeMeshNoPointConstTest)
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
#add_test(NAME itkSparseFieldFourthOrderLevelSetImageFilterTest
#      COMMAND ITK-IntegratedTestTestDriver itkSparseFieldFourthOrderLevelSetImageFilterTest)
add_test(NAME itkSparseFrequencyContainer2Test
      COMMAND ITK-IntegratedTestTestDriver itkSparseFrequencyContainer2Test)
add_test(NAME itkSparseImageTest
      COMMAND ITK-IntegratedTestTestDriver itkSparseImageTest)
#add_test(NAME itkSpatialObjectPrintTest
 #     COMMAND ITK-IntegratedTestTestDriver itkSpatialObjectPrintTest)
add_test(NAME itkSpatialObjectToImageRegistrationTest
      COMMAND ITK-IntegratedTestTestDriver itkSpatialObjectToImageRegistrationTest)
add_test(NAME itkSplineKernelTransformTest
      COMMAND ITK-IntegratedTestTestDriver itkSplineKernelTransformTest)
add_test(NAME itkStandardDeviationPerComponentSampleFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkStandardDeviationPerComponentSampleFilterTest)
add_test(NAME itkStatisticsAlgorithmTest
      COMMAND ITK-IntegratedTestTestDriver itkStatisticsAlgorithmTest)
add_test(NAME itkStatisticsAlgorithmTest2
      COMMAND ITK-IntegratedTestTestDriver itkStatisticsAlgorithmTest2)
#add_test(NAME itkStatisticsPrintTest
#      COMMAND ITK-IntegratedTestTestDriver itkStatisticsPrintTest)
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
add_test(NAME itkTransformFileReaderWriterTest0
      COMMAND ITK-IntegratedTestTestDriver itkTransformFileReaderWriterTest
              ${ITK_DATA_ROOT}/Input/itkAffineTransform00.txt ${ITK_TEST_OUTPUT_DIR}/itkTransformFileReaderWriterTest0.txt)
add_test(NAME itkTransformFileReaderWriterTest1
      COMMAND ITK-IntegratedTestTestDriver itkTransformFileReaderWriterTest
              ${ITK_DATA_ROOT}/Input/itkAffineTransform01.txt ${ITK_TEST_OUTPUT_DIR}/itkTransformFileReaderWriterTest1.txt)
add_test(NAME itkTransformFileReaderWriterTest2
      COMMAND ITK-IntegratedTestTestDriver itkTransformFileReaderWriterTest
              ${ITK_DATA_ROOT}/Input/itkAffineTransform01.txt ${ITK_TEST_OUTPUT_DIR}/itkTransformFileReaderWriterTest1.mat)
add_test(NAME itkTransformFileReaderWriterTest3
      COMMAND ITK-IntegratedTestTestDriver itkTransformFileReaderWriterTest
              ${ITK_DATA_ROOT}/Input/itkBSplineTransform00.txt ${ITK_TEST_OUTPUT_DIR}/itkBSplineTransform00.mat)
add_test(NAME itkTransformIOTest
      COMMAND ITK-IntegratedTestTestDriver itkTransformIOTest
              ${ITK_TEST_OUTPUT_DIR})
add_test(NAME itkTransformsSetParametersTest
      COMMAND ITK-IntegratedTestTestDriver itkTransformsSetParametersTest)
add_test(NAME itkTriangleMeshToSimplexMeshFilter2Test
      COMMAND ITK-IntegratedTestTestDriver itkTriangleMeshToSimplexMeshFilter2Test)
#add_test(NAME itkUnicodeIOTest
#      COMMAND ITK-IntegratedTestTestDriver itkUnicodeIOTest)
add_test(NAME itkUnsharpMaskLevelSetImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkUnsharpMaskLevelSetImageFilterTest)
add_test(NAME itkVectorAnisotropicDiffusionImageFilterTest
      COMMAND ITK-IntegratedTestTestDriver itkVectorAnisotropicDiffusionImageFilterTest)
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
add_test(NAME itkVersorRigid3DTransformTest
      COMMAND ITK-IntegratedTestTestDriver itkVersorRigid3DTransformTest)
add_test(NAME itkVersorTransformTest
      COMMAND ITK-IntegratedTestTestDriver itkVersorTransformTest)
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
add_test(NAME testMetaArray
      COMMAND ITK-IntegratedTestTestDriver testMetaArray
              ${ITK_TEST_OUTPUT_DIR})
add_test(NAME testMetaBlob
      COMMAND ITK-IntegratedTestTestDriver testMetaBlob
              ${ITK_TEST_OUTPUT_DIR})
add_test(NAME MetaCommandRequiredOptionsWithMultipleStrings
      COMMAND ITK-IntegratedTestTestDriver testMetaCommand
              -ExpectedFailStatus 0 -sum 7 -r1c_rs StringValue1 StringValue2 StringValue3 -r2_ri 7)
add_test(NAME MetaCommandRequiredOptionsWithMultipleStringsAtEnd
      COMMAND ITK-IntegratedTestTestDriver testMetaCommand
              -ExpectedFailStatus 0 -sum 7 -r2_ri 7 -r1c_rs StringValue1 StringValue2 StringValue3)
add_test(NAME MetaCommandOptionalWithRequired
      COMMAND ITK-IntegratedTestTestDriver testMetaCommand
              -ExpectedFailStatus 0 -sum 107 -r1c_rs StringValue1 StringValue2 StringValue3 -r2_ri 7 -o1_ri 1 -o2_ri 99)
add_test(NAME MetaCommandOptionalOptoinsWithOutArguments
      COMMAND ITK-IntegratedTestTestDriver testMetaCommand
              -ExpectedFailStatus 0 -sum 207 -r1c_rs StringValue1 StringValue2 StringValue3 -r2_ri 7 -o1_oi -o2_oi)
add_test(NAME MetaCommandOptionalOptoinsWithOutArguments1
      COMMAND ITK-IntegratedTestTestDriver testMetaCommand
              -ExpectedFailStatus 0 -sum 107 -r1c_rs StringValue1 StringValue2 StringValue3 -r2_ri 7 -o1_oi 1 -o2_oi 99)
add_test(NAME MetaCommandRequiredAndOptionalOptoinsWithArguments
      COMMAND ITK-IntegratedTestTestDriver testMetaCommand
              -ExpectedFailStatus 0 -sum 207 -r1c_rs StringValue1 StringValue2 StringValue3 -r2_ri 7 -o1_ri 1 -o2_ri 99 -o3_ri_oi 50 50)
add_test(NAME MetaCommandRequiredAndOptionalOptoinsWithOutArguments
      COMMAND ITK-IntegratedTestTestDriver testMetaCommand
              -ExpectedFailStatus 0 -sum 257 -r1c_rs StringValue1 StringValue2 StringValue3 -r2_ri 7 -o1_ri 1 -o2_ri 99 -o3_ri_oi 50)
add_test(NAME MetaCommandRequiredAndOptionalOptoinsWithOutArguments-1
      COMMAND ITK-IntegratedTestTestDriver testMetaCommand
              -ExpectedFailStatus 0 -sum 457 -r1c_rs StringValue1 StringValue2 StringValue3 -r2_ri 7 -o1_ri 1 -o2_ri 99 -o3_ri_oi 50 -o1_oi -o2_oi)
add_test(NAME MetaCommandRequiredOptionsWithMissingMultipleStrings
      COMMAND ITK-IntegratedTestTestDriver testMetaCommand
              -ExpectedFailStatus 1 -sum 7 -r1c_rs -r2_ri 7)
add_test(NAME MetaCommandRequiredOptionsWithMissingMultipleStringsAtEnd
      COMMAND ITK-IntegratedTestTestDriver testMetaCommand
              -ExpectedFailStatus 1 -sum 7 -r2_ri 7 -r1c_rs)
add_test(NAME MetaCommandOptionalWithRequiredMissing
      COMMAND ITK-IntegratedTestTestDriver testMetaCommand
              -ExpectedFailStatus 1 -sum 107 -r1c_rs StringValue1 StringValue2 StringValue3 -r2_ri -o1_ri 1 -o2_ri 99)
add_test(NAME MetaCommandRequiredAndOptionalOptoinsWithArgumentsMissing
      COMMAND ITK-IntegratedTestTestDriver testMetaCommand
              -ExpectedFailStatus 1 -sum 207 -r1c_rs StringValue1 StringValue2 StringValue3 -r2_ri 7 -o3_ri_oi)
add_test(NAME testMetaGroup
      COMMAND ITK-IntegratedTestTestDriver testMetaGroup
              ${ITK_TEST_OUTPUT_DIR})
add_test(NAME testMetaImage
      COMMAND ITK-IntegratedTestTestDriver testMetaImage)
add_test(NAME testMetaLandmark
      COMMAND ITK-IntegratedTestTestDriver testMetaLandmark
              ${ITK_TEST_OUTPUT_DIR})
add_test(NAME testMetaLine
      COMMAND ITK-IntegratedTestTestDriver testMetaLine
              ${ITK_TEST_OUTPUT_DIR})
add_test(NAME testMetaMesh
      COMMAND ITK-IntegratedTestTestDriver testMetaMesh
              ${ITK_TEST_OUTPUT_DIR})
add_test(NAME testMetaObject
      COMMAND ITK-IntegratedTestTestDriver testMetaObject
              ${ITK_TEST_OUTPUT_DIR})
add_test(NAME testMetaScene
      COMMAND ITK-IntegratedTestTestDriver testMetaScene
              ${ITK_TEST_OUTPUT_DIR})
add_test(NAME testMetaSurface
      COMMAND ITK-IntegratedTestTestDriver testMetaSurface
              ${ITK_TEST_OUTPUT_DIR})
add_test(NAME testMetaTube
      COMMAND ITK-IntegratedTestTestDriver testMetaTube
              ${ITK_TEST_OUTPUT_DIR})
add_test(NAME testMetaUtils
      COMMAND ITK-IntegratedTestTestDriver testMetaUtils
              ${ITK_TEST_OUTPUT_DIR})
add_test(NAME XORTest1
      COMMAND ITK-IntegratedTestTestDriver XORTest1
              ${ITK_DATA_ROOT}/Input/NeuralNetworks/xortest.txt)
add_test(NAME XORTest2
      COMMAND ITK-IntegratedTestTestDriver XORTest2
              ${ITK_DATA_ROOT}/Input/NeuralNetworks/xortest.txt)
