itk_module_test()
set(ITKReviewTests
itkAddConstantToImageFilterTest.cxx
itkAggregateLabelMapFilterTest.cxx
itkAreaClosingImageFilterTest.cxx
itkAreaOpeningImageFilterTest.cxx
itkAttributeKeepNObjectsLabelMapFilterTest1.cxx
itkAttributeLabelObjectAccessorsTest1.cxx
itkAttributeOpeningLabelMapFilterTest1.cxx
itkAttributePositionLabelMapFilterTest1.cxx
itkAttributeRelabelLabelMapFilterTest1.cxx
itkAttributeUniqueLabelMapFilterTest1.cxx
itkAutoCropLabelMapFilterTest1.cxx
itkBinaryClosingByReconstructionImageFilterTest.cxx
itkBinaryFillholeImageFilterTest1.cxx
itkBinaryGrindPeakImageFilterTest1.cxx
itkBinaryImageToLabelMapFilterTest.cxx
itkBinaryImageToShapeLabelMapFilterTest1.cxx
itkBinaryImageToStatisticsLabelMapFilterTest1.cxx
itkBinaryOpeningByReconstructionImageFilterTest.cxx
itkBinaryReconstructionByDilationImageFilterTest.cxx
itkBinaryReconstructionByErosionImageFilterTest.cxx
itkBinaryReconstructionLabelMapFilterTest.cxx
itkBinaryShapeKeepNObjectsImageFilterTest1.cxx
itkBinaryShapeOpeningImageFilterTest1.cxx
itkBinaryStatisticsKeepNObjectsImageFilterTest1.cxx
itkBinaryStatisticsOpeningImageFilterTest1.cxx
itkBoxMeanImageFilterTest.cxx
itkBoxSigmaImageFilterTest.cxx
itkBruker2DSEQImageIOTest.cxx
itkBSplineControlPointImageFilterTest.cxx
itkBSplineControlPointImageFunctionTest.cxx
itkBSplineDeformableTransformInitializerTest1.cxx
itkBSplineDeformableTransformInitializerTest2.cxx
itkBSplineScatteredDataPointSetToImageFilterTest.cxx
itkBSplineScatteredDataPointSetToImageFilterTest2.cxx
itkBSplineScatteredDataPointSetToImageFilterTest3.cxx
itkChangeLabelLabelMapFilterTest.cxx
itkChangeRegionLabelMapFilterTest1.cxx
itkCompositeTransformTest.cxx
itkConformalFlatteningMeshFilterTest.cxx
itkConformalFlatteningQuadEdgeMeshFilterTest.cxx
itkContourExtractor2DImageFilterTest.cxx
itkConvertLabelMapFilterTest1.cxx
itkConvolutionImageFilterTest.cxx
itkConvolutionImageFilterTestInt.cxx
itkCoxDeBoorBSplineKernelFunctionTest.cxx
itkCoxDeBoorBSplineKernelFunctionTest2.cxx
itkCropLabelMapFilterTest1.cxx
itkDeformationFieldTransformTest.cxx
itkDiffeomorphicDemonsRegistrationFilterTest.cxx
itkDiffeomorphicDemonsRegistrationFilterTest2.cxx
itkDirectFourierReconstructionImageToImageFilterTest.cxx
itkDiscreteGaussianDerivativeImageFilterScaleSpaceTest.cxx
itkDiscreteGaussianDerivativeImageFilterTest.cxx
itkDiscreteGaussianDerivativeImageFunctionTest.cxx
itkDiscreteGradientMagnitudeGaussianImageFunctionTest.cxx
itkDiscreteHessianGaussianImageFunctionTest.cxx
itkDivideByConstantImageFilterTest.cxx
itkExponentialDeformationFieldImageFilterTest.cxx
itkFastApproximateRankImageFilterTest.cxx
itkFastSymmetricForcesDemonsRegistrationFilterTest.cxx
itkFFTShiftImageFilterTest.cxx
itkGaborImageSourceTest.cxx
itkGaussianDerivativeOperatorTest.cxx
itkGridForwardWarpImageFilterTest.cxx
itkGridImageSourceTest.cxx
itkHessianToObjectnessMeasureImageFilterTest.cxx
itkImageReadComplexWriteMagnitudeAndPhaseTest.cxx
itkImageReadMagnitudeAndPhaseWriteComplexTest.cxx
itkImageFunctionTest.cxx
itkJPEG2000ImageIOFactoryTest01.cxx
itkJPEG2000ImageIORegionOfInterest.cxx
itkJPEG2000ImageIOTest00.cxx
itkJPEG2000ImageIOTest01.cxx
itkJPEG2000ImageIOTest02.cxx
itkJPEG2000ImageIOTest03.cxx
itkJPEG2000ImageIOTest04.cxx
itkJPEG2000ImageIOTest05.cxx
itkJPEG2000ImageIOTest06.cxx
itkKappaSigmaThresholdImageCalculatorTest.cxx
itkKappaSigmaThresholdImageFilterTest.cxx
itkLabelGeometryImageFilterTest.cxx
itkLabelImageToLabelMapFilterTest.cxx
itkLabelImageToShapeLabelMapFilterTest1.cxx
itkLabelImageToStatisticsLabelMapFilterTest1.cxx
itkLabelMapContourOverlayImageFilterTest1.cxx
itkLabelMapFilterTest.cxx
itkLabelMapMaskImageFilterTest.cxx
itkLabelMapOverlayImageFilterTest1.cxx
itkLabelMapOverlayImageFilterTest2.cxx
itkLabelMapTest.cxx
itkLabelMapToAttributeImageFilterTest1.cxx
itkLabelMapToBinaryImageFilterTest.cxx
itkLabelMapToLabelImageFilterTest.cxx
itkLabelMapToRGBImageFilterTest1.cxx
itkLabelMapToRGBImageFilterTest2.cxx
itkLabelObjectLineComparatorTest.cxx
itkLabelObjectLineTest.cxx
itkLabelObjectTest.cxx
itkLabelOverlayImageFilterTest.cxx
itkLabelSelectionLabelMapFilterTest.cxx
itkLabelShapeKeepNObjectsImageFilterTest1.cxx
itkLabelShapeOpeningImageFilterTest1.cxx
itkLabelStatisticsKeepNObjectsImageFilterTest1.cxx
itkLabelStatisticsOpeningImageFilterTest1.cxx
itkLabelToRGBImageFilterTest.cxx
itkLabelUniqueLabelMapFilterTest1.cxx
itkMapMaskedRankImageFilterTest.cxx
itkMapOptGrayscaleDilateImageFilterTest.cxx
itkMapOptGrayscaleErodeImageFilterTest.cxx
itkMapOptGrayscaleMorphologicalClosingImageFilterTest.cxx
itkMapOptGrayscaleMorphologicalOpeningImageFilterTest.cxx
itkMapRankImageFilterTest.cxx
itkMaskedRankImageFilterTest.cxx
itkMergeLabelMapFilterTest1.cxx
itkMorphologicalWatershedFromMarkersImageFilterTest.cxx
itkMorphologicalWatershedImageFilterTest.cxx
itkMRCImageIOTest.cxx
itkMultiphaseDenseFiniteDifferenceImageFilterTest.cxx
itkMultiphaseFiniteDifferenceImageFilterTest.cxx
itkMultiphaseSparseFiniteDifferenceImageFilterTest.cxx
itkMultiplyByConstantImageFilterTest.cxx
itkMultiScaleHessianBasedMeasureImageFilterTest.cxx
itkNeuralNetworkIOTest.cxx
itkN4MRIBiasFieldCorrectionImageFilterTest
itkObjectByObjectLabelMapFilterTest.cxx
itkObjectToObjectMetricTest.cxx
itkOptGrayscaleDilateImageFilterTest.cxx
itkOptGrayscaleErodeImageFilterTest.cxx
itkOptGrayscaleMorphologicalClosingImageFilterTest.cxx
itkOptGrayscaleMorphologicalOpeningImageFilterTest.cxx
itkOptImageToImageMetricsTest.cxx
itkOptImageToImageMetricsTest2.cxx
itkOptMattesMutualInformationImageToImageMetricThreadsTest1.cxx
itkPadLabelMapFilterTest1.cxx
itkPointsLocatorTest.cxx
itkRankImageFilterTest.cxx
itkRegionalMaximaImageFilterTest.cxx
itkRegionalMaximaImageFilterTest2.cxx
itkRegionalMinimaImageFilterTest.cxx
itkRegionalMinimaImageFilterTest2.cxx
itkRegionBasedLevelSetFunctionTest.cxx
itkRegionFromReferenceLabelMapFilterTest1.cxx
itkRelabelLabelMapFilterTest1.cxx
itkRobustAutomaticThresholdImageFilterTest.cxx
itkScalarChanAndVeseDenseLevelSetImageFilterTest1.cxx
itkScalarChanAndVeseDenseLevelSetImageFilterTest2.cxx
itkScalarChanAndVeseDenseLevelSetImageFilterTest3.cxx
itkScalarChanAndVeseDenseLevelSetImageFilterTest4.cxx
itkScalarChanAndVeseLevelSetFunctionTest1.cxx
itkScalarChanAndVeseLevelSetFunctionTest2.cxx
itkScalarChanAndVeseSparseLevelSetImageFilterTest1.cxx
itkScalarChanAndVeseSparseLevelSetImageFilterTest2.cxx
itkScalarRegionBasedLevelSetFunctionTest.cxx
itkScalarToRGBColormapImageFilterTest.cxx
itkShapedFloodFilledImageFunctionConditionalConstIteratorTest1.cxx
itkShapedFloodFilledImageFunctionConditionalConstIteratorTest2.cxx
itkShapedFloodFilledImageFunctionConditionalConstIteratorTest3.cxx
itkShapeKeepNObjectsLabelMapFilterTest1.cxx
itkShapeLabelObjectAccessorsTest1.cxx
itkShapeOpeningLabelMapFilterTest1.cxx
itkShapePositionLabelMapFilterTest1.cxx
itkShapeRelabelImageFilterTest1.cxx
itkShapeRelabelLabelMapFilterTest1.cxx
itkShapeUniqueLabelMapFilterTest1.cxx
itkShiftLabelObjectTest.cxx
itkShiftScaleLabelMapFilterTest1.cxx
itkSliceBySliceImageFilterTest.cxx
itkStatisticsKeepNObjectsLabelMapFilterTest1.cxx
itkStatisticsOpeningLabelMapFilterTest1.cxx
itkStatisticsPositionLabelMapFilterTest1.cxx
itkStatisticsRelabelImageFilterTest1.cxx
itkStatisticsRelabelLabelMapFilterTest1.cxx
itkStatisticsUniqueLabelMapFilterTest1.cxx
itkStochasticFractalDimensionImageFilterTest.cxx
itkSubtractConstantFromImageFilterTest.cxx
itkTimeAndMemoryProbeTest.cxx
itkTransformToDeformationFieldSourceTest.cxx
itkTransformToDeformationFieldSourceTest1.cxx
itkValuedRegionalMaximaImageFilterTest.cxx
itkValuedRegionalMinimaImageFilterTest.cxx
itkVectorCentralDifferenceImageFunctionTest.cxx
itkVectorLinearInterpolateNearestNeighborExtrapolateImageFunctionTest.cxx
itkVoxBoCUBImageIOTest.cxx
itkWarpHarmonicEnergyCalculatorTest.cxx
itkWarpJacobianDeterminantFilterTest.cxx
itkCompositeTransformWriterTest.cxx
itkCompositeTransformReaderTest.cxx
itkCompositeTransformWriterAndReaderTest.cxx
itkReviewHeaderTest.cxx
)

set(BASELINE ${ITK_DATA_ROOT}/Baseline/Review)
set(INPUTDATA ${ITK_DATA_ROOT}/Input)
set(BASELINE_ROOT ${ITK_DATA_ROOT}/Baseline)
set(TEMP ${ITK_TEST_OUTPUT_DIR})


if(ITK_USE_MINC2 AND MINC2_FOUND)
  set(ITKReviewTests ${ITKReviewTests} itkMINC2ImageIOTest.cxx)
endif(ITK_USE_MINC2 AND MINC2_FOUND)


if( USE_FFTWD OR USE_FFTWF )
  set( ITKReviewTests
    ${ITKReviewTests}
    itkFFTComplexToComplexImageFilterTest01.cxx
    itkFFTComplexToComplexImageFilterTest02.cxx
    )
endif( USE_FFTWD OR USE_FFTWF )


CreateTestDriver_SupportBuildInIOFactories(ITKReview  "${ITKReview-Test_LIBRARIES}" "${ITKReviewTests}")

itk_add_test(NAME itkShiftLabelObjectTest
      COMMAND ITKReviewTestDriver itkShiftLabelObjectTest)
itk_add_test(NAME itkReviewHeaderTest
      COMMAND ITKReviewTestDriver itkReviewHeaderTest)
itk_add_test(NAME itkAddConstantToImageFilterTest
      COMMAND ITKReviewTestDriver itkAddConstantToImageFilterTest)
itk_add_test(NAME itkAggregateLabelMapFilterTest1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1-labelAggregate.mha
              ${ITK_TEST_OUTPUT_DIR}/cthead1-labelAggregate.mha
    itkAggregateLabelMapFilterTest ${ITK_DATA_ROOT}/Input/cthead1Label.png ${ITK_TEST_OUTPUT_DIR}/cthead1-labelAggregate.mha)
itk_add_test(NAME itkAreaClosingImageFilterTest0
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkAreaClosingImageFilterTest0.png
              ${ITK_TEST_OUTPUT_DIR}/itkAreaClosingImageFilterTest0.png
    itkAreaClosingImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkAreaClosingImageFilterTest0.png 1000 0 0)
itk_add_test(NAME itkAreaOpeningImageFilterTest0
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkAreaOpeningImageFilterTest0.png
              ${ITK_TEST_OUTPUT_DIR}/itkAreaOpeningImageFilterTest0.png
    itkAreaOpeningImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkAreaOpeningImageFilterTest0.png 1000 0 1)
itk_add_test(NAME itkAttributeKeepNObjectsLabelMapFilterTest0
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkAttributeKeepNObjectsLabelMapFilterTest0.png
              ${ITK_TEST_OUTPUT_DIR}/itkAttributeKeepNObjectsLabelMapFilterTest0.png
    itkAttributeKeepNObjectsLabelMapFilterTest1 ${ITK_DATA_ROOT}/Input/cthead1Label.png ${ITK_TEST_OUTPUT_DIR}/itkAttributeKeepNObjectsLabelMapFilterTest0.png 2 0)
itk_add_test(NAME itkAttributeKeepNObjectsLabelMapFilterTest1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkAttributeKeepNObjectsLabelMapFilterTest1.png
              ${ITK_TEST_OUTPUT_DIR}/itkAttributeKeepNObjectsLabelMapFilterTest1.png
    itkAttributeKeepNObjectsLabelMapFilterTest1 ${ITK_DATA_ROOT}/Input/cthead1Label.png ${ITK_TEST_OUTPUT_DIR}/itkAttributeKeepNObjectsLabelMapFilterTest1.png 2 1)
itk_add_test(NAME itkAttributeLabelObjectAccessorsTest1
      COMMAND ITKReviewTestDriver itkAttributeLabelObjectAccessorsTest1
              ${ITK_DATA_ROOT}/Input/cthead1Label.png ${ITK_DATA_ROOT}/Input/cthead1.png)
itk_add_test(NAME itkAttributeOpeningLabelMapFilterTest0
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkAttributeOpeningLabelMapFilterTest0.png
              ${ITK_TEST_OUTPUT_DIR}/itkAttributeOpeningLabelMapFilterTest0.png
    itkAttributeOpeningLabelMapFilterTest1 ${ITK_DATA_ROOT}/Input/cthead1Label.png ${ITK_TEST_OUTPUT_DIR}/itkAttributeOpeningLabelMapFilterTest0.png 2 0)
itk_add_test(NAME itkAttributeOpeningLabelMapFilterTest1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkAttributeOpeningLabelMapFilterTest1.png
              ${ITK_TEST_OUTPUT_DIR}/itkAttributeOpeningLabelMapFilterTest1.png
    itkAttributeOpeningLabelMapFilterTest1 ${ITK_DATA_ROOT}/Input/cthead1Label.png ${ITK_TEST_OUTPUT_DIR}/itkAttributeOpeningLabelMapFilterTest1.png 2 1)
itk_add_test(NAME itkAttributePositionLabelMapFilterTest1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkAttributePositionLabelMapFilterTest1.png
              ${ITK_TEST_OUTPUT_DIR}/itkAttributePositionLabelMapFilterTest1.png
    itkAttributePositionLabelMapFilterTest1 ${ITK_DATA_ROOT}/Input/SpotsLabeled.png ${ITK_TEST_OUTPUT_DIR}/itkAttributePositionLabelMapFilterTest1.png)
itk_add_test(NAME itkAttributeRelabelLabelMapFilterTest0
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkAttributeRelabelLabelMapFilterTest0.png
              ${ITK_TEST_OUTPUT_DIR}/itkAttributeRelabelLabelMapFilterTest0.png
    itkAttributeRelabelLabelMapFilterTest1 ${ITK_DATA_ROOT}/Input/cthead1Label.png ${ITK_TEST_OUTPUT_DIR}/itkAttributeRelabelLabelMapFilterTest0.png 0)
itk_add_test(NAME itkAttributeRelabelLabelMapFilterTest1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkAttributeRelabelLabelMapFilterTest1.png
              ${ITK_TEST_OUTPUT_DIR}/itkAttributeRelabelLabelMapFilterTest1.png
    itkAttributeRelabelLabelMapFilterTest1 ${ITK_DATA_ROOT}/Input/cthead1Label.png ${ITK_TEST_OUTPUT_DIR}/itkAttributeRelabelLabelMapFilterTest1.png 1)
itk_add_test(NAME itkAttributeUniqueLabelMapFilterTest0
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkAttributeUniqueLabelMapFilterTest0.png
              ${ITK_TEST_OUTPUT_DIR}/itkAttributeUniqueLabelMapFilterTest0.png
    itkAttributeUniqueLabelMapFilterTest1 ${ITK_DATA_ROOT}/Input/SpotsLabeled.png ${ITK_TEST_OUTPUT_DIR}/itkAttributeUniqueLabelMapFilterTest0.png 0)
itk_add_test(NAME itkAttributeUniqueLabelMapFilterTest1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkAttributeUniqueLabelMapFilterTest1.png
              ${ITK_TEST_OUTPUT_DIR}/itkAttributeUniqueLabelMapFilterTest1.png
    itkAttributeUniqueLabelMapFilterTest1 ${ITK_DATA_ROOT}/Input/SpotsLabeled.png ${ITK_TEST_OUTPUT_DIR}/itkAttributeUniqueLabelMapFilterTest1.png 1)
itk_add_test(NAME itkAutoCropLabelMapFilterTest1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1-label-autocrop.mha
              ${ITK_TEST_OUTPUT_DIR}/cthead1-label-autocrop.mha
    itkAutoCropLabelMapFilterTest1 ${ITK_DATA_ROOT}/Input/cthead1Label.png ${ITK_TEST_OUTPUT_DIR}/cthead1-label-autocrop.mha 176 4 4)
itk_add_test(NAME itkBinaryClosingByReconstructionImageFilterTest
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkBinaryClosingByReconstructionImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryClosingByReconstructionImageFilterTest.png
    itkBinaryClosingByReconstructionImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1-bin.png ${ITK_TEST_OUTPUT_DIR}/itkBinaryClosingByReconstructionImageFilterTest.png 0 255 10)
itk_add_test(NAME itkBinaryFillholeImageFilterTest1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkBinaryFillholeImageFilterTest1.png
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryFillholeImageFilterTest1.png
    itkBinaryFillholeImageFilterTest1 ${ITK_DATA_ROOT}/Input/Spots.png ${ITK_TEST_OUTPUT_DIR}/itkBinaryFillholeImageFilterTest1.png 1 255)
itk_add_test(NAME itkBinaryGrindPeakImageFilterTest1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkBinaryGrindPeakImageFilterTest1.png
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryGrindPeakImageFilterTest1.png
    itkBinaryGrindPeakImageFilterTest1 ${ITK_DATA_ROOT}/Input/SpotsInverted.png ${ITK_TEST_OUTPUT_DIR}/itkBinaryGrindPeakImageFilterTest1.png 1 255)
itk_add_test(NAME itkBinaryImageToLabelMapFilterTest1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1-labeled.mha
              ${ITK_TEST_OUTPUT_DIR}/cthead1-labeled.mha
    itkBinaryImageToLabelMapFilterTest ${ITK_DATA_ROOT}/Input/cthead1-mask.png ${ITK_TEST_OUTPUT_DIR}/cthead1-labeled.mha 1 255 0 0)
itk_add_test(NAME itkBinaryImageToLabelMapFilterTest2
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/connected0lines-0.png
              ${ITK_TEST_OUTPUT_DIR}/connected0lines-0.png
    itkBinaryImageToLabelMapFilterTest ${ITK_DATA_ROOT}/Input/connected0lines.png ${ITK_TEST_OUTPUT_DIR}/connected0lines-0.png 0 255 0 0)
itk_add_test(NAME itkBinaryImageToLabelMapFilterTest3
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/connected0lines-1.png
              ${ITK_TEST_OUTPUT_DIR}/connected0lines-1.png
    itkBinaryImageToLabelMapFilterTest ${ITK_DATA_ROOT}/Input/connected0lines.png ${ITK_TEST_OUTPUT_DIR}/connected0lines-1.png 1 255 0 0)
itk_add_test(NAME itkBinaryImageToLabelMapFilterTest4
      COMMAND ITKReviewTestDriver itkBinaryImageToLabelMapFilterTest
              ${ITK_DATA_ROOT}/Input/289.png ${ITK_TEST_OUTPUT_DIR}/tooManyObjects.png 0 255 0 1)
itk_add_test(NAME itkBinaryImageToLabelMapFilterTest5
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/nonConnected3DLines-0.tif
              ${ITK_TEST_OUTPUT_DIR}/nonConnected3DLines-0.tif
    itkBinaryImageToLabelMapFilterTest ${ITK_DATA_ROOT}/Input/nonConnected3DLines.tif ${ITK_TEST_OUTPUT_DIR}/nonConnected3DLines-0.tif 0 255 0 0)
itk_add_test(NAME itkBinaryImageToShapeLabelMapFilterTest1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/Spots-binaryimage-to-shapelabel.mha
              ${ITK_TEST_OUTPUT_DIR}/Spots-binaryimage-to-shapelabel.mha
    itkBinaryImageToShapeLabelMapFilterTest1 ${ITK_DATA_ROOT}/Input/Spots.png ${ITK_TEST_OUTPUT_DIR}/Spots-binaryimage-to-shapelabel.mha 1 255 0 1 1)
itk_add_test(NAME itkBinaryImageToStatisticsLabelMapFilterTest1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/Spots-binaryimage-to-statisticslabel.mha
              ${ITK_TEST_OUTPUT_DIR}/Spots-binaryimage-to-statisticslabel.mha
    itkBinaryImageToStatisticsLabelMapFilterTest1 ${ITK_DATA_ROOT}/Input/Spots.png ${ITK_DATA_ROOT}/Input/Spots.png ${ITK_TEST_OUTPUT_DIR}/Spots-binaryimage-to-statisticslabel.mha 1 0 0 1 1 1 128)
itk_add_test(NAME itkBinaryOpeningByReconstructionImageFilterTest
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkBinaryOpeningByReconstructionImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryOpeningByReconstructionImageFilterTest.png
    itkBinaryOpeningByReconstructionImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1-bin.png ${ITK_TEST_OUTPUT_DIR}/itkBinaryOpeningByReconstructionImageFilterTest.png 0 255 10)
itk_add_test(NAME itkBinaryReconstructionByDilationImageFilterTest
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkBinaryReconstructionByDilationImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryReconstructionByDilationImageFilterTest.png
    itkBinaryReconstructionByDilationImageFilterTest ${ITK_DATA_ROOT}/Input/SpotsInverted.png ${ITK_DATA_ROOT}/Input/Spots-markers.png ${ITK_TEST_OUTPUT_DIR}/itkBinaryReconstructionByDilationImageFilterTest.png 255 100)
itk_add_test(NAME itkBinaryReconstructionByErosionImageFilterTest
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkBinaryReconstructionByErosionImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryReconstructionByErosionImageFilterTest.png
    itkBinaryReconstructionByErosionImageFilterTest ${ITK_DATA_ROOT}/Input/SpotsLabeled.png ${ITK_DATA_ROOT}/Input/Spots-markers.png ${ITK_TEST_OUTPUT_DIR}/itkBinaryReconstructionByErosionImageFilterTest.png 0 255)
itk_add_test(NAME itkBinaryReconstructionLabelMapFilterTest
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkBinaryReconstructionLabelMapFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryReconstructionLabelMapFilterTest.png
    itkBinaryReconstructionLabelMapFilterTest ${ITK_DATA_ROOT}/Input/cthead1Label.png ${ITK_DATA_ROOT}/Input/cthead1-markers.png ${ITK_TEST_OUTPUT_DIR}/itkBinaryReconstructionLabelMapFilterTest.png 94)
itk_add_test(NAME itkBinaryShapeOpeningImageFilterTest1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1-binary-shape-opening.mha
              ${ITK_TEST_OUTPUT_DIR}/cthead1-binary-shape-opening.mha
    itkBinaryShapeOpeningImageFilterTest1 ${ITK_DATA_ROOT}/Input/cthead1Label.png ${ITK_TEST_OUTPUT_DIR}/cthead1-binary-shape-opening.mha 255 0 140 1 1 100)
itk_add_test(NAME itkBoxMeanImageFilterTest3
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkBoxMeanImageFilter3.png
              ${ITK_TEST_OUTPUT_DIR}/itkBoxMeanImageFilter3.png
    itkBoxMeanImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkBoxMeanImageFilter3.png 3)
itk_add_test(NAME itkBoxMeanImageFilterTest10
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkBoxMeanImageFilter10.png
              ${ITK_TEST_OUTPUT_DIR}/itkBoxMeanImageFilter10.png
    itkBoxMeanImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkBoxMeanImageFilter10.png 10)
itk_add_test(NAME itkBoxSigmaImageFilterTest3
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkBoxSigmaImageFilter3.png
              ${ITK_TEST_OUTPUT_DIR}/itkBoxSigmaImageFilter3.png
    itkBoxSigmaImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkBoxSigmaImageFilter3.png 3)
itk_add_test(NAME itkBoxSigmaImageFilterTest10
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkBoxSigmaImageFilter10.png
              ${ITK_TEST_OUTPUT_DIR}/itkBoxSigmaImageFilter10.png
    itkBoxSigmaImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkBoxSigmaImageFilter10.png 10)
itk_add_test(NAME itkBruker2DSEQImageIOTest1
      COMMAND ITKReviewTestDriver itkBruker2DSEQImageIOTest
              ${ITK_DATA_ROOT}/Input/DCB120604.t61/11/pdata/1/2dseq ${ITK_TEST_OUTPUT_DIR}/itkBruker2DSEQImageIOTest1.mha)
itk_add_test(NAME itkBSplineControlPointImageFilterTest1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_TEST_OUTPUT_DIR}/N4ControlPoints_2D_output.nii.gz
              ${ITK_TEST_OUTPUT_DIR}/N4ControlPoints_2D_outputRefined.nii.gz
    itkBSplineControlPointImageFilterTest 2 ${ITK_DATA_ROOT}/Baseline/Review/N4ControlPoints_2D.nii.gz ${ITK_TEST_OUTPUT_DIR}/N4ControlPoints_2D_output.nii.gz ${ITK_TEST_OUTPUT_DIR}/N4ControlPoints_2D_outputRefined.nii.gz)
itk_add_test(NAME itkBSplineControlPointImageFilterTest2
      COMMAND ITKReviewTestDriver
    --compare ${ITK_TEST_OUTPUT_DIR}/N4ControlPoints_3D_output.nii.gz
              ${ITK_TEST_OUTPUT_DIR}/N4ControlPoints_3D_outputRefined.nii.gz
    itkBSplineControlPointImageFilterTest 3 ${ITK_DATA_ROOT}/Baseline/Review/N4ControlPoints_3D.nii.gz ${ITK_TEST_OUTPUT_DIR}/N4ControlPoints_3D_output.nii.gz ${ITK_TEST_OUTPUT_DIR}/N4ControlPoints_3D_outputRefined.nii.gz)
itk_add_test(NAME itkBSplineControlPointImageFunctionTest
      COMMAND ITKReviewTestDriver itkBSplineControlPointImageFunctionTest)
itk_add_test(NAME itkBSplineDeformableTransformInitializerTest1
      COMMAND ITKReviewTestDriver itkBSplineDeformableTransformInitializerTest1
              ${ITK_EXAMPLE_DATA_ROOT}/BSplineDisplacements1.txt ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformInitializerTest1.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformInitializerTest1DeformationField.mhd)
itk_add_test(NAME itkBSplineDeformableTransformInitializerTest2
      COMMAND ITKReviewTestDriver itkBSplineDeformableTransformInitializerTest2
              ${ITK_EXAMPLE_DATA_ROOT}/BSplineDisplacements1.txt ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformInitializerTest2.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformInitializerTest2DeformationField.mhd)
itk_add_test(NAME itkTransformToDeformationFieldSourceTest03
      COMMAND ITKReviewTestDriver
    --compare ${TEMP}/transformedImage.nii
              ${TEMP}/warpedImage.nii
  --compareNumberOfPixelsTolerance 20
   itkTransformToDeformationFieldSourceTest1 ${TEMP}/transformedImage.nii ${TEMP}/warpedImage.nii)
itk_add_test(NAME itkBSplineScatteredDataPointSetToImageFilterTest01
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkBSplineScatteredDataPointSetToImageFilterTest01.mha
              ${ITK_TEST_OUTPUT_DIR}/itkBSplineScatteredDataPointSetToImageFilterTest01.mha
    itkBSplineScatteredDataPointSetToImageFilterTest ${ITK_DATA_ROOT}/Input/cake_easy.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineScatteredDataPointSetToImageFilterTest01.mha)
itk_add_test(NAME itkBSplineScatteredDataPointSetToImageFilterTest02
      COMMAND ITKReviewTestDriver itkBSplineScatteredDataPointSetToImageFilterTest2
              ${ITK_TEST_OUTPUT_DIR}/itkBSplineScatteredApproximationDataPointsTestOutput2.txt)
itk_add_test(NAME itkBSplineScatteredDataPointSetToImageFilterTest03
      COMMAND ITKReviewTestDriver itkBSplineScatteredDataPointSetToImageFilterTest3
              ${ITK_DATA_ROOT}/Input/BSplineScatteredApproximationDataPointsInput.txt ${ITK_TEST_OUTPUT_DIR}/itkBSplineScatteredApproximationDataPointsTestOutput3.txt)
itk_add_test(NAME itkChangeLabelLabelMapFilterTest1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1-label-changed.mha
              ${ITK_TEST_OUTPUT_DIR}/cthead1-label-changed.mha
    itkChangeLabelLabelMapFilterTest ${ITK_DATA_ROOT}/Input/cthead1Label.png ${ITK_TEST_OUTPUT_DIR}/cthead1-label-changed.mha 94 150 92 163 176 176)
itk_add_test(NAME itkChangeLabelLabelMapFilterTest2
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1-label-changed2.mha
              ${ITK_TEST_OUTPUT_DIR}/cthead1-label-changed2.mha
    itkChangeLabelLabelMapFilterTest ${ITK_DATA_ROOT}/Input/cthead1Label.png ${ITK_TEST_OUTPUT_DIR}/cthead1-label-changed2.mha 94 100 0 50 92 0)
itk_add_test(NAME itkChangeLabelLabelMapFilterTest3
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1-label-changed3.png
              ${ITK_TEST_OUTPUT_DIR}/cthead1-label-changed3.png
    itkChangeLabelLabelMapFilterTest ${ITK_DATA_ROOT}/Input/cthead1Label.png ${ITK_TEST_OUTPUT_DIR}/cthead1-label-changed3.png 0 94)
itk_add_test(NAME itkChangeRegionLabelMapFilterTest1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1-label-changeregion.mha
              ${ITK_TEST_OUTPUT_DIR}/cthead1-label-changeregion.mha
    itkChangeRegionLabelMapFilterTest1 ${ITK_DATA_ROOT}/Input/cthead1Label.png ${ITK_TEST_OUTPUT_DIR}/cthead1-label-changeregion.mha 50 100 120 150)
itk_add_test(NAME itkCompositeTransformTest
      COMMAND ITKReviewTestDriver itkCompositeTransformTest)
itk_add_test(NAME itkConformalFlatteningMeshFilterTest1
      COMMAND ITKReviewTestDriver itkConformalFlatteningMeshFilterTest
              ${ITK_DATA_ROOT}/Input/genusZeroSurface01.vtk ${ITK_TEST_OUTPUT_DIR}/genusZeroMesh01ConformalFlattenedToSphere.vtk 1)
itk_add_test(NAME itkConformalFlatteningMeshFilterTest2
      COMMAND ITKReviewTestDriver itkConformalFlatteningMeshFilterTest
              ${ITK_DATA_ROOT}/Input/genusZeroSurface01.vtk ${ITK_TEST_OUTPUT_DIR}/genusZeroMesh01ConformalFlattenedToPlane.vtk 0)
itk_add_test(NAME itkConformalFlatteningQuadEdgeMeshFilterTest1
      COMMAND ITKReviewTestDriver itkConformalFlatteningQuadEdgeMeshFilterTest
              ${ITK_DATA_ROOT}/Input/genusZeroSurface01.vtk ${ITK_TEST_OUTPUT_DIR}/genusZeroQuadEdgeMesh01ConformalFlattenedToSphere.vtk 3 100.0 1)
itk_add_test(NAME itkConformalFlatteningQuadEdgeMeshFilterTest2
      COMMAND ITKReviewTestDriver itkConformalFlatteningQuadEdgeMeshFilterTest
              ${ITK_DATA_ROOT}/Input/genusZeroSurface01.vtk ${ITK_TEST_OUTPUT_DIR}/genusZeroQuadEdgeMesh01ConformalFlattenedToPlane.vtk 3 100.0 0)
itk_add_test(NAME itkContourExtractor2DImageFilterTest
      COMMAND ITKReviewTestDriver itkContourExtractor2DImageFilterTest
              ${ITK_DATA_ROOT}/Input/ContourExtractor2DTest.tif)
itk_add_test(NAME itkConvertLabelMapFilterTest1
      COMMAND ITKReviewTestDriver
  --compare ${INPUTDATA}/cthead1Label.png
            ${TEMP}/cthead1-convert.png
  itkConvertLabelMapFilterTest1 ${INPUTDATA}/cthead1Label.png ${TEMP}/cthead1-convert.png
  )
itk_add_test(NAME itkConvolutionImageFilterTestSobelX
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkConvolutionImageFilterTestSobelX.nii.gz
              ${ITK_TEST_OUTPUT_DIR}/itkConvolutionImageFilterTestSobelX.nii.gz
    itkConvolutionImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_DATA_ROOT}/Input/sobel_x.nii.gz ${ITK_TEST_OUTPUT_DIR}/itkConvolutionImageFilterTestSobelX.nii.gz)
itk_add_test(NAME itkConvolutionImageFilterTestSobelY
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkConvolutionImageFilterTestSobelY.nii.gz
              ${ITK_TEST_OUTPUT_DIR}/itkConvolutionImageFilterTestSobelY.nii.gz
    itkConvolutionImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_DATA_ROOT}/Input/sobel_y.nii.gz ${ITK_TEST_OUTPUT_DIR}/itkConvolutionImageFilterTestSobelY.nii.gz)
itk_add_test(NAME itkConvolutionImageFilterTest4x4Mean
      COMMAND ITKReviewTestDriver
    --compare DATA{Baseline/itkConvolutionImageFilterTest4x4Mean.png}
              ${ITK_TEST_OUTPUT_DIR}/itkConvolutionImageFilterTest4x4Mean.png
    itkConvolutionImageFilterTestInt ${ITK_DATA_ROOT}/Input/cthead1.png DATA{Input/4x4-constant.png} ${ITK_TEST_OUTPUT_DIR}/itkConvolutionImageFilterTest4x4Mean.png 1)
itk_add_test(NAME itkConvolutionImageFilterTest4x5Mean
      COMMAND ITKReviewTestDriver
    --compare DATA{Baseline/itkConvolutionImageFilterTest4x5Mean.png}
              ${ITK_TEST_OUTPUT_DIR}/itkConvolutionImageFilterTest4x5Mean.png
    itkConvolutionImageFilterTestInt ${ITK_DATA_ROOT}/Input/cthead1.png DATA{Input/4x5-constant.png} ${ITK_TEST_OUTPUT_DIR}/itkConvolutionImageFilterTest4x5Mean.png 1)
itk_add_test(NAME itkConvolutionImageFilterTest5x5Mean
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkConvolutionImageFilterTest5x5Mean.png
              ${ITK_TEST_OUTPUT_DIR}/itkConvolutionImageFilterTest5x5Mean.png
    itkConvolutionImageFilterTestInt ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_DATA_ROOT}/Input/5x5-constant.png ${ITK_TEST_OUTPUT_DIR}/itkConvolutionImageFilterTest5x5Mean.png 1)
itk_add_test(NAME itkCoxDeBoorBSplineKernelFunctionTest01
      COMMAND ITKReviewTestDriver itkCoxDeBoorBSplineKernelFunctionTest)
itk_add_test(NAME itkCoxDeBoorBSplineKernelFunctionTest02
      COMMAND ITKReviewTestDriver itkCoxDeBoorBSplineKernelFunctionTest2)
itk_add_test(NAME itkCropLabelMapFilterTest1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1-label-crop.mha
              ${ITK_TEST_OUTPUT_DIR}/cthead1-label-crop.mha
    itkCropLabelMapFilterTest1 ${ITK_DATA_ROOT}/Input/cthead1Label.png ${ITK_TEST_OUTPUT_DIR}/cthead1-label-crop.mha 40 50)
itk_add_test(NAME itkDeformationFieldTransformTest
      COMMAND ITKReviewTestDriver itkDeformationFieldTransformTest)
itk_add_test(NAME itkDiffeomorphicDemonsRegistrationFilterTest01
      COMMAND ITKReviewTestDriver itkDiffeomorphicDemonsRegistrationFilterTest
              0 0 0.001 20.0 200 0.01 0.08 5 10)
itk_add_test(NAME itkDiffeomorphicDemonsRegistrationFilterTest02
      COMMAND ITKReviewTestDriver itkDiffeomorphicDemonsRegistrationFilterTest
              1 0 0.01 3.0 400 0.2 0.9 10 70)
itk_add_test(NAME itkDiffeomorphicDemonsRegistrationFilterTest03
      COMMAND ITKReviewTestDriver itkDiffeomorphicDemonsRegistrationFilterTest
              2 0 0.001 20.0 200 0.01 0.08 5 10)
itk_add_test(NAME itkDiffeomorphicDemonsRegistrationFilterTest04
      COMMAND ITKReviewTestDriver itkDiffeomorphicDemonsRegistrationFilterTest
              3 0 0.01 3.0 200 0.2 0.8 5 70)
itk_add_test(NAME itkDiffeomorphicDemonsRegistrationFilterTest05
      COMMAND ITKReviewTestDriver itkDiffeomorphicDemonsRegistrationFilterTest
              0 1 0.01 3.0 400 0.2 0.08 5 70)
itk_add_test(NAME itkDiffeomorphicDemonsRegistrationFilterTest06
      COMMAND ITKReviewTestDriver itkDiffeomorphicDemonsRegistrationFilterTest
              0 0 0.005 20.0 200 0.01 0.08 5 10)
itk_add_test(NAME itkDiffeomorphicDemonsRegistrationFilterTest07
      COMMAND ITKReviewTestDriver itkDiffeomorphicDemonsRegistrationFilterTest2
              ${ITK_EXAMPLE_DATA_ROOT}/RatLungSlice1.mha ${ITK_EXAMPLE_DATA_ROOT}/RatLungSlice2.mha ${ITK_TEST_OUTPUT_DIR}/itkDiffeomorphicDemonsRegistrationFilterTest07.mha 0 0 0.001 0.1)
itk_add_test(NAME itkDiffeomorphicDemonsRegistrationFilterTest08
      COMMAND ITKReviewTestDriver itkDiffeomorphicDemonsRegistrationFilterTest2
              ${ITK_EXAMPLE_DATA_ROOT}/RatLungSlice1.mha ${ITK_EXAMPLE_DATA_ROOT}/RatLungSlice2.mha ${ITK_TEST_OUTPUT_DIR}/itkDiffeomorphicDemonsRegistrationFilterTest08.mha 1 0 0.001 0.1)
itk_add_test(NAME itkDiffeomorphicDemonsRegistrationFilterTest09
      COMMAND ITKReviewTestDriver itkDiffeomorphicDemonsRegistrationFilterTest2
              ${ITK_EXAMPLE_DATA_ROOT}/RatLungSlice1.mha ${ITK_EXAMPLE_DATA_ROOT}/RatLungSlice2.mha ${ITK_TEST_OUTPUT_DIR}/itkDiffeomorphicDemonsRegistrationFilterTest09.mha 2 0 0.001 0.1)
itk_add_test(NAME itkDiffeomorphicDemonsRegistrationFilterTest10
      COMMAND ITKReviewTestDriver itkDiffeomorphicDemonsRegistrationFilterTest2
              ${ITK_EXAMPLE_DATA_ROOT}/RatLungSlice1.mha ${ITK_EXAMPLE_DATA_ROOT}/RatLungSlice2.mha ${ITK_TEST_OUTPUT_DIR}/itkDiffeomorphicDemonsRegistrationFilterTest10.mha 3 0 0.001 0.1)
itk_add_test(NAME itkDiffeomorphicDemonsRegistrationFilterTest11
      COMMAND ITKReviewTestDriver itkDiffeomorphicDemonsRegistrationFilterTest2
              ${ITK_EXAMPLE_DATA_ROOT}/RatLungSlice1.mha ${ITK_EXAMPLE_DATA_ROOT}/RatLungSlice2.mha ${ITK_TEST_OUTPUT_DIR}/itkDiffeomorphicDemonsRegistrationFilterTest11.mha 0 1 0.001 0.1)
itk_add_test(NAME itkDirectFourierReconstructionImageToImageFilterTest
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/reconstruct.mha
              ${ITK_TEST_OUTPUT_DIR}/reconstruct.mha
    itkDirectFourierReconstructionImageToImageFilterTest ${ITK_DATA_ROOT}/Input/sinogram-small.mha ${ITK_TEST_OUTPUT_DIR}/reconstruct.mha 1 2 0 2 2 1 3 180 0 0 0 64 64 2 0)
itk_add_test(NAME itkDiscreteGaussianDerivativeImageFilterScaleSpaceTest
      COMMAND ITKReviewTestDriver itkDiscreteGaussianDerivativeImageFilterScaleSpaceTest)
itk_add_test(NAME itkDiscreteGaussianDerivativeImageFilterTest01
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkDiscreteGaussianDerivativeImageFilterTest01.png
              ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest01.png
    itkDiscreteGaussianDerivativeImageFilterTest ${ITK_DATA_ROOT}/Input/whiteSpot.png ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest01.png 0 1 3.0)
itk_add_test(NAME itkDiscreteGaussianDerivativeImageFilterTest10
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkDiscreteGaussianDerivativeImageFilterTest10.png
              ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest10.png
    itkDiscreteGaussianDerivativeImageFilterTest ${ITK_DATA_ROOT}/Input/whiteSpot.png ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest10.png 1 0 3.0)
itk_add_test(NAME itkDiscreteGaussianDerivativeImageFilterTest11
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkDiscreteGaussianDerivativeImageFilterTest11.png
              ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest11.png
    itkDiscreteGaussianDerivativeImageFilterTest ${ITK_DATA_ROOT}/Input/whiteSpot.png ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest11.png 1 1 3.0)
itk_add_test(NAME itkDiscreteGaussianDerivativeImageFilterTest20
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkDiscreteGaussianDerivativeImageFilterTest20.png
              ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest20.png
    itkDiscreteGaussianDerivativeImageFilterTest ${ITK_DATA_ROOT}/Input/whiteSpot.png ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest20.png 2 0 3.0)
itk_add_test(NAME itkDiscreteGaussianDerivativeImageFilterTest02
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkDiscreteGaussianDerivativeImageFilterTest02.png
              ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest02.png
    itkDiscreteGaussianDerivativeImageFilterTest ${ITK_DATA_ROOT}/Input/whiteSpot.png ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest02.png 0 2 3.0)
itk_add_test(NAME itkDiscreteGaussianDerivativeImageFilterTest21
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkDiscreteGaussianDerivativeImageFilterTest21.png
              ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest21.png
    itkDiscreteGaussianDerivativeImageFilterTest ${ITK_DATA_ROOT}/Input/whiteSpot.png ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest21.png 2 1 3.0)
itk_add_test(NAME itkDiscreteGaussianDerivativeImageFilterTest12
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkDiscreteGaussianDerivativeImageFilterTest12.png
              ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest12.png
    itkDiscreteGaussianDerivativeImageFilterTest ${ITK_DATA_ROOT}/Input/whiteSpot.png ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest12.png 1 2 3.0)
itk_add_test(NAME itkDiscreteGaussianDerivativeImageFilterTest30
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkDiscreteGaussianDerivativeImageFilterTest30.png
              ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest30.png
    itkDiscreteGaussianDerivativeImageFilterTest ${ITK_DATA_ROOT}/Input/whiteSpot.png ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest30.png 3 0 3.0)
itk_add_test(NAME itkDiscreteGaussianDerivativeImageFilterTest03
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkDiscreteGaussianDerivativeImageFilterTest03.png
              ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest03.png
    itkDiscreteGaussianDerivativeImageFilterTest ${ITK_DATA_ROOT}/Input/whiteSpot.png ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest03.png 0 3 3.0)
itk_add_test(NAME itkDiscreteGaussianDerivativeImageFunctionTest01
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkDiscreteGaussianDerivativeImageFunctionTest01.png
              ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFunctionTest01.png
    itkDiscreteGaussianDerivativeImageFunctionTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFunctionTest01.png 1 2.0 0.01 32)
itk_add_test(NAME itkDiscreteGradientMagnitudeGaussianImageFunctionTest01
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkDiscreteGradientMagnitudeGaussianImageFunctionTest01.png
              ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGradientMagnitudeGaussianImageFunctionTest01.png
    itkDiscreteGradientMagnitudeGaussianImageFunctionTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGradientMagnitudeGaussianImageFunctionTest01.png 2.0 0.01 32)
itk_add_test(NAME itkDivideByConstantImageFilterTest
      COMMAND ITKReviewTestDriver itkDivideByConstantImageFilterTest)
itk_add_test(NAME itkExponentialDeformationFieldImageFilterTest
      COMMAND ITKReviewTestDriver itkExponentialDeformationFieldImageFilterTest)
itk_add_test(NAME itkFastApproximateRankImageFilterTest3
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkFastApproximateRankImageFilter3.png
              ${ITK_TEST_OUTPUT_DIR}/itkFastApproximateRankImageFilter3.png
    itkFastApproximateRankImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkFastApproximateRankImageFilter3.png 3)
itk_add_test(NAME itkFastApproximateRankImageFilterTest10
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkFastApproximateRankImageFilter10.png
              ${ITK_TEST_OUTPUT_DIR}/itkFastApproximateRankImageFilter10.png
    itkFastApproximateRankImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkFastApproximateRankImageFilter10.png 10)
itk_add_test(NAME itkFastSymmetricForcesDemonsRegistrationFilterTest
      COMMAND ITKReviewTestDriver itkFastSymmetricForcesDemonsRegistrationFilterTest)
itk_add_test(NAME itkFFTShiftImageFilterTestOdd0
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkFFTShiftImageFilterTest0.png
              ${ITK_TEST_OUTPUT_DIR}/itkFFTShiftImageFilterTest0.png
    itkFFTShiftImageFilterTest ${ITK_DATA_ROOT}/Input/BrainSliceBinary.png ${ITK_TEST_OUTPUT_DIR}/itkFFTShiftImageFilterTest0.png 0)
itk_add_test(NAME itkFFTShiftImageFilterTestOdd1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Input/BrainSliceBinary.png
              ${ITK_TEST_OUTPUT_DIR}/itkFFTShiftImageFilterTest1.png
    itkFFTShiftImageFilterTest ${ITK_DATA_ROOT}/Baseline/Review/itkFFTShiftImageFilterTest0.png ${ITK_TEST_OUTPUT_DIR}/itkFFTShiftImageFilterTest1.png 1)
itk_add_test(NAME itkFFTShiftImageFilterTestEven0
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkFFTShiftImageFilterTestEven0.png
              ${ITK_TEST_OUTPUT_DIR}/itkFFTShiftImageFilterTestEven0.png
    itkFFTShiftImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkFFTShiftImageFilterTestEven0.png 0)
itk_add_test(NAME itkFFTShiftImageFilterTestEven1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Input/cthead1.png
              ${ITK_TEST_OUTPUT_DIR}/itkFFTShiftImageFilterTestEven1.png
    itkFFTShiftImageFilterTest ${ITK_DATA_ROOT}/Baseline/Review/itkFFTShiftImageFilterTestEven0.png ${ITK_TEST_OUTPUT_DIR}/itkFFTShiftImageFilterTestEven1.png 1)
itk_add_test(NAME itkGaborImageSourceTest0
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/GaborImageSourceTest0.mha
              ${ITK_TEST_OUTPUT_DIR}/GaborImageSourceTest0.mha
    itkGaborImageSourceTest ${ITK_TEST_OUTPUT_DIR}/GaborImageSourceTest0.mha 0)
itk_add_test(NAME itkGaborImageSourceTest1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/GaborImageSourceTest1.mha
              ${ITK_TEST_OUTPUT_DIR}/GaborImageSourceTest1.mha
    itkGaborImageSourceTest ${ITK_TEST_OUTPUT_DIR}/GaborImageSourceTest1.mha 1)
itk_add_test(NAME itkGaussianDerivativeOperatorTest
      COMMAND ITKReviewTestDriver itkGaussianDerivativeOperatorTest)
itk_add_test(NAME itkGridForwardWarpImageFilterTest
      COMMAND ITKReviewTestDriver itkGridForwardWarpImageFilterTest)
itk_add_test(NAME itkGridImageSourceTest1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkGridImageSourceTest1.nhdr
              ${ITK_TEST_OUTPUT_DIR}/itkGridImageSourceTest1.nhdr
    itkGridImageSourceTest ${ITK_TEST_OUTPUT_DIR}/itkGridImageSourceTest1.nhdr 0)
itk_add_test(NAME itkGridImageSourceTest2
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkGridImageSourceTest2.nhdr
              ${ITK_TEST_OUTPUT_DIR}/itkGridImageSourceTest2.nhdr
    itkGridImageSourceTest ${ITK_TEST_OUTPUT_DIR}/itkGridImageSourceTest2.nhdr 1)
itk_add_test(NAME itkGridImageSourceTest3
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkGridImageSourceTest3.nhdr
              ${ITK_TEST_OUTPUT_DIR}/itkGridImageSourceTest3.nhdr
    itkGridImageSourceTest ${ITK_TEST_OUTPUT_DIR}/itkGridImageSourceTest3.nhdr 2)
itk_add_test(NAME itkGridImageSourceTest4
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkGridImageSourceTest4.nhdr
              ${ITK_TEST_OUTPUT_DIR}/itkGridImageSourceTest4.nhdr
    itkGridImageSourceTest ${ITK_TEST_OUTPUT_DIR}/itkGridImageSourceTest4.nhdr 3)
itk_add_test(NAME itkImageReadComplexWriteMagnitudeAndPhaseTest
      COMMAND ITKReviewTestDriver itkImageReadComplexWriteMagnitudeAndPhaseTest
              ${ITK_EXAMPLE_DATA_ROOT}/itkBrainSliceComplex.mha ${ITK_TEST_OUTPUT_DIR}/itkBrainSliceComplexMagnitude.mha ${ITK_TEST_OUTPUT_DIR}/itkBrainSliceComplexPhase.mha)
itk_add_test(NAME itkImageReadMagnitudeAndPhaseWriteComplexTest
      COMMAND ITKReviewTestDriver itkImageReadMagnitudeAndPhaseWriteComplexTest
              ${ITK_EXAMPLE_DATA_ROOT}/itkBrainSliceComplexMagnitude.mha ${ITK_EXAMPLE_DATA_ROOT}/itkBrainSliceComplexPhase.mha ${ITK_TEST_OUTPUT_DIR}/itkBrainSliceComplex.mha)
itk_add_test(NAME itkImageFunctionTest
      COMMAND ITKReviewTestDriver itkImageFunctionTest)
itk_add_test(NAME itkJPEG2000Test00
      COMMAND ITKReviewTestDriver itkJPEG2000ImageIORegionOfInterest
              ${ITK_DATA_ROOT}/Input/Bretagne1.j2k ${ITK_TEST_OUTPUT_DIR}/Bretagne1_RegionTest01.tif 160 120 320 240)
itk_add_test(NAME itkJPEG2000Test01
      COMMAND ITKReviewTestDriver itkJPEG2000ImageIOTest03
              ${ITK_DATA_ROOT}/Input/Bretagne1.j2k ${ITK_TEST_OUTPUT_DIR}/Bretagne1_01.png)
itk_add_test(NAME itkJPEG2000Test02
      COMMAND ITKReviewTestDriver itkJPEG2000ImageIOTest03
              ${ITK_DATA_ROOT}/Input/Cevennes2.jp2 ${ITK_TEST_OUTPUT_DIR}/Cevennes2_01.png)
itk_add_test(NAME itkJPEG2000Test03
      COMMAND ITKReviewTestDriver itkJPEG2000ImageIOTest03
              ${ITK_TEST_OUTPUT_DIR}/Bretagne1_RegionTest01.tif ${ITK_TEST_OUTPUT_DIR}/Bretagne1_02.j2k)
set_tests_properties( itkJPEG2000Test03 PROPERTIES DEPENDS itkJPEG2000Test00 )

itk_add_test(NAME itkJPEG2000Test05
      COMMAND ITKReviewTestDriver itkJPEG2000ImageIOTest05
              ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR} jp2)
itk_add_test(NAME itkJPEG2000Test06
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/cthead1.tif
              ${ITK_TEST_OUTPUT_DIR}/cthead1.tif
    itkJPEG2000ImageIOTest06 ${ITK_DATA_ROOT}/Input/cthead1.j2k ${ITK_TEST_OUTPUT_DIR}/cthead1.tif)
itk_add_test(NAME itkKappaSigmaThresholdImageCalculatorTest01
      COMMAND ITKReviewTestDriver itkKappaSigmaThresholdImageCalculatorTest
              ${ITK_DATA_ROOT}/Input/CellsFluorescence1.png 10 3.5 19)
itk_add_test(NAME itkKappaSigmaThresholdImageCalculatorTest02
      COMMAND ITKReviewTestDriver itkKappaSigmaThresholdImageCalculatorTest
              ${ITK_DATA_ROOT}/Input/CellsFluorescence2.png 10 3.0 19)
itk_add_test(NAME itkKappaSigmaThresholdImageFilterTest01
      COMMAND ITKReviewTestDriver itkKappaSigmaThresholdImageFilterTest
              ${ITK_DATA_ROOT}/Input/CellsFluorescence1.png ${ITK_TEST_OUTPUT_DIR}/itkKappaSigmaThresholdImageFilterTest01.mha 10 3.5)
itk_add_test(NAME itkKappaSigmaThresholdImageFilterTest02
      COMMAND ITKReviewTestDriver itkKappaSigmaThresholdImageFilterTest
              ${ITK_DATA_ROOT}/Input/CellsFluorescence2.png ${ITK_TEST_OUTPUT_DIR}/itkKappaSigmaThresholdImageFilterTest02.mha 10 3.0)
itk_add_test(NAME itkLabelGeometryImageFilterTest2
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkLabelGeometryImageFilterTest2Image.png
              ${ITK_TEST_OUTPUT_DIR}/itkLabelGeometryImageFilterTest2Image.png
    itkLabelGeometryImageFilterTest ${ITK_DATA_ROOT}/Input/BinaryImageWithVariousShapes01.png ${ITK_TEST_OUTPUT_DIR}/itkLabelGeometryImageFilterTest2Image.png 2 ${ITK_DATA_ROOT}/Input/GrayscaleImageWithVariousShapes01.png ${ITK_TEST_OUTPUT_DIR})
itk_add_test(NAME itkLabelImageToLabelMapFilterTest
      COMMAND ITKReviewTestDriver itkLabelImageToLabelMapFilterTest)
itk_add_test(NAME itkLabelImageToShapeLabelMapFilterTest1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/simple-label-to-shapelabelmap.mha
              ${ITK_TEST_OUTPUT_DIR}/simple-label-to-shapelabelmap.mha
    itkLabelImageToShapeLabelMapFilterTest1 ${ITK_DATA_ROOT}/Input/simple-label-b.png ${ITK_TEST_OUTPUT_DIR}/simple-label-to-shapelabelmap.mha 90 1 1)
itk_add_test(NAME itkLabelImageToStatisticsLabelMapFilterTest1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Input/Spots.png
              ${ITK_TEST_OUTPUT_DIR}/Spots-labelimage-to-statisticslabel.png
    itkLabelImageToStatisticsLabelMapFilterTest1 ${ITK_DATA_ROOT}/Input/Spots.png ${ITK_DATA_ROOT}/Input/Spots.png ${ITK_TEST_OUTPUT_DIR}/Spots-labelimage-to-statisticslabel.png 0 1 1 1 128)
itk_add_test(NAME itkLabelMapContourOverlayImageFilterTest0
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapContourOverlayImageFilterTest0.png
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapContourOverlayImageFilterTest0.png
    itkLabelMapContourOverlayImageFilterTest1 ${ITK_DATA_ROOT}/Input/SpotsLabeled.png ${ITK_DATA_ROOT}/Input/SpotsInverted.png ${ITK_TEST_OUTPUT_DIR}/itkLabelMapContourOverlayImageFilterTest0.png 0.6 0 0 10 1)
itk_add_test(NAME itkLabelMapContourOverlayImageFilterTest1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapContourOverlayImageFilterTest1.png
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapContourOverlayImageFilterTest1.png
    itkLabelMapContourOverlayImageFilterTest1 ${ITK_DATA_ROOT}/Input/SpotsLabeled.png ${ITK_DATA_ROOT}/Input/SpotsInverted.png ${ITK_TEST_OUTPUT_DIR}/itkLabelMapContourOverlayImageFilterTest1.png 0.7 1 2 15 0)
itk_add_test(NAME itkLabelMapContourOverlayImageFilterTest2
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapContourOverlayImageFilterTest2.png
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapContourOverlayImageFilterTest2.png
    itkLabelMapContourOverlayImageFilterTest1 ${ITK_DATA_ROOT}/Input/SpotsLabeled.png ${ITK_DATA_ROOT}/Input/SpotsInverted.png ${ITK_TEST_OUTPUT_DIR}/itkLabelMapContourOverlayImageFilterTest2.png 0.8 2 3 12 1)
itk_add_test(NAME itkLabelMapFilterTest
      COMMAND ITKReviewTestDriver itkLabelMapFilterTest)
itk_add_test(NAME itkLabelMapMaskImageFilterTest-0-0-0
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTest-0-0-0.png
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTest-0-0-0.png
    itkLabelMapMaskImageFilterTest ${ITK_DATA_ROOT}/Input/2th_cthead1.png ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTest-0-0-0.png 0 0 0 0 0)
itk_add_test(NAME itkLabelMapMaskImageFilterTestCrop-0-0-0-0
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTestCrop-0-0-0-0.png
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-0-0-0-0.png
    itkLabelMapMaskImageFilterTest ${ITK_DATA_ROOT}/Input/2th_cthead1.png ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-0-0-0-0.png 0 0 0 1 0)
itk_add_test(NAME itkLabelMapMaskImageFilterTestCrop-0-0-0-10
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTestCrop-0-0-0-10.png
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-0-0-0-10.png
    itkLabelMapMaskImageFilterTest ${ITK_DATA_ROOT}/Input/2th_cthead1.png ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-0-0-0-10.png 0 0 0 1 10)
itk_add_test(NAME itkLabelMapMaskImageFilterTest-0-0-1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTest-0-0-1.png
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTest-0-0-1.png
    itkLabelMapMaskImageFilterTest ${ITK_DATA_ROOT}/Input/2th_cthead1.png ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTest-0-0-1.png 0 0 1 0 0)
itk_add_test(NAME itkLabelMapMaskImageFilterTestCrop-0-0-1-0
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTestCrop-0-0-1-0.png
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-0-0-1-0.png
    itkLabelMapMaskImageFilterTest ${ITK_DATA_ROOT}/Input/2th_cthead1.png ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-0-0-1-0.png 0 0 1 1 0)
itk_add_test(NAME itkLabelMapMaskImageFilterTestCrop-0-0-1-10
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTestCrop-0-0-1-10.png
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-0-0-1-10.png
    itkLabelMapMaskImageFilterTest ${ITK_DATA_ROOT}/Input/2th_cthead1.png ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-0-0-1-10.png 0 0 1 1 10)
itk_add_test(NAME itkLabelMapMaskImageFilterTest-0-1-0
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTest-0-1-0.png
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTest-0-1-0.png
    itkLabelMapMaskImageFilterTest ${ITK_DATA_ROOT}/Input/2th_cthead1.png ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTest-0-1-0.png 0 1 0 0 0)
itk_add_test(NAME itkLabelMapMaskImageFilterTestCrop-0-1-0-0
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTestCrop-0-1-0-0.png
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-0-1-0-0.png
    itkLabelMapMaskImageFilterTest ${ITK_DATA_ROOT}/Input/2th_cthead1.png ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-0-1-0-0.png 0 1 0 1 0)
itk_add_test(NAME itkLabelMapMaskImageFilterTestCrop-0-1-0-10
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTestCrop-0-1-0-10.png
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-0-1-0-10.png
    itkLabelMapMaskImageFilterTest ${ITK_DATA_ROOT}/Input/2th_cthead1.png ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-0-1-0-10.png 0 1 0 1 10)
itk_add_test(NAME itkLabelMapMaskImageFilterTest-0-1-1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTest-0-1-1.png
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTest-0-1-1.png
    itkLabelMapMaskImageFilterTest ${ITK_DATA_ROOT}/Input/2th_cthead1.png ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTest-0-1-1.png 0 1 1 0 0)
itk_add_test(NAME itkLabelMapMaskImageFilterTestCrop-0-1-1-0
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTestCrop-0-1-1-0.png
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-0-1-1-0.png
    itkLabelMapMaskImageFilterTest ${ITK_DATA_ROOT}/Input/2th_cthead1.png ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-0-1-1-0.png 0 1 1 1 0)
itk_add_test(NAME itkLabelMapMaskImageFilterTestCrop-0-1-1-10
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTestCrop-0-1-1-10.png
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-0-1-1-10.png
    itkLabelMapMaskImageFilterTest ${ITK_DATA_ROOT}/Input/2th_cthead1.png ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-0-1-1-10.png 0 1 1 1 10)
itk_add_test(NAME itkLabelMapMaskImageFilterTest-100-0-0
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTest-100-0-0.png
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTest-100-0-0.png
    itkLabelMapMaskImageFilterTest ${ITK_DATA_ROOT}/Input/2th_cthead1.png ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTest-100-0-0.png 100 0 0 0 0)
itk_add_test(NAME itkLabelMapMaskImageFilterTestCrop-100-0-0-0
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTestCrop-100-0-0-0.png
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-100-0-0-0.png
    itkLabelMapMaskImageFilterTest ${ITK_DATA_ROOT}/Input/2th_cthead1.png ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-100-0-0-0.png 100 0 0 1 0)
itk_add_test(NAME itkLabelMapMaskImageFilterTestCrop-100-0-0-10
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTestCrop-100-0-0-10.png
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-100-0-0-10.png
    itkLabelMapMaskImageFilterTest ${ITK_DATA_ROOT}/Input/2th_cthead1.png ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-100-0-0-10.png 100 0 0 1 10)
itk_add_test(NAME itkLabelMapMaskImageFilterTest-100-0-1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTest-100-0-1.png
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTest-100-0-1.png
    itkLabelMapMaskImageFilterTest ${ITK_DATA_ROOT}/Input/2th_cthead1.png ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTest-100-0-1.png 100 0 1 0 0)
itk_add_test(NAME itkLabelMapMaskImageFilterTestCrop-100-0-1-0
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTestCrop-100-0-1-0.png
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-100-0-1-0.png
    itkLabelMapMaskImageFilterTest ${ITK_DATA_ROOT}/Input/2th_cthead1.png ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-100-0-1-0.png 100 0 1 1 0)
itk_add_test(NAME itkLabelMapMaskImageFilterTestCrop-100-0-1-10
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTestCrop-100-0-1-10.png
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-100-0-1-10.png
    itkLabelMapMaskImageFilterTest ${ITK_DATA_ROOT}/Input/2th_cthead1.png ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-100-0-1-10.png 100 0 1 1 10)
itk_add_test(NAME itkLabelMapMaskImageFilterTest-100-1-0
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTest-100-1-0.png
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTest-100-1-0.png
    itkLabelMapMaskImageFilterTest ${ITK_DATA_ROOT}/Input/2th_cthead1.png ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTest-100-1-0.png 100 1 0 0 0)
itk_add_test(NAME itkLabelMapMaskImageFilterTestCrop-100-1-0-0
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTestCrop-100-1-0-0.png
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-100-1-0-0.png
    itkLabelMapMaskImageFilterTest ${ITK_DATA_ROOT}/Input/2th_cthead1.png ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-100-1-0-0.png 100 1 0 1 0)
itk_add_test(NAME itkLabelMapMaskImageFilterTestCrop-100-1-0-10
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTestCrop-100-1-0-10.png
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-100-1-0-10.png
    itkLabelMapMaskImageFilterTest ${ITK_DATA_ROOT}/Input/2th_cthead1.png ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-100-1-0-10.png 100 1 0 1 10)
itk_add_test(NAME itkLabelMapMaskImageFilterTest-100-1-1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTest-100-1-1.png
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTest-100-1-1.png
    itkLabelMapMaskImageFilterTest ${ITK_DATA_ROOT}/Input/2th_cthead1.png ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTest-100-1-1.png 100 1 1 0 0)
itk_add_test(NAME itkLabelMapMaskImageFilterTestCrop-100-1-1-0
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTestCrop-100-1-1-0.png
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-100-1-1-0.png
    itkLabelMapMaskImageFilterTest ${ITK_DATA_ROOT}/Input/2th_cthead1.png ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-100-1-1-0.png 100 1 1 1 0)
itk_add_test(NAME itkLabelMapMaskImageFilterTestCrop-100-1-1-10
      COMMAND ITKReviewTestDriver
    --compare  ${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTestCrop-100-1-1-10.png
               ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-100-1-1-10.png
    itkLabelMapMaskImageFilterTest ${ITK_DATA_ROOT}/Input/2th_cthead1.png ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-100-1-1-10.png 100 1 1 1 10)
itk_add_test(NAME itkLabelMapOverlayImageFilterTest1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapOverlayImageFilterTest1.png
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapOverlayImageFilterTest1.png
    itkLabelMapOverlayImageFilterTest1 ${ITK_DATA_ROOT}/Input/cthead1Label.png ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkLabelMapOverlayImageFilterTest1.png 0.3)
itk_add_test(NAME y$itkLabelMapOverlayImageFilterTest2
      COMMAND ITKReviewTestDriver
  --compare ${BASELINE}/itkLabelMapOverlayImageFilterTest1.png
            ${TEMP}/itkLabelMapOverlayImageFilterTest2.png
  itkLabelMapOverlayImageFilterTest2 ${INPUTDATA}/cthead1Label.png ${INPUTDATA}/cthead1.png ${TEMP}/itkLabelMapOverlayImageFilterTest2.png 0.3)
itk_add_test(NAME itkLabelMapTest
      COMMAND ITKReviewTestDriver itkLabelMapTest)
itk_add_test(NAME itkLabelMapToAttributeImageFilterTest1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapToAttributeImageFilterTest1.png
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapToAttributeImageFilterTest1.png
    itkLabelMapToAttributeImageFilterTest1 ${ITK_DATA_ROOT}/Input/SpotsLabeled.png ${ITK_TEST_OUTPUT_DIR}/itkLabelMapToAttributeImageFilterTest1.png)
itk_add_test(NAME itkLabelMapToBinaryImageFilterTest1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1-label-binary.mha
              ${ITK_TEST_OUTPUT_DIR}/cthead1-label-binary.mha
    itkLabelMapToBinaryImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1Label.png ${ITK_TEST_OUTPUT_DIR}/cthead1-label-binary.mha 255 0)
itk_add_test(NAME itkLabelMapToLabelImageFilterTest
      COMMAND ITKReviewTestDriver itkLabelMapToLabelImageFilterTest)
itk_add_test(NAME itkLabelMapToRGBImageFilterTest1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapToRGBImageFilterTest1.png
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapToRGBImageFilterTest1.png
    itkLabelMapToRGBImageFilterTest1 ${ITK_DATA_ROOT}/Input/SpotsLabeled.png ${ITK_TEST_OUTPUT_DIR}/itkLabelMapToRGBImageFilterTest1.png)
itk_add_test(NAME itkLabelMapToRGBImageFilterTest2
      COMMAND ITKReviewTestDriver
  --compare ${BASELINE}/itkLabelMapToRGBImageFilterTest1.png
            ${TEMP}/itkLabelMapToRGBImageFilterTest2.png
  itkLabelMapToRGBImageFilterTest2 ${INPUTDATA}/SpotsLabeled.png ${TEMP}/itkLabelMapToRGBImageFilterTest2.png)
itk_add_test(NAME itkLabelObjectLineComparatorTest
      COMMAND ITKReviewTestDriver itkLabelObjectLineComparatorTest)
itk_add_test(NAME itkLabelObjectLineTest
      COMMAND ITKReviewTestDriver itkLabelObjectLineTest)
itk_add_test(NAME itkLabelObjectTest
      COMMAND ITKReviewTestDriver itkLabelObjectTest)
itk_add_test(NAME itkLabelOverlayImageFilterTest-Opacity-0_0
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1-0_0Overlay.png
              ${ITK_TEST_OUTPUT_DIR}/cthead1-0_0Overlay.png
    itkLabelOverlayImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_DATA_ROOT}/Input/cthead1Label.png 0.0 ${ITK_TEST_OUTPUT_DIR}/cthead1-0_0Overlay.png)
itk_add_test(NAME itkLabelOverlayImageFilterTest-Opacity-0_1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1-0_1Overlay.png
              ${ITK_TEST_OUTPUT_DIR}/cthead1-0_1Overlay.png
    itkLabelOverlayImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_DATA_ROOT}/Input/cthead1Label.png 0.1 ${ITK_TEST_OUTPUT_DIR}/cthead1-0_1Overlay.png)
itk_add_test(NAME itkLabelOverlayImageFilterTest-Opacity-0_2
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1-0_2Overlay.png
              ${ITK_TEST_OUTPUT_DIR}/cthead1-0_2Overlay.png
    itkLabelOverlayImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_DATA_ROOT}/Input/cthead1Label.png 0.2 ${ITK_TEST_OUTPUT_DIR}/cthead1-0_2Overlay.png)
itk_add_test(NAME itkLabelOverlayImageFilterTest-Opacity-0_3
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1-0_3Overlay.png
              ${ITK_TEST_OUTPUT_DIR}/cthead1-0_3Overlay.png
    itkLabelOverlayImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_DATA_ROOT}/Input/cthead1Label.png 0.3 ${ITK_TEST_OUTPUT_DIR}/cthead1-0_3Overlay.png)
itk_add_test(NAME itkLabelOverlayImageFilterTest-Opacity-0_4
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1-0_4Overlay.png
              ${ITK_TEST_OUTPUT_DIR}/cthead1-0_4Overlay.png
    itkLabelOverlayImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_DATA_ROOT}/Input/cthead1Label.png 0.4 ${ITK_TEST_OUTPUT_DIR}/cthead1-0_4Overlay.png)
itk_add_test(NAME itkLabelOverlayImageFilterTest-Opacity-0_5
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1-0_5Overlay.png
              ${ITK_TEST_OUTPUT_DIR}/cthead1-0_5Overlay.png
    itkLabelOverlayImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_DATA_ROOT}/Input/cthead1Label.png 0.5 ${ITK_TEST_OUTPUT_DIR}/cthead1-0_5Overlay.png)
itk_add_test(NAME itkLabelOverlayImageFilterTest-Opacity-0_6
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1-0_6Overlay.png
              ${ITK_TEST_OUTPUT_DIR}/cthead1-0_6Overlay.png
    itkLabelOverlayImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_DATA_ROOT}/Input/cthead1Label.png 0.6 ${ITK_TEST_OUTPUT_DIR}/cthead1-0_6Overlay.png)
itk_add_test(NAME itkLabelOverlayImageFilterTest-Opacity-0_7
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1-0_7Overlay.png
              ${ITK_TEST_OUTPUT_DIR}/cthead1-0_7Overlay.png
    itkLabelOverlayImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_DATA_ROOT}/Input/cthead1Label.png 0.7 ${ITK_TEST_OUTPUT_DIR}/cthead1-0_7Overlay.png)
itk_add_test(NAME itkLabelOverlayImageFilterTest-Opacity-0_8
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1-0_8Overlay.png
              ${ITK_TEST_OUTPUT_DIR}/cthead1-0_8Overlay.png
    itkLabelOverlayImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_DATA_ROOT}/Input/cthead1Label.png 0.8 ${ITK_TEST_OUTPUT_DIR}/cthead1-0_8Overlay.png)
itk_add_test(NAME itkLabelOverlayImageFilterTest-Opacity-0_9
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1-0_9Overlay.png
              ${ITK_TEST_OUTPUT_DIR}/cthead1-0_9Overlay.png
    itkLabelOverlayImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_DATA_ROOT}/Input/cthead1Label.png 0.9 ${ITK_TEST_OUTPUT_DIR}/cthead1-0_9Overlay.png)
itk_add_test(NAME itkLabelOverlayImageFilterTest-Opacity-1_0
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1-1_0Overlay.png
              ${ITK_TEST_OUTPUT_DIR}/cthead1-1_0Overlay.png
    itkLabelOverlayImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_DATA_ROOT}/Input/cthead1Label.png 1.0 ${ITK_TEST_OUTPUT_DIR}/cthead1-1_0Overlay.png)
itk_add_test(NAME itkLabelSelectionLabelMapFilterTest0
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkLabelSelectionLabelMapFilterTest0.png
              ${ITK_TEST_OUTPUT_DIR}/itkLabelSelectionLabelMapFilterTest0.png
    itkLabelSelectionLabelMapFilterTest ${ITK_DATA_ROOT}/Input/cthead1Label.png ${ITK_TEST_OUTPUT_DIR}/itkLabelSelectionLabelMapFilterTest0.png 0 94)
itk_add_test(NAME itkLabelSelectionLabelMapFilterTest1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkLabelSelectionLabelMapFilterTest1.png
              ${ITK_TEST_OUTPUT_DIR}/itkLabelSelectionLabelMapFilterTest1.png
    itkLabelSelectionLabelMapFilterTest ${ITK_DATA_ROOT}/Input/cthead1Label.png ${ITK_TEST_OUTPUT_DIR}/itkLabelSelectionLabelMapFilterTest1.png 1 94)
itk_add_test(NAME itkLabelToRGBImageFilterTest
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1Label-color.png
              ${ITK_TEST_OUTPUT_DIR}/cthead1Label-color.png
    itkLabelToRGBImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1Label.png ${ITK_TEST_OUTPUT_DIR}/cthead1Label-color.png)
itk_add_test(NAME itkLabelUniqueLabelMapFilterTest0
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkLabelUniqueLabelMapFilterTest0.png
              ${ITK_TEST_OUTPUT_DIR}/itkLabelUniqueLabelMapFilterTest0.png
    itkLabelUniqueLabelMapFilterTest1 ${ITK_DATA_ROOT}/Input/SpotsLabeled.png ${ITK_TEST_OUTPUT_DIR}/itkLabelUniqueLabelMapFilterTest0.png 0)
itk_add_test(NAME itkLabelUniqueLabelMapFilterTest1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkLabelUniqueLabelMapFilterTest1.png
              ${ITK_TEST_OUTPUT_DIR}/itkLabelUniqueLabelMapFilterTest1.png
    itkLabelUniqueLabelMapFilterTest1 ${ITK_DATA_ROOT}/Input/SpotsLabeled.png ${ITK_TEST_OUTPUT_DIR}/itkLabelUniqueLabelMapFilterTest1.png 1)
itk_add_test(NAME itkMapMaskedRankImageFilterTest3
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkMaskedRankImageFilter3.png
              ${ITK_TEST_OUTPUT_DIR}/itkMapMaskedRankImageFilter3.png
    itkMapMaskedRankImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_DATA_ROOT}/Input/cthead1-mask.png ${ITK_TEST_OUTPUT_DIR}/itkMapMaskedRankImageFilter3.png 3)
itk_add_test(NAME itkMapMaskedRankImageFilterTest10
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkMaskedRankImageFilter10.png
              ${ITK_TEST_OUTPUT_DIR}/itkMapMaskedRankImageFilter10.png
    itkMapMaskedRankImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_DATA_ROOT}/Input/cthead1-mask.png ${ITK_TEST_OUTPUT_DIR}/itkMapMaskedRankImageFilter10.png 10)
itk_add_test(NAME itkMapRankImageFilterTest3
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkRankImageFilter3.png
              ${ITK_TEST_OUTPUT_DIR}/itkMapRankImageFilter3.png
    itkMapRankImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkMapRankImageFilter3.png 3)
itk_add_test(NAME itkMapRankImageFilterTest10
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkRankImageFilter10.png
              ${ITK_TEST_OUTPUT_DIR}/itkMapRankImageFilter10.png
    itkMapRankImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkMapRankImageFilter10.png 10)
itk_add_test(NAME itkMaskedRankImageFilterTest3
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkMaskedRankImageFilter3.png
              ${ITK_TEST_OUTPUT_DIR}/itkMaskedRankImageFilter3.png
    itkMaskedRankImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_DATA_ROOT}/Input/cthead1-mask.png ${ITK_TEST_OUTPUT_DIR}/itkMaskedRankImageFilter3.png 3)
itk_add_test(NAME itkMaskedRankImageFilterTest10
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkMaskedRankImageFilter10.png
              ${ITK_TEST_OUTPUT_DIR}/itkMaskedRankImageFilter10.png
    itkMaskedRankImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_DATA_ROOT}/Input/cthead1-mask.png ${ITK_TEST_OUTPUT_DIR}/itkMaskedRankImageFilter10.png 10)
itk_add_test(NAME itkMergeLabelMapFilterTest1a
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/simple-label-merge-keep.mha
              ${ITK_TEST_OUTPUT_DIR}/simple-label-merge-keep.mha
    itkMergeLabelMapFilterTest1 ${ITK_DATA_ROOT}/Input/simple-label-c.png ${ITK_DATA_ROOT}/Input/simple-label-d.png ${ITK_TEST_OUTPUT_DIR}/simple-label-merge-keep.mha 50 50 0 0)
itk_add_test(NAME itkMergeLabelMapFilterTest1b
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/simple-label-merge-aggregate.mha
              ${ITK_TEST_OUTPUT_DIR}/simple-label-merge-aggregate.mha
    itkMergeLabelMapFilterTest1 ${ITK_DATA_ROOT}/Input/simple-label-c.png ${ITK_DATA_ROOT}/Input/simple-label-d.png ${ITK_TEST_OUTPUT_DIR}/simple-label-merge-aggregate.mha 50 50 1 0)
itk_add_test(NAME itkMergeLabelMapFilterTest1c
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/simple-label-merge-pack.mha
              ${ITK_TEST_OUTPUT_DIR}/simple-label-merge-pack.mha
    itkMergeLabelMapFilterTest1 ${ITK_DATA_ROOT}/Input/simple-label-c.png ${ITK_DATA_ROOT}/Input/simple-label-d.png ${ITK_TEST_OUTPUT_DIR}/simple-label-merge-pack.mha 50 50 2 0)
itk_add_test(NAME itkMergeLabelMapFilterTest1d
      COMMAND ITKReviewTestDriver itkMergeLabelMapFilterTest1
              ${ITK_DATA_ROOT}/Input/simple-label-c.png ${ITK_DATA_ROOT}/Input/simple-label-d.png ${ITK_TEST_OUTPUT_DIR}/thisFileShouldNotExist.mha 50 50 3 1)
itk_add_test(NAME itkMergeLabelMapFilterTest1e
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/simple-label-merge-strict.mha
              ${ITK_TEST_OUTPUT_DIR}/simple-label-merge-strict.mha
    itkMergeLabelMapFilterTest1 ${ITK_DATA_ROOT}/Input/simple-label-e.png ${ITK_DATA_ROOT}/Input/simple-label-d.png ${ITK_TEST_OUTPUT_DIR}/simple-label-merge-strict.mha 50 50 3 0)
itk_add_test(NAME itkMergeLabelMapFilterTest1f
      COMMAND ITKReviewTestDriver itkMergeLabelMapFilterTest1
              ${ITK_DATA_ROOT}/Input/simple-label-e.png ${ITK_DATA_ROOT}/Input/simple-label-d.png ${ITK_TEST_OUTPUT_DIR}/simple-label-merge-wrong.mha 50 50 10 1)
itk_add_test(NAME itkMorphologicalWatershedFromMarkersImageFilterTestM0F0
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkMorphologicalWatershedFromMarkersImageFilterTestM0F0.png
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedFromMarkersImageFilterTestM0F0.png
    itkMorphologicalWatershedFromMarkersImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_DATA_ROOT}/Input/cthead1-markers.png ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedFromMarkersImageFilterTestM0F0.png 0 0)
itk_add_test(NAME itkMorphologicalWatershedFromMarkersImageFilterTestM0F1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkMorphologicalWatershedFromMarkersImageFilterTestM0F1.png
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedFromMarkersImageFilterTestM0F1.png
    itkMorphologicalWatershedFromMarkersImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_DATA_ROOT}/Input/cthead1-markers.png ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedFromMarkersImageFilterTestM0F1.png 0 1)
itk_add_test(NAME itkMorphologicalWatershedFromMarkersImageFilterTestM1F0
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkMorphologicalWatershedFromMarkersImageFilterTestM1F0.png
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedFromMarkersImageFilterTestM1F0.png
    itkMorphologicalWatershedFromMarkersImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_DATA_ROOT}/Input/cthead1-markers.png ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedFromMarkersImageFilterTestM1F0.png 1 0)
itk_add_test(NAME itkMorphologicalWatershedFromMarkersImageFilterTestM1F1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkMorphologicalWatershedFromMarkersImageFilterTestM1F1.png
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedFromMarkersImageFilterTestM1F1.png
    itkMorphologicalWatershedFromMarkersImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_DATA_ROOT}/Input/cthead1-markers.png ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedFromMarkersImageFilterTestM1F1.png 1 1)
itk_add_test(NAME itkMorphologicalWatershedImageFilterTestButtonHoleM0F0
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkMorphologicalWatershedImageFilterTestButtonHoleM0F0.png
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestButtonHoleM0F0.png
    itkMorphologicalWatershedImageFilterTest ${ITK_DATA_ROOT}/Input/button-hole.png ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestButtonHoleM0F0.png 0 0 0)
itk_add_test(NAME itkMorphologicalWatershedImageFilterTestPassValuesM0F0
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkMorphologicalWatershedImageFilterTestPassValuesM0F0.png
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestPassValuesM0F0.png
    itkMorphologicalWatershedImageFilterTest ${ITK_DATA_ROOT}/Input/pass-values.png ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestPassValuesM0F0.png 0 0 0)
itk_add_test(NAME itkMorphologicalWatershedImageFilterTestPlateauM0F0
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkMorphologicalWatershedImageFilterTestPlateauM0F0.png
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestPlateauM0F0.png
    itkMorphologicalWatershedImageFilterTest ${ITK_DATA_ROOT}/Input/plateau.png ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestPlateauM0F0.png 0 0 0)
itk_add_test(NAME itkMorphologicalWatershedImageFilterTestThickLinesM0F0
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkMorphologicalWatershedImageFilterTestThickLinesM0F0.png
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestThickLinesM0F0.png
    itkMorphologicalWatershedImageFilterTest ${ITK_DATA_ROOT}/Input/thick-lines.png ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestThickLinesM0F0.png 0 0 0)
itk_add_test(NAME itkMorphologicalWatershedImageFilterTestButtonHoleM0F1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkMorphologicalWatershedImageFilterTestButtonHoleM0F1.png
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestButtonHoleM0F1.png
    itkMorphologicalWatershedImageFilterTest ${ITK_DATA_ROOT}/Input/button-hole.png ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestButtonHoleM0F1.png 0 1 0)
itk_add_test(NAME itkMorphologicalWatershedImageFilterTestPassValuesM0F1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkMorphologicalWatershedImageFilterTestPassValuesM0F1.png
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestPassValuesM0F1.png
    itkMorphologicalWatershedImageFilterTest ${ITK_DATA_ROOT}/Input/pass-values.png ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestPassValuesM0F1.png 0 1 0)
itk_add_test(NAME itkMorphologicalWatershedImageFilterTestPlateauM0F1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkMorphologicalWatershedImageFilterTestPlateauM0F1.png
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestPlateauM0F1.png
    itkMorphologicalWatershedImageFilterTest ${ITK_DATA_ROOT}/Input/plateau.png ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestPlateauM0F1.png 0 1 0)
itk_add_test(NAME itkMorphologicalWatershedImageFilterTestThickLinesM0F1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkMorphologicalWatershedImageFilterTestThickLinesM0F1.png
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestThickLinesM0F1.png
    itkMorphologicalWatershedImageFilterTest ${ITK_DATA_ROOT}/Input/thick-lines.png ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestThickLinesM0F1.png 0 1 0)
itk_add_test(NAME itkMorphologicalWatershedImageFilterTestButtonHoleM1F0
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkMorphologicalWatershedImageFilterTestButtonHoleM1F0.png
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestButtonHoleM1F0.png
    itkMorphologicalWatershedImageFilterTest ${ITK_DATA_ROOT}/Input/button-hole.png ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestButtonHoleM1F0.png 1 0 0)
itk_add_test(NAME itkMorphologicalWatershedImageFilterTestPassValuesM1F0
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkMorphologicalWatershedImageFilterTestPassValuesM1F0.png
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestPassValuesM1F0.png
    itkMorphologicalWatershedImageFilterTest ${ITK_DATA_ROOT}/Input/pass-values.png ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestPassValuesM1F0.png 1 0 0)
itk_add_test(NAME itkMorphologicalWatershedImageFilterTestPlateauM1F0
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkMorphologicalWatershedImageFilterTestPlateauM1F0.png
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestPlateauM1F0.png
    itkMorphologicalWatershedImageFilterTest ${ITK_DATA_ROOT}/Input/plateau.png ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestPlateauM1F0.png 1 0 0)
itk_add_test(NAME itkMorphologicalWatershedImageFilterTestThickLinesM1F0
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkMorphologicalWatershedImageFilterTestThickLinesM1F0.png
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestThickLinesM1F0.png
    itkMorphologicalWatershedImageFilterTest ${ITK_DATA_ROOT}/Input/thick-lines.png ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestThickLinesM1F0.png 1 0 0)
itk_add_test(NAME itkMorphologicalWatershedImageFilterTestButtonHoleM1F1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkMorphologicalWatershedImageFilterTestButtonHoleM1F1.png
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestButtonHoleM1F1.png
    itkMorphologicalWatershedImageFilterTest ${ITK_DATA_ROOT}/Input/button-hole.png ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestButtonHoleM1F1.png 1 1 0)
itk_add_test(NAME itkMorphologicalWatershedImageFilterTestPassValuesM1F1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkMorphologicalWatershedImageFilterTestPassValuesM1F1.png
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestPassValuesM1F1.png
    itkMorphologicalWatershedImageFilterTest ${ITK_DATA_ROOT}/Input/pass-values.png ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestPassValuesM1F1.png 1 1 0)
itk_add_test(NAME itkMorphologicalWatershedImageFilterTestPlateauM1F1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkMorphologicalWatershedImageFilterTestPlateauM1F1.png
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestPlateauM1F1.png
    itkMorphologicalWatershedImageFilterTest ${ITK_DATA_ROOT}/Input/plateau.png ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestPlateauM1F1.png 1 1 0)
itk_add_test(NAME itkMorphologicalWatershedImageFilterTestThickLinesM1F1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkMorphologicalWatershedImageFilterTestThickLinesM1F1.png
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestThickLinesM1F1.png
    itkMorphologicalWatershedImageFilterTest ${ITK_DATA_ROOT}/Input/thick-lines.png ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestThickLinesM1F1.png 1 1 0)
itk_add_test(NAME itkMorphologicalWatershedImageFilterTestLevel00
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkMorphologicalWatershedImageFilterTestLevel00.png
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestLevel00.png
    itkMorphologicalWatershedImageFilterTest ${ITK_DATA_ROOT}/Input/level.png ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestLevel00.png 1 0 00)
itk_add_test(NAME itkMorphologicalWatershedImageFilterTestLevel10
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkMorphologicalWatershedImageFilterTestLevel10.png
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestLevel10.png
    itkMorphologicalWatershedImageFilterTest ${ITK_DATA_ROOT}/Input/level.png ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestLevel10.png 1 0 10)
itk_add_test(NAME itkMorphologicalWatershedImageFilterTestLevel20
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkMorphologicalWatershedImageFilterTestLevel20.png
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestLevel20.png
    itkMorphologicalWatershedImageFilterTest ${ITK_DATA_ROOT}/Input/level.png ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestLevel20.png 1 0 20)
itk_add_test(NAME itkMorphologicalWatershedImageFilterTestLevel30
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkMorphologicalWatershedImageFilterTestLevel30.png
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestLevel30.png
    itkMorphologicalWatershedImageFilterTest ${ITK_DATA_ROOT}/Input/level.png ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestLevel30.png 1 0 30)
itk_add_test(NAME itkMorphologicalWatershedImageFilterTestLevel40
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkMorphologicalWatershedImageFilterTestLevel40.png
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestLevel40.png
    itkMorphologicalWatershedImageFilterTest ${ITK_DATA_ROOT}/Input/level.png ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestLevel40.png 1 0 40)
itk_add_test(NAME itkMorphologicalWatershedImageFilterTestLevel50
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkMorphologicalWatershedImageFilterTestLevel50.png
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestLevel50.png
    itkMorphologicalWatershedImageFilterTest ${ITK_DATA_ROOT}/Input/level.png ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestLevel50.png 1 0 50)
itk_add_test(NAME itkMRCImageIOTest
      COMMAND ITKReviewTestDriver itkMRCImageIOTest
              ${ITK_TEST_OUTPUT_DIR})
itk_add_test(NAME itkMultiphaseDenseFiniteDifferenceImageFilterTest
      COMMAND ITKReviewTestDriver itkMultiphaseDenseFiniteDifferenceImageFilterTest)
itk_add_test(NAME itkMultiphaseFiniteDifferenceImageFilterTest
      COMMAND ITKReviewTestDriver itkMultiphaseFiniteDifferenceImageFilterTest)
itk_add_test(NAME itkMultiphaseSparseFiniteDifferenceImageFilterTest
      COMMAND ITKReviewTestDriver itkMultiphaseSparseFiniteDifferenceImageFilterTest)
itk_add_test(NAME itkMultiplyByConstantImageFilterTest
      COMMAND ITKReviewTestDriver itkMultiplyByConstantImageFilterTest)
itk_add_test(NAME itkMultiScaleHessianBasedMeasureImageFilterTest
      COMMAND ITKReviewTestDriver itkMultiScaleHessianBasedMeasureImageFilterTest
              ${ITK_DATA_ROOT}/Input/DSA.png ${ITK_TEST_OUTPUT_DIR}/itkMultiScaleHessianBasedMeasureImageFilterTestEnhancedOutput.mha ${ITK_TEST_OUTPUT_DIR}/itkMultiScaleHessianBasedMeasureImageFilterTestScalesOutput.mha 5 10 10 1 0 ${ITK_TEST_OUTPUT_DIR}/itkMultiScaleHessianBasedMeasureImageFilterTestEnhancedOutput2.mha)
itk_add_test(NAME itkN4MRIBiasFieldCorrectionImageFilterTest1
      COMMAND ITKReviewTestDriver
    --compare ${BASELINE}/N4ControlPoints_2D.nii.gz
              ${TEMP}/N4ControlPoints_2D.nii.gz
    itkN4MRIBiasFieldCorrectionImageFilterTest 2
    ${ITK_EXAMPLE_DATA_ROOT}/BrainT1SliceBorder20.png    # input
    ${TEMP}/N4ControlPoints_2D.nii.gz                           # control point lattice
    4                                                           # shrink factor
    20x20                                                       # number of iterations
    none                                                        # mask
    200                                                         # spline distance
    )
itk_add_test(NAME itkN4MRIBiasFieldCorrectionImageFilterTest2
      COMMAND ITKReviewTestDriver
    --compare ${BASELINE}/N4ControlPoints_3D.nii.gz
              ${TEMP}/N4ControlPoints_3D.nii.gz
    itkN4MRIBiasFieldCorrectionImageFilterTest
    3
    ${ITK_DATA_ROOT}/Input/HeadMRVolumeCompressed.mha    # input
    ${TEMP}/N4ControlPoints_3D.nii.gz                                  # control point lattice
    3                                                                  # shrink factor
    10x10x10                                                           # number of iterations
    none                                                               # mask
    150                                                                # spline distance
    )
itk_add_test(NAME itkNeuralNetworkIOTest
      COMMAND ITKReviewTestDriver itkNeuralNetworkIOTest
              ${ITK_DATA_ROOT}/Input/xornet.txt ${ITK_DATA_ROOT}/Input/xortest.txt ${ITK_TEST_OUTPUT_DIR})
itk_add_test(NAME itkObjectByObjectLabelMapFilterTest0
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkObjectByObjectLabelMapFilterTest0.png
              ${ITK_TEST_OUTPUT_DIR}/itkObjectByObjectLabelMapFilterTest0.png
    itkObjectByObjectLabelMapFilterTest ${ITK_DATA_ROOT}/Input/SpotsLabeled.png ${ITK_TEST_OUTPUT_DIR}/itkObjectByObjectLabelMapFilterTest0.png 0)
itk_add_test(NAME itkObjectByObjectLabelMapFilterTest1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkObjectByObjectLabelMapFilterTest1.png
              ${ITK_TEST_OUTPUT_DIR}/itkObjectByObjectLabelMapFilterTest1.png
    itkObjectByObjectLabelMapFilterTest ${ITK_DATA_ROOT}/Input/SpotsLabeled.png ${ITK_TEST_OUTPUT_DIR}/itkObjectByObjectLabelMapFilterTest1.png 1)
itk_add_test(NAME itkObjectToObjectMetricTest
      COMMAND ITKReviewTestDriver itkObjectToObjectMetricTest)
itk_add_test(NAME itkOptImageToImageMetricsTest01
      COMMAND ITKReviewTestDriver itkOptImageToImageMetricsTest
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png)
itk_add_test(NAME itkOptImageToImageMetricsTest02
      COMMAND ITKReviewTestDriver --with-threads 2
              itkOptImageToImageMetricsTest
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png)
itk_add_test(NAME itkOptImageToImageMetricsTest03
      COMMAND ITKReviewTestDriver --without-threads
              itkOptImageToImageMetricsTest
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png)
itk_add_test(NAME itkOptImageToImageMetricsTest04
      COMMAND ITKReviewTestDriver itkOptImageToImageMetricsTest2
              ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_DATA_ROOT}/Baseline/Common/itkBSplineDeformableTransformTest4.png)
itk_add_test(NAME itkOptMattesImageToImageMetricsTest01
      COMMAND ITKReviewTestDriver itkOptMattesMutualInformationImageToImageMetricThreadsTest1
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png 0 100)
itk_add_test(NAME itkPadLabelMapFilterTest1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1-label-pad.mha
              ${ITK_TEST_OUTPUT_DIR}/cthead1-label-pad.mha
    itkPadLabelMapFilterTest1 ${ITK_DATA_ROOT}/Input/cthead1Label.png ${ITK_TEST_OUTPUT_DIR}/cthead1-label-pad.mha 40 50)
itk_add_test(NAME itkPointsLocatorTest
      COMMAND ITKReviewTestDriver itkPointsLocatorTest)
itk_add_test(NAME itkRankImageFilterTest3
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkRankImageFilter3.png
              ${ITK_TEST_OUTPUT_DIR}/itkRankImageFilter3.png
    itkRankImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkRankImageFilter3.png 3)
itk_add_test(NAME itkRankImageFilterTest10
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkRankImageFilter10.png
              ${ITK_TEST_OUTPUT_DIR}/itkRankImageFilter10.png
    itkRankImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkRankImageFilter10.png 10)
itk_add_test(NAME itkRegionalMinimaImageFilterTest2_1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1RegionalMinimal-ref2_1.png
              ${ITK_TEST_OUTPUT_DIR}/cthead1RegionalMinimal2_1.png
    itkRegionalMaximaImageFilterTest2 1 1 ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/cthead1RegionalMinimal2_1.png ${ITK_TEST_OUTPUT_DIR}/cthead1RegionalMinimal-ref2_1.png)
itk_add_test(NAME itkRegionalMinimaImageFilterTest2_2
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1RegionalMinimal-ref2_2.png
              ${ITK_TEST_OUTPUT_DIR}/cthead1RegionalMinimal2_2.png
    itkRegionalMaximaImageFilterTest2 0 1 ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/cthead1RegionalMinimal2_2.png ${ITK_TEST_OUTPUT_DIR}/cthead1RegionalMinimal-ref2_2.png)
itk_add_test(NAME itkRegionalMaximaImageFilterTest2_1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1RegionalMaximal-ref2_1.png
              ${ITK_TEST_OUTPUT_DIR}/cthead1RegionalMaximal2_1.png
    itkRegionalMaximaImageFilterTest2 1 1 ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/cthead1RegionalMaximal2_1.png ${ITK_TEST_OUTPUT_DIR}/cthead1RegionalMaximal-ref2_1.png)
itk_add_test(NAME itkRegionalMaximaImageFilterTest2_2
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1RegionalMaximal-ref2_2.png
              ${ITK_TEST_OUTPUT_DIR}/cthead1RegionalMaximal2_2.png
    itkRegionalMaximaImageFilterTest2 0 1 ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/cthead1RegionalMaximal2_2.png ${ITK_TEST_OUTPUT_DIR}/cthead1RegionalMaximal-ref2_2.png)
itk_add_test(NAME itkRegionBasedLevelSetFunctionTest
      COMMAND ITKReviewTestDriver itkRegionBasedLevelSetFunctionTest)
itk_add_test(NAME itkRegionFromReferenceLabelMapFilterTest1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1-label-regionreference.mha
              ${ITK_TEST_OUTPUT_DIR}/cthead1-label-regionreference.mha
    itkRegionFromReferenceLabelMapFilterTest1 ${ITK_DATA_ROOT}/Input/cthead1Label.png ${ITK_DATA_ROOT}/Input/circle.png ${ITK_TEST_OUTPUT_DIR}/cthead1-label-regionreference.mha)
itk_add_test(NAME itkRelabelLabelMapFilterTest1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1-label-relabeled.mha
              ${ITK_TEST_OUTPUT_DIR}/cthead1-label-relabeled.mha
    itkRelabelLabelMapFilterTest1 ${ITK_DATA_ROOT}/Input/cthead1Label.png ${ITK_TEST_OUTPUT_DIR}/cthead1-label-relabeled.mha)
itk_add_test(NAME itkRobustAutomaticThresholdImageFilterTest
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkRobustAutomaticThresholdImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/itkRobustAutomaticThresholdImageFilterTest.png
    itkRobustAutomaticThresholdImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkRobustAutomaticThresholdImageFilterTest.png 2)
itk_add_test(NAME itkScalarChanAndVeseDenseLevelSetImageFilterTest1
      COMMAND ITKReviewTestDriver itkScalarChanAndVeseDenseLevelSetImageFilterTest1)
itk_add_test(NAME itkScalarChanAndVeseDenseLevelSetImageFilterTest2_0
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkScalarChanAndVeseDenseLevelSetImageFilterTest2_0.mha
              ${ITK_TEST_OUTPUT_DIR}/itkScalarChanAndVeseDenseLevelSetImageFilterTest2_0.mha
    itkScalarChanAndVeseDenseLevelSetImageFilterTest2 ${ITK_DATA_ROOT}/Baseline/Segmentation/ShapeDetectionLevelSetLeftVentricleTest.png ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySlice.png ${ITK_TEST_OUTPUT_DIR}/itkScalarChanAndVeseDenseLevelSetImageFilterTest2_0.mha 1 1 0 0 0)
itk_add_test(NAME itkScalarChanAndVeseDenseLevelSetImageFilterTest2_1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkScalarChanAndVeseDenseLevelSetImageFilterTest2_1.mha
              ${ITK_TEST_OUTPUT_DIR}/itkScalarChanAndVeseDenseLevelSetImageFilterTest2_1.mha
    itkScalarChanAndVeseDenseLevelSetImageFilterTest2 ${ITK_DATA_ROOT}/Baseline/Segmentation/ShapeDetectionLevelSetLeftVentricleTest.png ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySlice.png ${ITK_TEST_OUTPUT_DIR}/itkScalarChanAndVeseDenseLevelSetImageFilterTest2_1.mha 1 1 1 1 200)
itk_add_test(NAME itkScalarChanAndVeseDenseLevelSetImageFilterTest2_2
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkScalarChanAndVeseDenseLevelSetImageFilterTest2_2.mha
              ${ITK_TEST_OUTPUT_DIR}/itkScalarChanAndVeseDenseLevelSetImageFilterTest2_2.mha
    itkScalarChanAndVeseDenseLevelSetImageFilterTest2 ${ITK_DATA_ROOT}/Baseline/Segmentation/ShapeDetectionLevelSetLeftVentricleTest.png ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySlice.png ${ITK_TEST_OUTPUT_DIR}/itkScalarChanAndVeseDenseLevelSetImageFilterTest2_2.mha 0 0 1 0 0)
itk_add_test(NAME itkScalarChanAndVeseDenseLevelSetImageFilterTest2_3
      COMMAND ITKReviewTestDriver itkScalarChanAndVeseDenseLevelSetImageFilterTest2
              ${ITK_DATA_ROOT}/Baseline/Segmentation/ShapeDetectionLevelSetLeftVentricleTest.png ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySlice.png ${ITK_TEST_OUTPUT_DIR}/itkScalarChanAndVeseDenseLevelSetImageFilterTest2_3.mha 0 0 0 0 0)
itk_add_test(NAME itkScalarChanAndVeseDenseLevelSetImageFilterTest3
      COMMAND ITKReviewTestDriver itkScalarChanAndVeseDenseLevelSetImageFilterTest3)
itk_add_test(NAME itkScalarChanAndVeseDenseLevelSetImageFilterTest4
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkScalarChanAndVeseDenseLevelSetImageFilterTest2_0.mha
              ${ITK_TEST_OUTPUT_DIR}/itkScalarChanAndVeseDenseLevelSetImageFilterTest4.mha
    itkScalarChanAndVeseDenseLevelSetImageFilterTest4 ${ITK_DATA_ROOT}/Baseline/Segmentation/ShapeDetectionLevelSetLeftVentricleTest.png ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySlice.png ${ITK_TEST_OUTPUT_DIR}/itkScalarChanAndVeseDenseLevelSetImageFilterTest4.mha 1 1 0 0 0)
itk_add_test(NAME itkScalarChanAndVeseLevelSetFunctionTest1
      COMMAND ITKReviewTestDriver itkScalarChanAndVeseLevelSetFunctionTest1)
itk_add_test(NAME itkScalarChanAndVeseLevelSetFunctionTest2
      COMMAND ITKReviewTestDriver itkScalarChanAndVeseLevelSetFunctionTest2)
itk_add_test(NAME itkScalarChanAndVeseSparseLevelSetImageFilterTest1
      COMMAND ITKReviewTestDriver itkScalarChanAndVeseSparseLevelSetImageFilterTest1)
itk_add_test(NAME itkScalarChanAndVeseSparseLevelSetImageFilterTest2
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkScalarChanAndVeseSparseLevelSetImageFilterTest2.mha
              ${ITK_TEST_OUTPUT_DIR}/itkScalarChanAndVeseSparseLevelSetImageFilterTest2.mha
    itkScalarChanAndVeseSparseLevelSetImageFilterTest2 ${ITK_DATA_ROOT}/Baseline/Segmentation/ShapeDetectionLevelSetLeftVentricleTest.png ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySlice.png ${ITK_TEST_OUTPUT_DIR}/itkScalarChanAndVeseSparseLevelSetImageFilterTest2.mha)
itk_add_test(NAME itkScalarRegionBasedLevelSetFunctionTest
      COMMAND ITKReviewTestDriver itkScalarRegionBasedLevelSetFunctionTest)
itk_add_test(NAME RGBColormapTest_red
      COMMAND ITKReviewTestDriver
    --compare ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_red.png
              ${ITK_DATA_ROOT}/Baseline/Review/RGBColormapTest_red.png
    itkScalarToRGBColormapImageFilterTest ${ITK_DATA_ROOT}/Input/Colormap_Grey.png ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_red.png red)
itk_add_test(NAME RGBColormapTest_green
      COMMAND ITKReviewTestDriver
    --compare ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_green.png
              ${ITK_DATA_ROOT}/Baseline/Review/RGBColormapTest_green.png
    itkScalarToRGBColormapImageFilterTest ${ITK_DATA_ROOT}/Input/Colormap_Grey.png ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_green.png green)
itk_add_test(NAME RGBColormapTest_blue
      COMMAND ITKReviewTestDriver
    --compare ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_blue.png
              ${ITK_DATA_ROOT}/Baseline/Review/RGBColormapTest_blue.png
    itkScalarToRGBColormapImageFilterTest ${ITK_DATA_ROOT}/Input/Colormap_Grey.png ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_blue.png blue)
itk_add_test(NAME RGBColormapTest_hot
      COMMAND ITKReviewTestDriver
    --compare ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_hot.png
              ${ITK_DATA_ROOT}/Baseline/Review/RGBColormapTest_hot.png
    itkScalarToRGBColormapImageFilterTest ${ITK_DATA_ROOT}/Input/Colormap_Grey.png ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_hot.png hot)
itk_add_test(NAME RGBColormapTest_cool
      COMMAND ITKReviewTestDriver
    --compare ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_cool.png
              ${ITK_DATA_ROOT}/Baseline/Review/RGBColormapTest_cool.png
    itkScalarToRGBColormapImageFilterTest ${ITK_DATA_ROOT}/Input/Colormap_Grey.png ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_cool.png cool)
itk_add_test(NAME RGBColormapTest_spring
      COMMAND ITKReviewTestDriver
    --compare ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_spring.png
              ${ITK_DATA_ROOT}/Baseline/Review/RGBColormapTest_spring.png
    itkScalarToRGBColormapImageFilterTest ${ITK_DATA_ROOT}/Input/Colormap_Grey.png ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_spring.png spring)
itk_add_test(NAME RGBColormapTest_summer
      COMMAND ITKReviewTestDriver
    --compare ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_summer.png
              ${ITK_DATA_ROOT}/Baseline/Review/RGBColormapTest_summer.png
    itkScalarToRGBColormapImageFilterTest ${ITK_DATA_ROOT}/Input/Colormap_Grey.png ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_summer.png summer)
itk_add_test(NAME RGBColormapTest_autumn
      COMMAND ITKReviewTestDriver
    --compare ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_autumn.png
              ${ITK_DATA_ROOT}/Baseline/Review/RGBColormapTest_autumn.png
    itkScalarToRGBColormapImageFilterTest ${ITK_DATA_ROOT}/Input/Colormap_Grey.png ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_autumn.png autumn)
itk_add_test(NAME RGBColormapTest_winter
      COMMAND ITKReviewTestDriver
    --compare ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_winter.png
              ${ITK_DATA_ROOT}/Baseline/Review/RGBColormapTest_winter.png
    itkScalarToRGBColormapImageFilterTest ${ITK_DATA_ROOT}/Input/Colormap_Grey.png ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_winter.png winter)
itk_add_test(NAME RGBColormapTest_copper
      COMMAND ITKReviewTestDriver
    --compare ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_copper.png
              ${ITK_DATA_ROOT}/Baseline/Review/RGBColormapTest_copper.png
    itkScalarToRGBColormapImageFilterTest ${ITK_DATA_ROOT}/Input/Colormap_Grey.png ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_copper.png copper)
itk_add_test(NAME RGBColormapTest_hsv
      COMMAND ITKReviewTestDriver
    --compare ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_hsv.png
              ${ITK_DATA_ROOT}/Baseline/Review/RGBColormapTest_hsv.png
    itkScalarToRGBColormapImageFilterTest ${ITK_DATA_ROOT}/Input/Colormap_Grey.png ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_hsv.png hsv)
itk_add_test(NAME RGBColormapTest_jet
      COMMAND ITKReviewTestDriver
    --compare ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_jet.png
              ${ITK_DATA_ROOT}/Baseline/Review/RGBColormapTest_jet.png
    itkScalarToRGBColormapImageFilterTest ${ITK_DATA_ROOT}/Input/Colormap_Grey.png ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_jet.png jet)
itk_add_test(NAME RGBColormapTest_overunder
      COMMAND ITKReviewTestDriver
    --compare ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_overunder.png
              ${ITK_DATA_ROOT}/Baseline/Review/RGBColormapTest_overunder.png
    itkScalarToRGBColormapImageFilterTest ${ITK_DATA_ROOT}/Input/Colormap_Grey.png ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_overunder.png overunder)
itk_add_test(NAME itkShapedFloodFilledImageFunctionConditionalConstIteratorTest1
      COMMAND ITKReviewTestDriver itkShapedFloodFilledImageFunctionConditionalConstIteratorTest1
              ${ITK_DATA_ROOT}/Input/8ConnectedImage.bmp)
itk_add_test(NAME itkShapedFloodFilledImageFunctionConditionalConstIteratorTest2
      COMMAND ITKReviewTestDriver itkShapedFloodFilledImageFunctionConditionalConstIteratorTest2)
itk_add_test(NAME itkShapedFloodFilledImageFunctionConditionalConstIteratorTest3
      COMMAND ITKReviewTestDriver itkShapedFloodFilledImageFunctionConditionalConstIteratorTest3)
itk_add_test(NAME itkShapeKeepNObjectsLabelMapFilterTest1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1-keep-n-objects.mha
              ${ITK_TEST_OUTPUT_DIR}/cthead1-shape-keep-n-objects.mha
    itkShapeKeepNObjectsLabelMapFilterTest1 ${ITK_DATA_ROOT}/Input/cthead1Label.png ${ITK_TEST_OUTPUT_DIR}/cthead1-shape-keep-n-objects.mha 0 0 2)
itk_add_test(NAME itkShapeLabelObjectAccessorsTest1
      COMMAND ITKReviewTestDriver itkShapeLabelObjectAccessorsTest1
              ${ITK_DATA_ROOT}/Input/cthead1Label.png)
itk_add_test(NAME itkShapeOpeningLabelMapFilterTest1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1-shape-opening.mha
              ${ITK_TEST_OUTPUT_DIR}/cthead1-shape-opening.mha
    itkShapeOpeningLabelMapFilterTest1 ${ITK_DATA_ROOT}/Input/cthead1Label.png ${ITK_TEST_OUTPUT_DIR}/cthead1-shape-opening.mha 140 1 0)
itk_add_test(NAME itkShapePositionLabelMapFilterTest1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkShapePositionLabelMapFilterTest1.png
              ${ITK_TEST_OUTPUT_DIR}/itkShapePositionLabelMapFilterTest1.png
    itkShapePositionLabelMapFilterTest1 ${ITK_DATA_ROOT}/Input/SpotsLabeled.png ${ITK_TEST_OUTPUT_DIR}/itkShapePositionLabelMapFilterTest1.png Centroid)
itk_add_test(NAME itkShapeRelabelLabelMapFilterTest1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1-shape-relabel-labelmap.mha
              ${ITK_TEST_OUTPUT_DIR}/cthead1-shape-relabel-labelmap.mha
    itkShapeRelabelLabelMapFilterTest1 ${ITK_DATA_ROOT}/Input/cthead1Label.png ${ITK_TEST_OUTPUT_DIR}/cthead1-shape-relabel-labelmap.mha 1 0)
itk_add_test(NAME itkShiftScaleLabelMapFilterTest1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1-label-shiftscaled.mha
              ${ITK_TEST_OUTPUT_DIR}/cthead1-label-shiftscaled.mha
    itkShiftScaleLabelMapFilterTest1 ${ITK_DATA_ROOT}/Input/cthead1Label.png ${ITK_TEST_OUTPUT_DIR}/cthead1-label-shiftscaled.mha 10 0.5 true)
itk_add_test(NAME itkSliceBySliceImageFilterTest
      COMMAND ITKReviewTestDriver itkSliceBySliceImageFilterTest
              ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/itkSliceBySliceImageFilterTest.mha)
itk_add_test(NAME itkStatisticsPositionLabelMapFilterTest1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkShapePositionLabelMapFilterTest1.png
              ${ITK_TEST_OUTPUT_DIR}/itkStatisticsPositionLabelMapFilterTest1.png
    itkStatisticsPositionLabelMapFilterTest1 ${ITK_DATA_ROOT}/Input/SpotsLabeled.png ${ITK_DATA_ROOT}/Input/SpotsLabeled.png ${ITK_TEST_OUTPUT_DIR}/itkStatisticsPositionLabelMapFilterTest1.png CenterOfGravity)
itk_add_test(NAME itkStochasticFractalDimensionImageFilterTest1
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkStochasticFractalDimensionImageFilterTest1.mha
              ${ITK_TEST_OUTPUT_DIR}/itkStochasticFractalDimensionImageFilterTest1.mha
    itkStochasticFractalDimensionImageFilterTest 2 ${ITK_DATA_ROOT}/Input/TreeBarkTexture.png ${ITK_TEST_OUTPUT_DIR}/itkStochasticFractalDimensionImageFilterTest1.mha 2)
itk_add_test(NAME itkStochasticFractalDimensionImageFilterTest2
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkStochasticFractalDimensionImageFilterTest2.mha
              ${ITK_TEST_OUTPUT_DIR}/itkStochasticFractalDimensionImageFilterTest2.mha
    itkStochasticFractalDimensionImageFilterTest 2 ${ITK_DATA_ROOT}/Input/TreeBarkTexture.png ${ITK_TEST_OUTPUT_DIR}/itkStochasticFractalDimensionImageFilterTest2.mha 2 ${ITK_DATA_ROOT}/Input/circle100.png 255)
itk_add_test(NAME itkSubtractConstantFromImageFilterTest
      COMMAND ITKReviewTestDriver itkSubtractConstantFromImageFilterTest)
itk_add_test(NAME itkTimeAndMemoryProbeTest1
      COMMAND ITKReviewTestDriver itkTimeAndMemoryProbeTest)
itk_add_test(NAME itkTransformToDeformationFieldSourceTest01
      COMMAND ITKReviewTestDriver itkTransformToDeformationFieldSourceTest
              Affine ${ITK_TEST_OUTPUT_DIR}/itkTransformToDeformationFieldSourceTestField01.mha ${ITK_TEST_OUTPUT_DIR}/itkTransformToDeformationFieldSourceTestImage01.mha)
itk_add_test(NAME itkTransformToDeformationFieldSourceTest02
      COMMAND ITKReviewTestDriver itkTransformToDeformationFieldSourceTest
              BSpline ${ITK_TEST_OUTPUT_DIR}/itkTransformToDeformationFieldSourceTestField02.mha ${ITK_DATA_ROOT}/Input/parametersBSpline.txt ${ITK_TEST_OUTPUT_DIR}/itkTransformToDeformationFieldSourceTestImage02.mha)
itk_add_test(NAME itkValuedRegionalMaximaImageFilterTest
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1ValuedRegionalMaximal-ref.png
              ${ITK_TEST_OUTPUT_DIR}/cthead1ValuedRegionalMaximal.png
    itkValuedRegionalMaximaImageFilterTest 1 ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/cthead1ValuedRegionalMaximal.png ${ITK_TEST_OUTPUT_DIR}/cthead1ValuedRegionalMaximal-ref.png)
itk_add_test(NAME itkValuedRegionalMaximaImageFilterTest2
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1ValuedRegionalMaximal-ref2.png
              ${ITK_TEST_OUTPUT_DIR}/cthead1ValuedRegionalMaximal2.png
    itkValuedRegionalMaximaImageFilterTest 0 ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/cthead1ValuedRegionalMaximal2.png ${ITK_TEST_OUTPUT_DIR}/cthead1ValuedRegionalMaximal-ref2.png)
itk_add_test(NAME itkValuedRegionalMinimaImageFilterTest
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1ValuedRegionalMinimal-ref.png
              ${ITK_TEST_OUTPUT_DIR}/cthead1ValuedRegionalMinimal.png
    itkValuedRegionalMinimaImageFilterTest 1 ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/cthead1ValuedRegionalMinimal.png ${ITK_TEST_OUTPUT_DIR}/cthead1ValuedRegionalMinimal-ref.png)
itk_add_test(NAME itkValuedRegionalMinimaImageFilterTest2
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1ValuedRegionalMinimal-ref2.png
              ${ITK_TEST_OUTPUT_DIR}/cthead1ValuedRegionalMinimal2.png
    itkValuedRegionalMinimaImageFilterTest 0 ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/cthead1ValuedRegionalMinimal2.png ${ITK_TEST_OUTPUT_DIR}/cthead1ValuedRegionalMinimal-ref2.png)
itk_add_test(NAME itkVectorCentralDifferenceImageFunctionTest
      COMMAND ITKReviewTestDriver itkVectorCentralDifferenceImageFunctionTest)
itk_add_test(NAME itkVectorLinearInterpolateNearestNeighborExtrapolateImageFunctionTest
      COMMAND ITKReviewTestDriver itkVectorLinearInterpolateNearestNeighborExtrapolateImageFunctionTest)
itk_add_test(NAME itkVoxBoCUBImageIOTest1
      COMMAND ITKReviewTestDriver itkVoxBoCUBImageIOTest
              ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/HeadMRVolume.cub)
itk_add_test(NAME itkVoxBoCUBImageIOTest2
      COMMAND ITKReviewTestDriver itkVoxBoCUBImageIOTest
              ${ITK_DATA_ROOT}/Input/HeadMRVolume.cub ${ITK_TEST_OUTPUT_DIR}/HeadMRVolume1.cub)
itk_add_test(NAME itkVoxBoCUBImageIOTest3
      COMMAND ITKReviewTestDriver itkVoxBoCUBImageIOTest
              ${ITK_DATA_ROOT}/Input/HeadMRVolume1.cub ${ITK_TEST_OUTPUT_DIR}/HeadMRVolume1.cub.gz)
itk_add_test(NAME itkVoxBoCUBImageIOTest4
      COMMAND ITKReviewTestDriver itkVoxBoCUBImageIOTest
              ${ITK_DATA_ROOT}/Input/HeadMRVolume1.cub.gz ${ITK_TEST_OUTPUT_DIR}/HeadMRVolume2.cub.gz)
itk_add_test(NAME itkVoxBoCUBImageIOTest5
      COMMAND ITKReviewTestDriver itkVoxBoCUBImageIOTest
              ${ITK_DATA_ROOT}/Input/HeadMRVolume2.cub.gz ${ITK_TEST_OUTPUT_DIR}/HeadMRVolume2.cub)
itk_add_test(NAME itkVoxBoCUBImageIOTest6
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/HeadMRVolume.mhd
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeFromCUB2.mha
    itkVoxBoCUBImageIOTest ${ITK_DATA_ROOT}/Input/HeadMRVolume2.cub ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeFromCUB2.mha)
itk_add_test(NAME itkVoxBoCUBImageIOTest7
      COMMAND ITKReviewTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/HeadMRVolume.mhd
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeFromCUB3.mha
    itkVoxBoCUBImageIOTest ${ITK_DATA_ROOT}/Input/HeadMRVolume2.cub.gz ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeFromCUB3.mha)
itk_add_test(NAME itkWarpHarmonicEnergyCalculatorTest
      COMMAND ITKReviewTestDriver itkWarpHarmonicEnergyCalculatorTest)
itk_add_test(NAME itkOptGrayscaleMorphologicalClosingImageFilterTest
      COMMAND ITKReviewTestDriver
    --compare ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestBasic.png
              ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestHisto.png
    --compare ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestBasic.png
              ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestVHGW.png
    --compare ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestBasic.png
              ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestAnchor.png
    itkOptGrayscaleMorphologicalClosingImageFilterTest
    ${INPUTDATA}/cthead1.png
    ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestBasic.png
    ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestHisto.png
    ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestVHGW.png
    ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestAnchor.png
    0)
itk_add_test(NAME itkOptGrayscaleMorphologicalClosingImageFilterTestSafeBorder
      COMMAND ITKReviewTestDriver
    --compare ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestBasicSafeBorder.png
              ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestHistoSafeBorder.png
    --compare ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestBasicSafeBorder.png
              ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestVHGWSafeBorder.png
    --compare ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestBasicSafeBorder.png
              ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestAnchorSafeBorder.png
    itkOptGrayscaleMorphologicalClosingImageFilterTest
    ${INPUTDATA}/cthead1.png
    ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestBasicSafeBorder.png
    ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestHistoSafeBorder.png
    ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestVHGWSafeBorder.png
    ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestAnchorSafeBorder.png
    1)
itk_add_test(NAME itkOptGrayscaleMorphologicalOpeningImageFilterTest
      COMMAND ITKReviewTestDriver
    --compare ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestBasic.png
              ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestHisto.png
    --compare ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestBasic.png
              ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestVHGW.png
    --compare ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestBasic.png
              ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestAnchor.png
    itkOptGrayscaleMorphologicalOpeningImageFilterTest
    ${INPUTDATA}/cthead1.png
    ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestBasic.png
    ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestHisto.png
    ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestVHGW.png
    ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestAnchor.png
    0)
itk_add_test(NAME itkOptGrayscaleMorphologicalOpeningImageFilterTestSafeBorder
      COMMAND ITKReviewTestDriver
    --compare ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestBasicSafeBorder.png
              ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestHistoSafeBorder.png
    --compare ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestBasicSafeBorder.png
              ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestVHGWSafeBorder.png
    --compare ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestBasicSafeBorder.png
              ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestAnchorSafeBorder.png
    itkOptGrayscaleMorphologicalOpeningImageFilterTest
    ${INPUTDATA}/cthead1.png
    ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestBasicSafeBorder.png
    ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestHistoSafeBorder.png
    ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestVHGWSafeBorder.png
    ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestAnchorSafeBorder.png
    1)
itk_add_test(NAME itkOptGrayscaleDilateImageFilterTest
      COMMAND ITKReviewTestDriver
    --compare ${TEMP}/itkOptGrayscaleDilateImageFilterTestBasic.png
              ${TEMP}/itkOptGrayscaleDilateImageFilterTestHisto.png
    --compare ${TEMP}/itkOptGrayscaleDilateImageFilterTestBasic.png
              ${TEMP}/itkOptGrayscaleDilateImageFilterTestVHGW.png
    --compare ${TEMP}/itkOptGrayscaleDilateImageFilterTestBasic.png
              ${TEMP}/itkOptGrayscaleDilateImageFilterTestAnchor.png
    itkOptGrayscaleDilateImageFilterTest
    ${INPUTDATA}/cthead1.png
    ${TEMP}/itkOptGrayscaleDilateImageFilterTestBasic.png
    ${TEMP}/itkOptGrayscaleDilateImageFilterTestHisto.png
    ${TEMP}/itkOptGrayscaleDilateImageFilterTestVHGW.png
    ${TEMP}/itkOptGrayscaleDilateImageFilterTestAnchor.png)
itk_add_test(NAME itkOptGrayscaleErodeImageFilterTest
      COMMAND ITKReviewTestDriver
    --compare ${TEMP}/itkOptGrayscaleErodeImageFilterTestBasic.png
              ${TEMP}/itkOptGrayscaleErodeImageFilterTestHisto.png
    --compare ${TEMP}/itkOptGrayscaleErodeImageFilterTestBasic.png
              ${TEMP}/itkOptGrayscaleErodeImageFilterTestVHGW.png
    --compare ${TEMP}/itkOptGrayscaleErodeImageFilterTestBasic.png
              ${TEMP}/itkOptGrayscaleErodeImageFilterTestAnchor.png
    itkOptGrayscaleErodeImageFilterTest
    ${INPUTDATA}/cthead1.png
    ${TEMP}/itkOptGrayscaleErodeImageFilterTestBasic.png
    ${TEMP}/itkOptGrayscaleErodeImageFilterTestHisto.png
    ${TEMP}/itkOptGrayscaleErodeImageFilterTestVHGW.png
    ${TEMP}/itkOptGrayscaleErodeImageFilterTestAnchor.png)

itk_add_test(NAME itkMapOptGrayscaleMorphologicalClosingImageFilterTest
      COMMAND ITKReviewTestDriver
  --compare ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestBasic.png
            ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestHisto.png
  --compare ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestBasic.png
            ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestVHGW.png
  --compare ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestBasic.png
            ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestAnchor.png
  itkMapOptGrayscaleMorphologicalClosingImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestBasic.png
  ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestHisto.png
  ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestVHGW.png
  ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestAnchor.png
  0
)
itk_add_test(NAME itkMapOptGrayscaleMorphologicalClosingImageFilterTestSafeBorder
      COMMAND ITKReviewTestDriver
  --compare ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestBasicSafeBorder.png
            ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestHistoSafeBorder.png
  --compare ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestBasicSafeBorder.png
            ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestVHGWSafeBorder.png
  --compare ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestBasicSafeBorder.png
            ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestAnchorSafeBorder.png
  itkMapOptGrayscaleMorphologicalClosingImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestBasicSafeBorder.png
  ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestHistoSafeBorder.png
  ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestVHGWSafeBorder.png
  ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestAnchorSafeBorder.png
  1
)
itk_add_test(NAME itkMapOptGrayscaleMorphologicalOpeningImageFilterTest
      COMMAND ITKReviewTestDriver
  --compare ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestBasic.png
            ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestHisto.png
  --compare ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestBasic.png
            ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestVHGW.png
  --compare ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestBasic.png
            ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestAnchor.png
  itkMapOptGrayscaleMorphologicalOpeningImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestBasic.png
  ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestHisto.png
  ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestVHGW.png
  ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestAnchor.png
  0
)
itk_add_test(NAME itkMapOptGrayscaleMorphologicalOpeningImageFilterTestSafeBorder
      COMMAND ITKReviewTestDriver
  --compare ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestBasicSafeBorder.png
            ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestHistoSafeBorder.png
  --compare ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestBasicSafeBorder.png
            ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestVHGWSafeBorder.png
  --compare ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestBasicSafeBorder.png
            ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestAnchorSafeBorder.png
  itkMapOptGrayscaleMorphologicalOpeningImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestBasicSafeBorder.png
  ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestHistoSafeBorder.png
  ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestVHGWSafeBorder.png
  ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestAnchorSafeBorder.png
  1
)
itk_add_test(NAME itkMapOptGrayscaleDilateImageFilterTest
      COMMAND ITKReviewTestDriver
  --compare ${TEMP}/itkMapOptGrayscaleDilateImageFilterTestBasic.png
            ${TEMP}/itkMapOptGrayscaleDilateImageFilterTestHisto.png
  --compare ${TEMP}/itkMapOptGrayscaleDilateImageFilterTestBasic.png
            ${TEMP}/itkMapOptGrayscaleDilateImageFilterTestVHGW.png
  --compare ${TEMP}/itkMapOptGrayscaleDilateImageFilterTestBasic.png
            ${TEMP}/itkMapOptGrayscaleDilateImageFilterTestAnchor.png
  itkMapOptGrayscaleDilateImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkMapOptGrayscaleDilateImageFilterTestBasic.png
  ${TEMP}/itkMapOptGrayscaleDilateImageFilterTestHisto.png
  ${TEMP}/itkMapOptGrayscaleDilateImageFilterTestVHGW.png
  ${TEMP}/itkMapOptGrayscaleDilateImageFilterTestAnchor.png
)
itk_add_test(NAME itkMapOptGrayscaleErodeImageFilterTest
      COMMAND ITKReviewTestDriver
  --compare ${TEMP}/itkMapOptGrayscaleErodeImageFilterTestBasic.png
            ${TEMP}/itkMapOptGrayscaleErodeImageFilterTestHisto.png
  --compare ${TEMP}/itkMapOptGrayscaleErodeImageFilterTestBasic.png
            ${TEMP}/itkMapOptGrayscaleErodeImageFilterTestVHGW.png
  --compare ${TEMP}/itkMapOptGrayscaleErodeImageFilterTestBasic.png
            ${TEMP}/itkMapOptGrayscaleErodeImageFilterTestAnchor.png
  itkMapOptGrayscaleErodeImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkMapOptGrayscaleErodeImageFilterTestBasic.png
  ${TEMP}/itkMapOptGrayscaleErodeImageFilterTestHisto.png
  ${TEMP}/itkMapOptGrayscaleErodeImageFilterTestVHGW.png
  ${TEMP}/itkMapOptGrayscaleErodeImageFilterTestAnchor.png
)
itk_add_test(NAME itkHessianToObjectnessMeasureImageFilterTest
      COMMAND ITKReviewTestDriver
  --compare ${BASELINE}/itkHessianToObjectnessMeasureImageFilterTest.mha
            ${TEMP}/itkHessianToObjectnessMeasureImageFilterTestOutput.mha
            --compareIntensityTolerance .01
  itkHessianToObjectnessMeasureImageFilterTest
  ${INPUTDATA}/DSA.png
  ${TEMP}/itkHessianToObjectnessMeasureImageFilterTestOutput.mha
  1 0)

itk_add_test(NAME itkHessianToObjectnessMeasureImageFilterTest2
      COMMAND ITKReviewTestDriver
  --compare ${BASELINE}/itkHessianToObjectnessMeasureImageFilterTest2.mha
            ${TEMP}/itkHessianToObjectnessMeasureImageFilterTestOutput2.mha
            --compareIntensityTolerance .01
  itkHessianToObjectnessMeasureImageFilterTest
  ${INPUTDATA}/DSA.png
  ${TEMP}/itkHessianToObjectnessMeasureImageFilterTestOutput2.mha
  0 0)
itk_add_test(NAME itkCompositeTransformReaderTest
      COMMAND ITKReviewTestDriver itkCompositeTransformReaderTest)
itk_add_test(NAME itkCompositeTransformWriterTest
      COMMAND ITKReviewTestDriver itkCompositeTransformWriterTest ${TEMP})
itk_add_test(NAME itkCompositeTransformWriterAndReaderTest
      COMMAND ITKReviewTestDriver
      itkCompositeTransformWriterAndReaderTest ${TEMP})
