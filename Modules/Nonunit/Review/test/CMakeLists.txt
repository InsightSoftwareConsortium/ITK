itk_module_test()
set(ITKReviewTests
itkAreaClosingImageFilterTest.cxx
itkAreaOpeningImageFilterTest.cxx
itkConformalFlatteningMeshFilterTest.cxx
itkConformalFlatteningQuadEdgeMeshFilterTest.cxx
itkContourExtractor2DImageFilterTest.cxx
itkDirectFourierReconstructionImageToImageFilterTest.cxx
itkDiscreteGaussianDerivativeImageFilterScaleSpaceTest.cxx
itkDiscreteGaussianDerivativeImageFilterTest.cxx
itkDiscreteGaussianDerivativeImageFunctionTest.cxx
itkDiscreteGradientMagnitudeGaussianImageFunctionTest.cxx
itkDiscreteHessianGaussianImageFunctionTest.cxx
itkFastApproximateRankImageFilterTest.cxx
itkGridForwardWarpImageFilterTest.cxx
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
itkLabelGeometryImageFilterTest.cxx
itkMultiphaseDenseFiniteDifferenceImageFilterTest.cxx
itkMultiphaseFiniteDifferenceImageFilterTest.cxx
itkMultiphaseSparseFiniteDifferenceImageFilterTest.cxx
itkMultiScaleHessianBasedMeasureImageFilterTest.cxx
itkNeuralNetworkIOTest.cxx
itkOptImageToImageMetricsTest.cxx
itkOptImageToImageMetricsTest2.cxx
itkOptMattesMutualInformationImageToImageMetricThreadsTest1.cxx
itkRegionBasedLevelSetFunctionTest.cxx
itkRobustAutomaticThresholdCalculatorTest.cxx
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
itkShapedFloodFilledImageFunctionConditionalConstIteratorTest1.cxx
itkShapedFloodFilledImageFunctionConditionalConstIteratorTest2.cxx
itkShapedFloodFilledImageFunctionConditionalConstIteratorTest3.cxx
itkStochasticFractalDimensionImageFilterTest.cxx
itkTimeAndMemoryProbeTest.cxx
itkVectorCentralDifferenceImageFunctionTest.cxx
itkVoxBoCUBImageIOTest.cxx
itkWarpHarmonicEnergyCalculatorTest.cxx
itkWarpJacobianDeterminantFilterTest.cxx
)

set(BASELINE ${ITK_DATA_ROOT}/Baseline/Review)
set(INPUTDATA ${ITK_DATA_ROOT}/Input)
set(BASELINE_ROOT ${ITK_DATA_ROOT}/Baseline)
set(TEMP ${ITK_TEST_OUTPUT_DIR})


CreateTestDriver_SupportBuildInIOFactories(ITKReview  "${ITKReview-Test_LIBRARIES}" "${ITKReviewTests}")

itk_add_test(NAME itkAreaClosingImageFilterTest0
      COMMAND ITKReviewTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkAreaClosingImageFilterTest0.png}
              ${ITK_TEST_OUTPUT_DIR}/itkAreaClosingImageFilterTest0.png
    itkAreaClosingImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkAreaClosingImageFilterTest0.png 1000 0 0)
itk_add_test(NAME itkAreaOpeningImageFilterTest0
      COMMAND ITKReviewTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkAreaOpeningImageFilterTest0.png}
              ${ITK_TEST_OUTPUT_DIR}/itkAreaOpeningImageFilterTest0.png
    itkAreaOpeningImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkAreaOpeningImageFilterTest0.png 1000 0 1)
itk_add_test(NAME itkConformalFlatteningMeshFilterTest1
      COMMAND ITKReviewTestDriver itkConformalFlatteningMeshFilterTest
              DATA{${ITK_DATA_ROOT}/Input/genusZeroSurface01.vtk} ${ITK_TEST_OUTPUT_DIR}/genusZeroMesh01ConformalFlattenedToSphere.vtk 1)
itk_add_test(NAME itkConformalFlatteningMeshFilterTest2
      COMMAND ITKReviewTestDriver itkConformalFlatteningMeshFilterTest
              DATA{${ITK_DATA_ROOT}/Input/genusZeroSurface01.vtk} ${ITK_TEST_OUTPUT_DIR}/genusZeroMesh01ConformalFlattenedToPlane.vtk 0)
itk_add_test(NAME itkConformalFlatteningQuadEdgeMeshFilterTest1
      COMMAND ITKReviewTestDriver itkConformalFlatteningQuadEdgeMeshFilterTest
              DATA{${ITK_DATA_ROOT}/Input/genusZeroSurface01.vtk} ${ITK_TEST_OUTPUT_DIR}/genusZeroQuadEdgeMesh01ConformalFlattenedToSphere.vtk 3 100.0 1)
itk_add_test(NAME itkConformalFlatteningQuadEdgeMeshFilterTest2
      COMMAND ITKReviewTestDriver itkConformalFlatteningQuadEdgeMeshFilterTest
              DATA{${ITK_DATA_ROOT}/Input/genusZeroSurface01.vtk} ${ITK_TEST_OUTPUT_DIR}/genusZeroQuadEdgeMesh01ConformalFlattenedToPlane.vtk 3 100.0 0)
itk_add_test(NAME itkContourExtractor2DImageFilterTest
      COMMAND ITKReviewTestDriver itkContourExtractor2DImageFilterTest
              DATA{${ITK_DATA_ROOT}/Input/ContourExtractor2DTest.tif})
itk_add_test(NAME itkDirectFourierReconstructionImageToImageFilterTest
      COMMAND ITKReviewTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/reconstruct.mha}
              ${ITK_TEST_OUTPUT_DIR}/reconstruct.mha
    itkDirectFourierReconstructionImageToImageFilterTest DATA{${ITK_DATA_ROOT}/Input/sinogram-small.mha} ${ITK_TEST_OUTPUT_DIR}/reconstruct.mha 1 2 0 2 2 1 3 180 0 0 0 64 64 2 0)
itk_add_test(NAME itkDiscreteGaussianDerivativeImageFilterScaleSpaceTest
      COMMAND ITKReviewTestDriver itkDiscreteGaussianDerivativeImageFilterScaleSpaceTest)
itk_add_test(NAME itkDiscreteGaussianDerivativeImageFilterTest01
      COMMAND ITKReviewTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkDiscreteGaussianDerivativeImageFilterTest01.png}
              ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest01.png
    itkDiscreteGaussianDerivativeImageFilterTest DATA{${ITK_DATA_ROOT}/Input/whiteSpot.png} ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest01.png 0 1 3.0)
itk_add_test(NAME itkDiscreteGaussianDerivativeImageFilterTest10
      COMMAND ITKReviewTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkDiscreteGaussianDerivativeImageFilterTest10.png}
              ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest10.png
    itkDiscreteGaussianDerivativeImageFilterTest DATA{${ITK_DATA_ROOT}/Input/whiteSpot.png} ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest10.png 1 0 3.0)
itk_add_test(NAME itkDiscreteGaussianDerivativeImageFilterTest11
      COMMAND ITKReviewTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkDiscreteGaussianDerivativeImageFilterTest11.png}
              ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest11.png
    itkDiscreteGaussianDerivativeImageFilterTest DATA{${ITK_DATA_ROOT}/Input/whiteSpot.png} ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest11.png 1 1 3.0)
itk_add_test(NAME itkDiscreteGaussianDerivativeImageFilterTest20
      COMMAND ITKReviewTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkDiscreteGaussianDerivativeImageFilterTest20.png}
              ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest20.png
    itkDiscreteGaussianDerivativeImageFilterTest DATA{${ITK_DATA_ROOT}/Input/whiteSpot.png} ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest20.png 2 0 3.0)
itk_add_test(NAME itkDiscreteGaussianDerivativeImageFilterTest02
      COMMAND ITKReviewTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkDiscreteGaussianDerivativeImageFilterTest02.png}
              ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest02.png
    itkDiscreteGaussianDerivativeImageFilterTest DATA{${ITK_DATA_ROOT}/Input/whiteSpot.png} ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest02.png 0 2 3.0)
itk_add_test(NAME itkDiscreteGaussianDerivativeImageFilterTest21
      COMMAND ITKReviewTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkDiscreteGaussianDerivativeImageFilterTest21.png}
              ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest21.png
    itkDiscreteGaussianDerivativeImageFilterTest DATA{${ITK_DATA_ROOT}/Input/whiteSpot.png} ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest21.png 2 1 3.0)
itk_add_test(NAME itkDiscreteGaussianDerivativeImageFilterTest12
      COMMAND ITKReviewTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkDiscreteGaussianDerivativeImageFilterTest12.png}
              ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest12.png
    itkDiscreteGaussianDerivativeImageFilterTest DATA{${ITK_DATA_ROOT}/Input/whiteSpot.png} ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest12.png 1 2 3.0)
itk_add_test(NAME itkDiscreteGaussianDerivativeImageFilterTest30
      COMMAND ITKReviewTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkDiscreteGaussianDerivativeImageFilterTest30.png}
              ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest30.png
    itkDiscreteGaussianDerivativeImageFilterTest DATA{${ITK_DATA_ROOT}/Input/whiteSpot.png} ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest30.png 3 0 3.0)
itk_add_test(NAME itkDiscreteGaussianDerivativeImageFilterTest03
      COMMAND ITKReviewTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkDiscreteGaussianDerivativeImageFilterTest03.png}
              ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest03.png
    itkDiscreteGaussianDerivativeImageFilterTest DATA{${ITK_DATA_ROOT}/Input/whiteSpot.png} ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFilterTest03.png 0 3 3.0)
itk_add_test(NAME itkDiscreteGaussianDerivativeImageFunctionTest01
      COMMAND ITKReviewTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkDiscreteGaussianDerivativeImageFunctionTest01.png}
              ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFunctionTest01.png
    itkDiscreteGaussianDerivativeImageFunctionTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFunctionTest01.png 1 2.0 0.01 32 0)
itk_add_test(NAME itkDiscreteGaussianDerivativeImageFunctionTest02
      COMMAND ITKReviewTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFunctionTest02.png
                  488a4bf5daa08cadb3e8b1018a3fe5c0
    itkDiscreteGaussianDerivativeImageFunctionTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGaussianDerivativeImageFunctionTest02.png 2 1.5 0.03 16 1)
itk_add_test(NAME itkDiscreteGradientMagnitudeGaussianImageFunctionTest01
      COMMAND ITKReviewTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkDiscreteGradientMagnitudeGaussianImageFunctionTest01.png}
              ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGradientMagnitudeGaussianImageFunctionTest01.png
    itkDiscreteGradientMagnitudeGaussianImageFunctionTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGradientMagnitudeGaussianImageFunctionTest01.png 2.0 0.01 32 0 1)
itk_add_test(NAME itkDiscreteGradientMagnitudeGaussianImageFunctionTest02
      COMMAND ITKReviewTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGradientMagnitudeGaussianImageFunctionTest02.png
                  de1904605e667018baaa3f73c5e5c220
    itkDiscreteGradientMagnitudeGaussianImageFunctionTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGradientMagnitudeGaussianImageFunctionTest02.png 1.5 0.03 64 1 1)
itk_add_test(NAME itkDiscreteGradientMagnitudeGaussianImageFunctionTestUseImageSpacingOff
      COMMAND ITKReviewTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGradientMagnitudeGaussianImageFunctionTestUseImageSpacingOff.png
                  de1904605e667018baaa3f73c5e5c220
    itkDiscreteGradientMagnitudeGaussianImageFunctionTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkDiscreteGradientMagnitudeGaussianImageFunctionTestUseImageSpacingOff.png 1.5 0.03 64 1 0)
itk_add_test(NAME itkDiscreteHessianGaussianImageFunctionTest
      COMMAND ITKReviewTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkDiscreteHessianGaussianImageFunctionTest.mha}
              ${ITK_TEST_OUTPUT_DIR}/itkDiscreteHessianGaussianImageFunctionTest.mha
    itkDiscreteHessianGaussianImageFunctionTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkDiscreteHessianGaussianImageFunctionTest.mha 2.0 0.01 32)
itk_add_test(NAME itkFastApproximateRankImageFilterTest3
      COMMAND ITKReviewTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkFastApproximateRankImageFilter3.png}
              ${ITK_TEST_OUTPUT_DIR}/itkFastApproximateRankImageFilter3.png
    itkFastApproximateRankImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkFastApproximateRankImageFilter3.png 3)
itk_add_test(NAME itkFastApproximateRankImageFilterTest10
      COMMAND ITKReviewTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkFastApproximateRankImageFilter10.png}
              ${ITK_TEST_OUTPUT_DIR}/itkFastApproximateRankImageFilter10.png
    itkFastApproximateRankImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkFastApproximateRankImageFilter10.png 10)
itk_add_test(NAME itkGridForwardWarpImageFilterTest
      COMMAND ITKReviewTestDriver
      --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkGridForwardWarpImageFilterTest.mha
                    7dea362b3fac8e00956a4952a3d4f474
     itkGridForwardWarpImageFilterTest ${ITK_TEST_OUTPUT_DIR}/itkGridForwardWarpImageFilterTest.mha )
itk_add_test(NAME itkImageFunctionTest
      COMMAND ITKReviewTestDriver itkImageFunctionTest)
itk_add_test(NAME itkJPEG2000Test00
      COMMAND ITKReviewTestDriver itkJPEG2000ImageIORegionOfInterest
              DATA{${ITK_DATA_ROOT}/Input/Bretagne1.j2k} ${ITK_TEST_OUTPUT_DIR}/Bretagne1_RegionTest01.tif 160 120 320 240)
itk_add_test(NAME itkJPEG2000Test01
      COMMAND ITKReviewTestDriver itkJPEG2000ImageIOTest03
              DATA{${ITK_DATA_ROOT}/Input/Bretagne1.j2k} ${ITK_TEST_OUTPUT_DIR}/Bretagne1_01.png)
itk_add_test(NAME itkJPEG2000Test02
      COMMAND ITKReviewTestDriver itkJPEG2000ImageIOTest03
              DATA{${ITK_DATA_ROOT}/Input/Cevennes2.jp2} ${ITK_TEST_OUTPUT_DIR}/Cevennes2_01.png)
itk_add_test(NAME itkJPEG2000Test03
      COMMAND ITKReviewTestDriver itkJPEG2000ImageIOTest03
              ${ITK_TEST_OUTPUT_DIR}/Bretagne1_RegionTest01.tif ${ITK_TEST_OUTPUT_DIR}/Bretagne1_02.j2k)
set_tests_properties( itkJPEG2000Test03 PROPERTIES
  DEPENDS itkJPEG2000Test00
  REQUIRED_FILES ${ITK_TEST_OUTPUT_DIR}/Bretagne1_RegionTest01.tif )

itk_add_test(NAME itkJPEG2000Test05
      COMMAND ITKReviewTestDriver itkJPEG2000ImageIOTest05
              DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd,HeadMRVolume.raw} ${ITK_TEST_OUTPUT_DIR} jp2)
itk_add_test(NAME itkJPEG2000Test06
      COMMAND ITKReviewTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/IO/cthead1-unitspacing.tif}
              ${ITK_TEST_OUTPUT_DIR}/cthead1.tif
    itkJPEG2000ImageIOTest06 DATA{${ITK_DATA_ROOT}/Input/cthead1.j2k} ${ITK_TEST_OUTPUT_DIR}/cthead1.tif)
itk_add_test(NAME itkLabelGeometryImageFilterTest
      COMMAND ITKReviewTestDriver
    --compare DATA{${BASELINE}/itkLabelGeometryImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkLabelGeometryImageFilterTest.png
    itkLabelGeometryImageFilterTest DATA{${INPUTDATA}/LabelImageWithVariousShapes.png} DATA{${INPUTDATA}/GrayscaleImageWithVariousShapes.png} ${ITK_TEST_OUTPUT_DIR}/itkLabelGeometryImageFilterTest.png ${ITK_TEST_OUTPUT_DIR}/itkLabelGeometryImageFilterTest.csv DATA{${BASELINE}/itkLabelGeometryImageFilterTest.csv})
itk_add_test(NAME itkLabelGeometryImageFilterTest3D
      COMMAND ITKReviewTestDriver
    --compare DATA{${BASELINE}/itkLabelGeometryImageFilterTest3D.mhd,itkLabelGeometryImageFilterTest3D.raw}
              ${ITK_TEST_OUTPUT_DIR}/itkLabelGeometryImageFilterTest3D.mhd
    itkLabelGeometryImageFilterTest DATA{${INPUTDATA}/LabelVolumeWithVariousShapes.mhd,LabelVolumeWithVariousShapes.raw} DATA{${INPUTDATA}/GrayscaleVolumeWithVariousShapes.mhd,GrayscaleVolumeWithVariousShapes.raw} ${ITK_TEST_OUTPUT_DIR}/itkLabelGeometryImageFilterTest3D.mhd ${ITK_TEST_OUTPUT_DIR}/itkLabelGeometryImageFilterTest3D.csv DATA{${BASELINE}/itkLabelGeometryImageFilterTest3D.csv})
itk_add_test(NAME itkMultiphaseDenseFiniteDifferenceImageFilterTest
      COMMAND ITKReviewTestDriver itkMultiphaseDenseFiniteDifferenceImageFilterTest)
itk_add_test(NAME itkMultiphaseFiniteDifferenceImageFilterTest
      COMMAND ITKReviewTestDriver itkMultiphaseFiniteDifferenceImageFilterTest)
itk_add_test(NAME itkMultiphaseSparseFiniteDifferenceImageFilterTest
      COMMAND ITKReviewTestDriver itkMultiphaseSparseFiniteDifferenceImageFilterTest)
itk_add_test(NAME itkMultiScaleHessianBasedMeasureImageFilterTest
      COMMAND ITKReviewTestDriver
          --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkMultiScaleHessianBasedMeasureImageFilterTestEnhancedOutput.mha}
              ${ITK_TEST_OUTPUT_DIR}/itkMultiScaleHessianBasedMeasureImageFilterTestEnhancedOutput.mha
              itkMultiScaleHessianBasedMeasureImageFilterTest DATA{${ITK_DATA_ROOT}/Input/DSA.png} ${ITK_TEST_OUTPUT_DIR}/itkMultiScaleHessianBasedMeasureImageFilterTestEnhancedOutput.mha ${ITK_TEST_OUTPUT_DIR}/itkMultiScaleHessianBasedMeasureImageFilterTestScalesOutput.mha 5 10 10 1 0 ${ITK_TEST_OUTPUT_DIR}/itkMultiScaleHessianBasedMeasureImageFilterTestEnhancedOutput2.mha)
itk_add_test(NAME itkNeuralNetworkIOTest
      COMMAND ITKReviewTestDriver itkNeuralNetworkIOTest
              DATA{${ITK_DATA_ROOT}/Input/xornet.txt} DATA{${ITK_DATA_ROOT}/Input/xortest.txt} ${ITK_TEST_OUTPUT_DIR})

itk_add_test(NAME itkOptImageToImageMetricsTest01
      COMMAND ITKReviewTestDriver --redirectOutput ${TEMP}/itkOptImageToImageMetricsTest01.txt
              itkOptImageToImageMetricsTest
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png)
set_tests_properties(itkOptImageToImageMetricsTest01 PROPERTIES ATTACHED_FILES_ON_FAIL ${TEMP}/itkOptImageToImageMetricsTest01.txt)

itk_add_test(NAME itkOptImageToImageMetricsTest02
      COMMAND ITKReviewTestDriver --with-threads 2 --redirectOutput ${TEMP}/itkOptImageToImageMetricsTest02.txt
              itkOptImageToImageMetricsTest
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png)
set_tests_properties(itkOptImageToImageMetricsTest02 PROPERTIES ATTACHED_FILES_ON_FAIL ${TEMP}/itkOptImageToImageMetricsTest02.txt)

itk_add_test(NAME itkOptImageToImageMetricsTest03
      COMMAND ITKReviewTestDriver --without-threads --redirectOutput ${TEMP}/itkOptImageToImageMetricsTest03.txt
              itkOptImageToImageMetricsTest
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png)
set_tests_properties(itkOptImageToImageMetricsTest03 PROPERTIES ATTACHED_FILES_ON_FAIL ${TEMP}/itkOptImageToImageMetricsTest03.txt)

itk_add_test(NAME itkOptImageToImageMetricsTest04
      COMMAND ITKReviewTestDriver
      itkOptImageToImageMetricsTest2
      ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png DATA{Baseline/itkBSplineTransformTest4.png})
itk_add_test(NAME itkOptMattesImageToImageMetricsTest01
      COMMAND ITKReviewTestDriver itkOptMattesMutualInformationImageToImageMetricThreadsTest1
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png 0 100)
itk_add_test(NAME itkRegionBasedLevelSetFunctionTest
      COMMAND ITKReviewTestDriver itkRegionBasedLevelSetFunctionTest)
itk_add_test(NAME itkRobustAutomaticThresholdImageFilterTest
      COMMAND ITKReviewTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkRobustAutomaticThresholdImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkRobustAutomaticThresholdImageFilterTest.png
    itkRobustAutomaticThresholdImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkRobustAutomaticThresholdImageFilterTest.png 2 65535 0 94)
itk_add_test(NAME itkRobustAutomaticThresholdCalculatorTest
      COMMAND ITKReviewTestDriver
    itkRobustAutomaticThresholdCalculatorTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} 2 94)
itk_add_test(NAME itkScalarChanAndVeseDenseLevelSetImageFilterTest1
      COMMAND ITKReviewTestDriver itkScalarChanAndVeseDenseLevelSetImageFilterTest1)
itk_add_test(NAME itkScalarChanAndVeseDenseLevelSetImageFilterTest2_0
      COMMAND ITKReviewTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkScalarChanAndVeseDenseLevelSetImageFilterTest2_0.mha}
              ${ITK_TEST_OUTPUT_DIR}/itkScalarChanAndVeseDenseLevelSetImageFilterTest2_0.mha
    itkScalarChanAndVeseDenseLevelSetImageFilterTest2 DATA{${ITK_DATA_ROOT}/Baseline/Segmentation/ShapeDetectionLevelSetLeftVentricleTest.png,:} ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySlice.png ${ITK_TEST_OUTPUT_DIR}/itkScalarChanAndVeseDenseLevelSetImageFilterTest2_0.mha 1 1 0 0 0)
itk_add_test(NAME itkScalarChanAndVeseDenseLevelSetImageFilterTest2_1
      COMMAND ITKReviewTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkScalarChanAndVeseDenseLevelSetImageFilterTest2_1.mha}
              ${ITK_TEST_OUTPUT_DIR}/itkScalarChanAndVeseDenseLevelSetImageFilterTest2_1.mha
    itkScalarChanAndVeseDenseLevelSetImageFilterTest2 DATA{${ITK_DATA_ROOT}/Baseline/Segmentation/ShapeDetectionLevelSetLeftVentricleTest.png,:} ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySlice.png ${ITK_TEST_OUTPUT_DIR}/itkScalarChanAndVeseDenseLevelSetImageFilterTest2_1.mha 1 1 1 1 200)
itk_add_test(NAME itkScalarChanAndVeseDenseLevelSetImageFilterTest2_2
      COMMAND ITKReviewTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkScalarChanAndVeseDenseLevelSetImageFilterTest2_2.mha}
              ${ITK_TEST_OUTPUT_DIR}/itkScalarChanAndVeseDenseLevelSetImageFilterTest2_2.mha
    itkScalarChanAndVeseDenseLevelSetImageFilterTest2 DATA{${ITK_DATA_ROOT}/Baseline/Segmentation/ShapeDetectionLevelSetLeftVentricleTest.png,:} ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySlice.png ${ITK_TEST_OUTPUT_DIR}/itkScalarChanAndVeseDenseLevelSetImageFilterTest2_2.mha 0 0 1 0 0)
itk_add_test(NAME itkScalarChanAndVeseDenseLevelSetImageFilterTest2_3
      COMMAND ITKReviewTestDriver itkScalarChanAndVeseDenseLevelSetImageFilterTest2
              DATA{${ITK_DATA_ROOT}/Baseline/Segmentation/ShapeDetectionLevelSetLeftVentricleTest.png,:} ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySlice.png ${ITK_TEST_OUTPUT_DIR}/itkScalarChanAndVeseDenseLevelSetImageFilterTest2_3.mha 0 0 0 0 0)
itk_add_test(NAME itkScalarChanAndVeseDenseLevelSetImageFilterTest3
      COMMAND ITKReviewTestDriver itkScalarChanAndVeseDenseLevelSetImageFilterTest3)
itk_add_test(NAME itkScalarChanAndVeseDenseLevelSetImageFilterTest4
      COMMAND ITKReviewTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkScalarChanAndVeseDenseLevelSetImageFilterTest2_0.mha}
              ${ITK_TEST_OUTPUT_DIR}/itkScalarChanAndVeseDenseLevelSetImageFilterTest4.mha
    itkScalarChanAndVeseDenseLevelSetImageFilterTest4 DATA{${ITK_DATA_ROOT}/Baseline/Segmentation/ShapeDetectionLevelSetLeftVentricleTest.png,:} ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySlice.png ${ITK_TEST_OUTPUT_DIR}/itkScalarChanAndVeseDenseLevelSetImageFilterTest4.mha 1 1 0 0 0)
itk_add_test(NAME itkScalarChanAndVeseLevelSetFunctionTest1
      COMMAND ITKReviewTestDriver itkScalarChanAndVeseLevelSetFunctionTest1)
itk_add_test(NAME itkScalarChanAndVeseLevelSetFunctionTest2
      COMMAND ITKReviewTestDriver itkScalarChanAndVeseLevelSetFunctionTest2)
itk_add_test(NAME itkScalarChanAndVeseSparseLevelSetImageFilterTest1
      COMMAND ITKReviewTestDriver itkScalarChanAndVeseSparseLevelSetImageFilterTest1)
itk_add_test(NAME itkScalarChanAndVeseSparseLevelSetImageFilterTest2
      COMMAND ITKReviewTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkScalarChanAndVeseSparseLevelSetImageFilterTest2.mha}
              ${ITK_TEST_OUTPUT_DIR}/itkScalarChanAndVeseSparseLevelSetImageFilterTest2.mha
    itkScalarChanAndVeseSparseLevelSetImageFilterTest2 DATA{${ITK_DATA_ROOT}/Baseline/Segmentation/ShapeDetectionLevelSetLeftVentricleTest.png,:} ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySlice.png ${ITK_TEST_OUTPUT_DIR}/itkScalarChanAndVeseSparseLevelSetImageFilterTest2.mha)
itk_add_test(NAME itkScalarRegionBasedLevelSetFunctionTest
      COMMAND ITKReviewTestDriver itkScalarRegionBasedLevelSetFunctionTest)
itk_add_test(NAME itkShapedFloodFilledImageFunctionConditionalConstIteratorTest1
      COMMAND ITKReviewTestDriver itkShapedFloodFilledImageFunctionConditionalConstIteratorTest1
              DATA{${ITK_DATA_ROOT}/Input/8ConnectedImage.bmp})
itk_add_test(NAME itkShapedFloodFilledImageFunctionConditionalConstIteratorTest2
      COMMAND ITKReviewTestDriver itkShapedFloodFilledImageFunctionConditionalConstIteratorTest2)
itk_add_test(NAME itkShapedFloodFilledImageFunctionConditionalConstIteratorTest3
      COMMAND ITKReviewTestDriver itkShapedFloodFilledImageFunctionConditionalConstIteratorTest3)
itk_add_test(NAME itkStochasticFractalDimensionImageFilterTest1
      COMMAND ITKReviewTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkStochasticFractalDimensionImageFilterTest1.mha}
              ${ITK_TEST_OUTPUT_DIR}/itkStochasticFractalDimensionImageFilterTest1.mha
    itkStochasticFractalDimensionImageFilterTest 2 DATA{${ITK_DATA_ROOT}/Input/TreeBarkTexture.png} ${ITK_TEST_OUTPUT_DIR}/itkStochasticFractalDimensionImageFilterTest1.mha 2)
itk_add_test(NAME itkStochasticFractalDimensionImageFilterTest2
      COMMAND ITKReviewTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkStochasticFractalDimensionImageFilterTest2.mha}
              ${ITK_TEST_OUTPUT_DIR}/itkStochasticFractalDimensionImageFilterTest2.mha
    itkStochasticFractalDimensionImageFilterTest 2 DATA{${ITK_DATA_ROOT}/Input/TreeBarkTexture.png} ${ITK_TEST_OUTPUT_DIR}/itkStochasticFractalDimensionImageFilterTest2.mha 2 DATA{${ITK_DATA_ROOT}/Input/circle100.png} 255)
itk_add_test(NAME itkTimeAndMemoryProbeTest1
      COMMAND ITKReviewTestDriver itkTimeAndMemoryProbeTest)
itk_add_test(NAME itkVectorCentralDifferenceImageFunctionTest
      COMMAND ITKReviewTestDriver itkVectorCentralDifferenceImageFunctionTest)
itk_add_test(NAME itkVoxBoCUBImageIOTest1
      COMMAND ITKReviewTestDriver itkVoxBoCUBImageIOTest
              DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd,HeadMRVolume.raw} ${ITK_TEST_OUTPUT_DIR}/HeadMRVolume.cub)
itk_add_test(NAME itkVoxBoCUBImageIOTest2
      COMMAND ITKReviewTestDriver itkVoxBoCUBImageIOTest
              DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.cub} ${ITK_TEST_OUTPUT_DIR}/HeadMRVolume1.cub)
itk_add_test(NAME itkVoxBoCUBImageIOTest3
      COMMAND ITKReviewTestDriver itkVoxBoCUBImageIOTest
              DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume1.cub} ${ITK_TEST_OUTPUT_DIR}/HeadMRVolume1.cub.gz)
itk_add_test(NAME itkVoxBoCUBImageIOTest4
      COMMAND ITKReviewTestDriver itkVoxBoCUBImageIOTest
              DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume1.cub.gz} ${ITK_TEST_OUTPUT_DIR}/HeadMRVolume2.cub.gz)
itk_add_test(NAME itkVoxBoCUBImageIOTest5
      COMMAND ITKReviewTestDriver itkVoxBoCUBImageIOTest
              DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume2.cub.gz} ${ITK_TEST_OUTPUT_DIR}/HeadMRVolume2.cub)
itk_add_test(NAME itkVoxBoCUBImageIOTest6
      COMMAND ITKReviewTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/IO/HeadMRVolume.mhd,HeadMRVolume.raw}
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeFromCUB2.mha
    itkVoxBoCUBImageIOTest DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume2.cub} ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeFromCUB2.mha)
itk_add_test(NAME itkVoxBoCUBImageIOTest7
      COMMAND ITKReviewTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/IO/HeadMRVolume.mhd,HeadMRVolume.raw}
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeFromCUB3.mha
    itkVoxBoCUBImageIOTest DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume2.cub.gz} ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeFromCUB3.mha)
itk_add_test(NAME itkWarpHarmonicEnergyCalculatorTestUseImageSpacingOn
      COMMAND ITKReviewTestDriver itkWarpHarmonicEnergyCalculatorTest 1 1.0 0.0)
itk_add_test(NAME itkWarpHarmonicEnergyCalculatorTestUseImageSpacingOff
      COMMAND ITKReviewTestDriver itkWarpHarmonicEnergyCalculatorTest 0 1.0 0.0)
