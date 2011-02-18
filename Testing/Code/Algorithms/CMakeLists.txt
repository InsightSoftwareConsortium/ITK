if( NOT ITK_DISABLE_CXX_TESTING )
if(ITK_BINARY_DIR)
  if(TCL_TCLSH)
    add_test(PrintSelf-Algorithms ${TCL_TCLSH}
             ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl
             ${ITK_SOURCE_DIR}/Code/Algorithms)
  endif(TCL_TCLSH)
endif(ITK_BINARY_DIR)

# Define some convenient locations
set(BASELINE ${ITK_DATA_ROOT}/Baseline/Algorithms)
set(INPUTDATA ${ITK_DATA_ROOT}/Input)
set(TEMP ${ITKTesting_BINARY_DIR}/Temporary)

############################################################
# CXX Testing
#
set(KIT Algorithms)
set(KIT_LIBS ITKAlgorithms ITKFEM ITKNumerics ITKStatistics)
set(KitTests
  itk2DDeformableTest
  itkAnisotropicFourthOrderLevelSetImageFilterTest
  itkAntiAliasBinaryImageFilterTest
  itkAutomaticTopologyMeshSourceTest
  itkAutomaticTopologyQuadEdgeMeshSourceTest
  itkBayesianClassifierImageFilterTest
  itkBinaryMask3DMeshSourceTest
  itkBinaryMask3DQuadEdgeMeshSourceTest
  itkBinaryMedialNodeMetricTest
  itkBinaryMinMaxCurvatureFlowImageFilterTest
  itkBinaryThinningImageFilterTest
  itkBioCellTest
  itkBioCellularAggregateTest
  itkBioGeneNetworkTest
  itkBioGeneTest
  itkBioGenomeTest
  itkBresenhamLineTest
  itkCannySegmentationLevelSetImageFilterTest
  itkCleanQuadEdgeMeshFilterTest
  itkCollidingFrontsImageFilterTest
  itkCompareHistogramImageToImageMetricTest
  itkCorrelationCoefficientHistogramImageToImageMetricTest
  itkCurvatureFlowTest
  itkCurvesLevelSetImageFilterTest
  itkCurvesLevelSetImageFilterZeroSigmaTest
  itkDeformableSimplexMesh3DBalloonForceFilterTest
  itkDeformableSimplexMesh3DFilterTest
  itkDeformableSimplexMesh3DGradientConstraintForceFilterTest
  itkDeformableTest
  itkDelaunayConformingQuadEdgeMeshFilterTest
  itkDemonsRegistrationFilterTest
  itkDiscreteGaussianCurvatureQuadEdgeMeshFilterTest
  itkDiscreteMaximumCurvatureQuadEdgeMeshFilterTest
  itkDiscreteMeanCurvatureQuadEdgeMeshFilterTest
  itkDiscreteMinimumCurvatureQuadEdgeMeshFilterTest
  itkExtensionVelocitiesImageFilterTest
  itkExtractMeshConnectedRegionsTest
  itkFEMRegistrationFilterTest
  itkFastChamferDistanceImageFilterTest
  itkFastMarchingExtensionImageFilterTest
  itkFastMarchingTest
  itkFastMarchingTest2
  itkFastMarchingUpwindGradientTest
  itkGeodesicActiveContourLevelSetImageFilterTest
  itkGeodesicActiveContourLevelSetImageFilterZeroSigmaTest
  itkGeodesicActiveContourShapePriorLevelSetImageFilterTest
  itkGeodesicActiveContourShapePriorLevelSetImageFilterTest_2
  itkGibbsTest
  itkGradientDifferenceImageToImageMetricTest
  itkGradientVectorFlowImageFilterTest
  itkHistogramImageToImageMetricTest
  itkHistogramMatchingImageFilterTest
  itkImageMomentsTest
  itkImagePCADecompositionCalculatorTest
  itkImagePCAShapeModelEstimatorTest
  itkImageRegistrationMethodTest
  itkImageRegistrationMethodTest_1
  itkImageRegistrationMethodTest_10
  itkImageRegistrationMethodTest_11
  itkImageRegistrationMethodTest_12
  itkImageRegistrationMethodTest_13
  itkImageRegistrationMethodTest_14
  itkImageRegistrationMethodTest_15
  itkImageRegistrationMethodTest_16
  itkImageRegistrationMethodTest_2
  itkImageRegistrationMethodTest_3
  itkImageRegistrationMethodTest_4
  itkImageRegistrationMethodTest_5
  itkImageRegistrationMethodTest_6
  itkImageRegistrationMethodTest_7
  itkImageRegistrationMethodTest_8
  itkImageRegistrationMethodTest_9
  itkInterpolateTest
  itkIsoContourDistanceImageFilterTest
  itkIsolatedWatershedImageFilterTest
  itkIsotropicFourthOrderLevelSetImageFilterTest
  itkKalmanLinearEstimatorTest
  itkKappaStatisticImageToImageMetricTest
  itkKmeansModelEstimatorTest
  itkKullbackLeiblerCompareHistogramImageToImageMetricTest
  itkLabelVotingImageFilterTest
  itkLaplacianSegmentationLevelSetImageFilterTest
  itkLevelSetMotionRegistrationFilterTest
  itkLevelSetNeighborhoodExtractorTest
  itkLevelSetVelocityNeighborhoodExtractorTest
  itkMIRegistrationFunctionTest
  itkMRFImageFilterTest
  itkMRIBiasFieldCorrectionFilterTest
  itkMatchCardinalityImageToImageMetricTest
  itkMattesMutualInformationImageToImageMetricTest
  itkMeanReciprocalSquareDifferenceImageMetricTest
  itkMeanReciprocalSquareDifferencePointSetToImageMetricTest
  itkMeanSquaresHistogramImageToImageMetricTest
  itkMeanSquaresImageMetricTest
  itkMeanSquaresPointSetToImageMetricTest
  itkMinMaxCurvatureFlowImageFilterTest
  itkMultiResolutionImageRegistrationMethodTest
  itkMultiResolutionImageRegistrationMethodTest_1
  itkMultiResolutionImageRegistrationMethodTest_2
  itkMultiResolutionPDEDeformableRegistrationTest
  itkMultiResolutionPyramidImageFilterTest
  itkMutualInformationHistogramImageToImageMetricTest
  itkMutualInformationMetricTest
  itkNarrowBandCurvesLevelSetImageFilterTest
  itkNarrowBandThresholdSegmentationLevelSetImageFilterTest
  itkNewTest
  itkNormalQuadEdgeMeshFilterTest
  itkNormalizedCorrelationImageMetricTest
  itkNormalizedCorrelationPointSetToImageMetricTest
  itkNormalizedMutualInformationHistogramImageToImageMetricTest
  itkOrthogonalSwath2DPathFilterTest
  itkOtsuMultipleThresholdsCalculatorTest
  itkOtsuMultipleThresholdsImageFilterTest
  itkOtsuThresholdImageCalculatorTest
  itkOtsuThresholdImageFilterTest
  itkParameterizationQuadEdgeMeshFilterTest
  itkPointSetToImageRegistrationTest_1
  itkPointSetToPointSetRegistrationTest
  itkPointSetToSpatialObjectDemonsRegistrationTest
  itkQuadricDecimationQuadEdgeMeshFilterTest
  itkRayCastInterpolateImageFunctionTest
  itkRecursiveMultiResolutionPyramidImageFilterTest
  itkRegionGrow2DTest
  itkRegularSphereMeshSourceTest
  itkRegularSphereQuadEdgeMeshSourceTest
  itkReinitializeLevelSetImageFilterTest
  itkSTAPLEImageFilterTest
  itkScalarImageKmeansImageFilter3DTest
  itkShapeDetectionLevelSetImageFilterTest
  itkShapePriorMAPCostFunctionTest
  itkShapePriorSegmentationLevelSetFunctionTest
  itkSimplexMeshVolumeCalculatorTest
  itkSmoothingQuadEdgeMeshFilterTest
  itkSpatialObjectToImageRegistrationTest
  itkSphereMeshSourceTest
  itkSquaredEdgeLengthDecimationQuadEdgeMeshFilterTest
  itkSupervisedImageClassifierTest
  itkSymmetricForcesDemonsRegistrationFilterTest
  itkThresholdMaximumConnectedComponentsImageFilterTest
  itkThresholdSegmentationLevelSetImageFilterTest
  itkUnsharpMaskLevelSetImageFilterTest
  itkVectorThresholdSegmentationLevelSetImageFilterTest
  itkVoronoiDiagram2DTest
  itkVoronoiPartitioningImageFilterTest
  itkVoronoiSegmentationImageFilterTest
  itkVoronoiSegmentationRGBImageFilterTest
  itkWatershedImageFilterTest
)

# Given KIT, KIT_LIBS and KitTests, build TestDriver
CreateTestDriver(${KIT} "${KIT_LIBS}" "${KitTests}" TestDriver)

#############################################################
# Tests that use the Test Driver
#
add_test(itkAnisotropicFourthOrderLevelSetImageFilterTest ${TestDriver}
         itkAnisotropicFourthOrderLevelSetImageFilterTest)
add_test(itkAntiAliasBinaryImageFilterTest ${TestDriver}
         itkAntiAliasBinaryImageFilterTest)
add_test(itkAutomaticTopologyMeshSourceTest ${TestDriver}
         itkAutomaticTopologyMeshSourceTest)
add_test(itkBinaryMinMaxCurvatureFlowImageFilterTest ${TestDriver}
         itkBinaryMinMaxCurvatureFlowImageFilterTest )
add_test(itkBinaryMask3DMeshSourceTest0 ${TestDriver}
         itkBinaryMask3DMeshSourceTest 0 )
add_test(itkBinaryMask3DMeshSourceTest1 ${TestDriver}
         itkBinaryMask3DMeshSourceTest 1 )
add_test(itkBioCellTest ${TestDriver}
         itkBioCellTest )
add_test(itkBioCellularAggregateTest ${TestDriver}
         itkBioCellularAggregateTest )
add_test(itkBioGeneTest ${TestDriver}
         itkBioGeneTest )
add_test(itkBioGenomeTest ${TestDriver}
         itkBioGenomeTest )
add_test(itkBioGeneNetworkTest ${TestDriver}
         itkBioGeneNetworkTest )
add_test(itkBresenhamLineTest ${TestDriver}
         itkBresenhamLineTest )
add_test(itkCannySegmentationLevelSetImageFilterTest ${TestDriver}
         itkCannySegmentationLevelSetImageFilterTest)
add_test(itkCorrelationCoefficientHistogramImageToImageMetricTest ${TestDriver}
         itkCorrelationCoefficientHistogramImageToImageMetricTest)
add_test(itkCompareHistogramImageToImageMetricTest ${TestDriver}
         itkCompareHistogramImageToImageMetricTest)
add_test(itkCurvesLevelSetImageFilterTest ${TestDriver}
         itkCurvesLevelSetImageFilterTest)
add_test(itkDemonsRegistrationFilterTest ${TestDriver}
         itkDemonsRegistrationFilterTest)
add_test(itkExtensionVelocitiesImageFilterTest ${TestDriver}
         itkExtensionVelocitiesImageFilterTest)
add_test(itkExtractMeshConnectedRegionsTest ${TestDriver}
         itkExtractMeshConnectedRegionsTest)
add_test(itkFastChamferDistanceImageFilterTest ${TestDriver}
         itkFastChamferDistanceImageFilterTest)
add_test(itkFastMarchingTest ${TestDriver}
         itkFastMarchingTest)
add_test(itkFastMarchingTest2 ${TestDriver}
         itkFastMarchingTest2)
add_test(itkFastMarchingExtensionImageFilterTest ${TestDriver}
         itkFastMarchingExtensionImageFilterTest)
add_test(itkFastMarchingUpwindGradientTest ${TestDriver}
         itkFastMarchingUpwindGradientTest)

# This test uses inputs from BayesianClassifierInitializer.
# If that output changes, a new file should be placed in Examples/Data.
add_test(itkBayesianClassifierImageFilterTest ${TestDriver}
  --compare ${BASELINE}/itkBayesianClassifierImageFilterTestLabelMap.png
            ${TEMP}/itkBayesianClassifierImageFilterTestLabelMap.png
  itkBayesianClassifierImageFilterTest
     ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
     ${TEMP}/itkBayesianClassifierImageFilterTestLabelMap.png
     4  # Number of classes
     2  # Number of smoothing iterations
)

add_test(itkCollidingFrontsImageFilterTest ${TestDriver}
         itkCollidingFrontsImageFilterTest)
add_test(itkCurvesLevelSetImageFilterZeroSigmaTest ${TestDriver}
         itkCurvesLevelSetImageFilterZeroSigmaTest)
add_test(itkGeodesicActiveContourLevelSetImageFilterZeroSigmaTest ${TestDriver}
         itkGeodesicActiveContourLevelSetImageFilterZeroSigmaTest)
add_test(itkLabelVotingImageFilterTest ${TestDriver}
         itkLabelVotingImageFilterTest)
add_test(itkBinaryMask3DQuadEdgeMeshSourceTest ${TestDriver}
         itkBinaryMask3DQuadEdgeMeshSourceTest)
add_test(itkAutomaticTopologyQuadEdgeMeshSourceTest ${TestDriver}
         itkAutomaticTopologyQuadEdgeMeshSourceTest)
add_test(itkLevelSetMotionRegistrationFilterTest ${TestDriver}
         itkLevelSetMotionRegistrationFilterTest
  ${TEMP}/itkLevelSetMotionRegistrationFilterTestFixedImage.mha
  ${TEMP}/itkLevelSetMotionRegistrationFilterTestMovingImage.mha
  ${TEMP}/itkLevelSetMotionRegistrationFilterTestResampledImage.mha
)
add_test(itkFEMRegistrationFilterTest ${TestDriver}
         itkFEMRegistrationFilterTest)
add_test(itkGeodesicActiveContourLevelSetImageFilterTest ${TestDriver}
         itkGeodesicActiveContourLevelSetImageFilterTest)
add_test(itkGeodesicActiveContourShapePriorLevelSetImageFilterTest ${TestDriver}
         itkGeodesicActiveContourShapePriorLevelSetImageFilterTest)
add_test(itkGeodesicActiveContourShapePriorLevelSetImageFilterTest_2 ${TestDriver}
         itkGeodesicActiveContourShapePriorLevelSetImageFilterTest_2)
add_test(itkGradientVectorFlowImageFilterTest ${TestDriver}
         itkGradientVectorFlowImageFilterTest)
add_test(itkGradientDifferenceImageToImageMetricTest ${TestDriver}
         itkGradientDifferenceImageToImageMetricTest)
add_test(itkHistogramMatchingImageFilterTest ${TestDriver}
         itkHistogramMatchingImageFilterTest)
add_test(itkHistogramImageToImageMetricTest ${TestDriver}
         itkHistogramImageToImageMetricTest)
add_test(itkImageMomentsTest ${TestDriver}
         itkImageMomentsTest)
add_test(itkImagePCAShapeModelEstimatorTest ${TestDriver}
         itkImagePCAShapeModelEstimatorTest)
add_test(itkImagePCADecompositionCalculatorTest ${TestDriver}
         itkImagePCADecompositionCalculatorTest)
add_test(itkImageRegistrationMethodTest ${TestDriver}
           itkImageRegistrationMethodTest  )
add_test(itkImageRegistrationMethodTest_1 ${TestDriver}
         itkImageRegistrationMethodTest_1)
add_test(itkImageRegistrationMethodTest_2 ${TestDriver}
         itkImageRegistrationMethodTest_2)
add_test(itkImageRegistrationMethodTest_3 ${TestDriver}
         itkImageRegistrationMethodTest_3)
add_test(itkImageRegistrationMethodTest_4 ${TestDriver}
         itkImageRegistrationMethodTest_4)
add_test(itkImageRegistrationMethodTest_5 ${TestDriver}
         itkImageRegistrationMethodTest_5)
add_test(itkImageRegistrationMethodTest_6 ${TestDriver}
         itkImageRegistrationMethodTest_6)
add_test(itkImageRegistrationMethodTest_7 ${TestDriver}
         itkImageRegistrationMethodTest_7)
add_test(itkImageRegistrationMethodTest_8 ${TestDriver}
         itkImageRegistrationMethodTest_8)
add_test(itkImageRegistrationMethodTest_9 ${TestDriver}
         itkImageRegistrationMethodTest_9)
add_test(itkImageRegistrationMethodTest_10 ${TestDriver}
         itkImageRegistrationMethodTest_10)
add_test(itkImageRegistrationMethodTest_11 ${TestDriver}
         itkImageRegistrationMethodTest_11)
add_test(itkImageRegistrationMethodTest_12 ${TestDriver}
         itkImageRegistrationMethodTest_12)
add_test(itkImageRegistrationMethodTest_13 ${TestDriver}
         itkImageRegistrationMethodTest_13)
add_test(itkImageRegistrationMethodTest_14 ${TestDriver}
         itkImageRegistrationMethodTest_14)
add_test(itkImageRegistrationMethodTest_15 ${TestDriver}
         itkImageRegistrationMethodTest_15)
add_test(itkImageRegistrationMethodTest_16 ${TestDriver}
         itkImageRegistrationMethodTest_16)
add_test(itkInterpolateTest ${TestDriver}
         itkInterpolateTest)
add_test(itkIsoContourDistanceImageFilterTest ${TestDriver}
         itkIsoContourDistanceImageFilterTest)
add_test(itkIsotropicFourthOrderLevelSetImageFilterTest ${TestDriver}
         itkIsotropicFourthOrderLevelSetImageFilterTest)
add_test(itkKalmanLinearEstimatorTest ${TestDriver}
         itkKalmanLinearEstimatorTest)
add_test(itkKmeansModelEstimatorTest ${TestDriver}
         itkKmeansModelEstimatorTest)
add_test(itkScalarImageKmeansImageFilter3DTest ${TestDriver}
    --compare ${ITK_SOURCE_DIR}/Examples/Data/KmeansTest_T1KmeansPrelimSegmentation.nii.gz
              ${ITK_TEST_OUTPUT_DIR}/KmeansTest_T1KmeansPrelimSegmentation.nii.gz
         itkScalarImageKmeansImageFilter3DTest
              ${ITK_SOURCE_DIR}/Examples/Data/KmeansTest_T1UCharRaw.nii.gz
              ${ITK_SOURCE_DIR}/Examples/Data/KmeansTest_T1RawSkullStrip.nii.gz
              ${ITK_TEST_OUTPUT_DIR}/KmeansTest_T1KmeansPrelimSegmentation.nii.gz )
add_test(itkKullbackLeiblerCompareHistogramImageToImageMetricTest ${TestDriver}
         itkKullbackLeiblerCompareHistogramImageToImageMetricTest)
add_test(itkLaplacianSegmentationLevelSetImageFilterTest ${TestDriver}
         itkLaplacianSegmentationLevelSetImageFilterTest)
add_test(itkLevelSetNeighborhoodExtractorTest ${TestDriver}
         itkLevelSetNeighborhoodExtractorTest)
add_test(itkLevelSetVelocityNeighborhoodExtractorTest ${TestDriver}
         itkLevelSetVelocityNeighborhoodExtractorTest)
add_test(itkMattesMutualInformationImageToImageMetricTest ${TestDriver}
         itkMattesMutualInformationImageToImageMetricTest 1 1)
add_test(itkMattesMutualInformationImageToImageMetricTest2 ${TestDriver}
         itkMattesMutualInformationImageToImageMetricTest 1 0)

add_test(itkMattesMutualInformationImageToImageMetricTest3 ${TestDriver}
         itkMattesMutualInformationImageToImageMetricTest 0 1)
add_test(itkMattesMutualInformationImageToImageMetricTest4 ${TestDriver}
         itkMattesMutualInformationImageToImageMetricTest 0 0)
add_test(itkMeanSquaresImageMetricTest ${TestDriver}
         itkMeanSquaresImageMetricTest)
add_test(itkMeanSquaresHistogramImageToImageMetricTest ${TestDriver}
         itkMeanSquaresHistogramImageToImageMetricTest)
add_test(itkMinMaxCurvatureFlowImageFilterTest ${TestDriver}
         itkMinMaxCurvatureFlowImageFilterTest)
add_test(itkMRFImageFilterTest ${TestDriver}
         itkMRFImageFilterTest)
add_test(itkMRIBiasFieldCorrectionFilterTest ${TestDriver}
         itkMRIBiasFieldCorrectionFilterTest)
add_test(itkMultiResolutionPyramidImageFilterWithResampleFilterTest ${TestDriver}
         itkMultiResolutionPyramidImageFilterTest Resample)
add_test(itkMultiResolutionPyramidImageFilterWithShrinkFilterTest ${TestDriver}
         itkMultiResolutionPyramidImageFilterTest Shrink)
add_test(itkRecursiveMultiResolutionPyramidImageFilterWithResampleFilterTest ${TestDriver}
         itkRecursiveMultiResolutionPyramidImageFilterTest Resample)
add_test(itkRecursiveMultiResolutionPyramidImageFilterWithShrinkFilterTest ${TestDriver}
         itkRecursiveMultiResolutionPyramidImageFilterTest Shrink)
add_test(itkRecursiveMultiResolutionPyramidImageFilterWithResampleFilterTest2 ${TestDriver}
         itkMultiResolutionPyramidImageFilterTest Resample TestRecursive)
add_test(itkRecursiveMultiResolutionPyramidImageFilterWithShrinkFilterTest2 ${TestDriver}
         itkMultiResolutionPyramidImageFilterTest Shrink TestRecursive)
add_test(itkMutualInformationHistogramImageToImageMetricTest ${TestDriver}
         itkMutualInformationHistogramImageToImageMetricTest)
add_test(itkMultiResolutionPDEDeformableRegistrationTest ${TestDriver}
           --compare ${BASELINE}/itkMultiResolutionPDEDeformableRegistrationTestPixelCentered.png
            ${TEMP}/itkMultiResolutionPDEDeformableRegistrationTestPixelCentered.png
  --compareNumberOfPixelsTolerance 20
  itkMultiResolutionPDEDeformableRegistrationTest
            ${TEMP}/itkMultiResolutionPDEDeformableRegistrationTestPixelCentered.png)
add_test(itkMultiResolutionImageRegistrationMethodTest ${TestDriver}
         itkMultiResolutionImageRegistrationMethodTest  )
add_test(itkMultiResolutionImageRegistrationMethodTest_1 ${TestDriver}
         itkMultiResolutionImageRegistrationMethodTest_1  )
add_test(itkMultiResolutionImageRegistrationMethodTest_2 ${TestDriver}
         itkMultiResolutionImageRegistrationMethodTest_2  )
add_test(itkMutualInformationMetricTest ${TestDriver}
         itkMutualInformationMetricTest)
add_test(itkNarrowBandCurvesLevelSetImageFilterTest1 ${TestDriver}
  --compare ${BASELINE}/itkNarrowBandCurvesLevelSetImageFilterTest.png
            ${TEMP}/itkNarrowBandCurvesLevelSetImageFilterTest.png
  itkNarrowBandCurvesLevelSetImageFilterTest
            ${TEMP}/itkNarrowBandCurvesLevelSetImageFilterTest.png)
add_test(itkNarrowBandThresholdSegmentationLevelSetImageFilterTest ${TestDriver}
         itkNarrowBandThresholdSegmentationLevelSetImageFilterTest)
add_test(itkNewTest ${TestDriver}
         itkNewTest)
add_test(itkNormalizedCorrelationImageMetricTest ${TestDriver}
         itkNormalizedCorrelationImageMetricTest)
add_test(itkNormalizedCorrelationPointSetToImageMetricTest ${TestDriver}
         itkNormalizedCorrelationPointSetToImageMetricTest)
add_test(itkMeanSquaresPointSetToImageMetricTest ${TestDriver}
         itkMeanSquaresPointSetToImageMetricTest)
add_test(itkMeanReciprocalSquareDifferencePointSetToImageMetricTest ${TestDriver}
         itkMeanReciprocalSquareDifferencePointSetToImageMetricTest)
add_test(itkNormalizedMutualInformationHistogramImageToImageMetricTest ${TestDriver}
         itkNormalizedMutualInformationHistogramImageToImageMetricTest)
add_test(itkOtsuThresholdImageCalculatorTest ${TestDriver}
         itkOtsuThresholdImageCalculatorTest)
add_test(itkOtsuMultipleThresholdsCalculatorTest ${TestDriver}
         itkOtsuMultipleThresholdsCalculatorTest)
add_test(itkOrthogonalSwath2DPathFilterTest ${TestDriver}
         itkOrthogonalSwath2DPathFilterTest)
add_test(itkMeanReciprocalSquareDifferenceImageMetricTest ${TestDriver}
          itkMeanReciprocalSquareDifferenceImageMetricTest)
add_test(itkPointSetToImageRegistrationTest_1 ${TestDriver}
          itkPointSetToImageRegistrationTest_1)
add_test(itkRegionGrow2DTest ${TestDriver}
         itkRegionGrow2DTest)
add_test(itkReinitializeLevelSetImageFilterTest ${TestDriver}
         itkReinitializeLevelSetImageFilterTest)
add_test(itkRegularSphereMeshSourceTest ${TestDriver}
         itkRegularSphereMeshSourceTest)
add_test(itkRegularSphereQuadEdgeMeshSourceTest ${TestDriver}
         itkRegularSphereQuadEdgeMeshSourceTest ${ITK_TEST_OUTPUT_DIR}/itkRegularSphereMeshQuadEdgeMeshSourceTest.vtk
  )
add_test(itkRayCastInterpolateImageFunctionTest ${TestDriver}
         itkRayCastInterpolateImageFunctionTest)
add_test(itkSimplexMeshVolumeCalculatorTest ${TestDriver}
         itkSimplexMeshVolumeCalculatorTest)
add_test(itkShapeDetectionLevelSetImageFilterTest ${TestDriver}
         itkShapeDetectionLevelSetImageFilterTest)
add_test(itkShapePriorMAPCostFunctionTest ${TestDriver}
         itkShapePriorMAPCostFunctionTest)
add_test(itkShapePriorSegmentationLevelSetFunctionTest ${TestDriver}
         itkShapePriorSegmentationLevelSetFunctionTest)
add_test(itkSpatialObjectToImageRegistrationTest ${TestDriver}
         itkSpatialObjectToImageRegistrationTest)
add_test(itkSupervisedImageClassifierTest ${TestDriver}
         itkSupervisedImageClassifierTest)
add_test(itkThresholdSegmentationLevelSetImageFilterTest ${TestDriver}
         itkThresholdSegmentationLevelSetImageFilterTest)
add_test(itkGibbsTest ${TestDriver}
         itkGibbsTest)
add_test(itkDeformableTest ${TestDriver}
         itkDeformableTest)
add_test(itkDeformableSimplexMesh3DBalloonForceFilterTest ${TestDriver}
         itkDeformableSimplexMesh3DBalloonForceFilterTest)
add_test(itkDeformableSimplexMesh3DGradientConstaintForceFilterTest ${TestDriver}
         itkDeformableSimplexMesh3DGradientConstraintForceFilterTest)
add_test(itkDeformableSimplexMesh3DFilterTest ${TestDriver}
         itkDeformableSimplexMesh3DFilterTest)
add_test(itk2DDeformableTest ${TestDriver}
         itk2DDeformableTest)
add_test(itkSphereMeshSourceTest ${TestDriver}
         itkSphereMeshSourceTest)
add_test(itkUnsharpMaskLevelSetImageFilterTest ${TestDriver}
         itkUnsharpMaskLevelSetImageFilterTest)
add_test(itkMIRegistrationFunctionTest ${TestDriver}
         itkMIRegistrationFunctionTest)
add_test(itkSymmetricForcesDemonsRegistrationFilterTest ${TestDriver}
         itkSymmetricForcesDemonsRegistrationFilterTest)
add_test(itkVoronoiDiagram2DTest ${TestDriver}
         itkVoronoiDiagram2DTest)
add_test(itkVoronoiSegmentationImageFilterTest ${TestDriver}
         itkVoronoiSegmentationImageFilterTest)
add_test(itkVoronoiSegmentationRGBImageFilterTest ${TestDriver}
         itkVoronoiSegmentationRGBImageFilterTest)
add_test(itkWatershedImageFilterTest ${TestDriver}
         itkWatershedImageFilterTest)
add_test(itkPointSetToPointSetRegistrationTest ${TestDriver}
         itkPointSetToPointSetRegistrationTest)
add_test(itkPointSetToSpatialObjectDemonsRegistrationTest ${TestDriver}
         itkPointSetToSpatialObjectDemonsRegistrationTest)
add_test(itkCurvatureFlowTest ${TestDriver}
         itkCurvatureFlowTest ${ITK_TEST_OUTPUT_DIR}/itkCurvatureFlowTest.vtk)

# Metric fails when centered pixel coordinates not in use
add_test(itkBinaryMedialNodeMetricTest ${TestDriver}
         itkBinaryMedialNodeMetricTest)

############################################################
# Some tests require data
#
if(ITK_DATA_ROOT)

set( CURV_TESTS Gaussian Maximum Mean Minimum )
foreach( loop_var ${CURV_TESTS} )
  add_test( itkDiscrete${loop_var}CurvatureQuadEdgeMeshFilterTest
    ${TestDriver}
    itkDiscrete${loop_var}CurvatureQuadEdgeMeshFilterTest
    ${INPUTDATA}/mushroom.vtk
    )
endforeach( loop_var )

foreach( border 0 1 )
  foreach( coeff 0 1 2 3 4 )
    add_test( itkParameterizationQuadEdgeMeshFilterTest${border}_${coeff}
    ${TestDriver}
    itkParameterizationQuadEdgeMeshFilterTest
    ${INPUTDATA}/mushroom.vtk ${border} ${coeff}
    ${TEMP}/param_border${border}_coeff${coeff}.vtk
    )
  endforeach( coeff )
endforeach( border )

add_test( itkDelaunayConformingQuadEdgeMeshFilterTest ${TestDriver}
  itkDelaunayConformingQuadEdgeMeshFilterTest
  ${INPUTDATA}/mushroom.vtk
  ${TEMP}/mushrom_delaunay.vtk
  )

add_test( itkQuadricDecimationQuadEdgeMeshFilterTest ${TestDriver}
  itkQuadricDecimationQuadEdgeMeshFilterTest
  ${INPUTDATA}/mushroom.vtk
  100
  ${TEMP}/temp_QuadricDecimationResult1.vtk
  )
add_test( itkQuadEdgeMeshQuadricDecimationTetrahedronTest ${TestDriver}
  itkQuadricDecimationQuadEdgeMeshFilterTest
  ${INPUTDATA}/tetrahedron.vtk
  2
  ${TEMP}/temp_QuadricDecimationTetrahedron.vtk
  )
add_test( itkSquaredEdgeLengthDecimationQuadEdgeMeshFilterTest ${TestDriver}
  itkSquaredEdgeLengthDecimationQuadEdgeMeshFilterTest
  ${INPUTDATA}/mushroom.vtk
  20
  ${TEMP}/temp_SquaredEdgeLengthDecimationResult1.vtk
  )
add_test( itkCleanQuadEdgeMeshFilterTest
  ${TestDriver}
  itkCleanQuadEdgeMeshFilterTest
  ${INPUTDATA}/mushroom.vtk
  0.1
  ${TEMP}/temp_CleanResult1.vtk
  )
add_test( itkNormalQuadEdgeMeshFilterTest0 ${TestDriver}
  itkNormalQuadEdgeMeshFilterTest
  ${INPUTDATA}/mushroom.vtk
  0
)
add_test( itkNormalQuadEdgeMeshFilterTest1 ${TestDriver}
  itkNormalQuadEdgeMeshFilterTest
  ${INPUTDATA}/mushroom.vtk
  1
)
add_test( itkNormalQuadEdgeMeshFilterTest2 ${TestDriver}
  itkNormalQuadEdgeMeshFilterTest
  ${INPUTDATA}/mushroom.vtk
  2
)

add_test( itkSmoothingQuadEdgeMeshFilterTest0 ${TestDriver}
  itkSmoothingQuadEdgeMeshFilterTest
  ${INPUTDATA}/genusZeroSurface01.vtk
  10
  0.1
  0
  ${TEMP}/temp_SmoothResult0.vtk
  )

add_test( itkSmoothingQuadEdgeMeshFilterTest1 ${TestDriver}
  itkSmoothingQuadEdgeMeshFilterTest
  ${INPUTDATA}/genusZeroSurface01.vtk
  10
  0.1
  1
  ${TEMP}/temp_SmoothResult1.vtk
  )

add_test( itkThresholdMaximumConnectedComponentsImageFilterTest1 ${TestDriver}
  --compare ${BASELINE}/itkThresholdMaximumConnectedComponentsImageFilterTest1.png
            ${TEMP}/itkThresholdMaximumConnectedComponentsImageFilterTest1.png
         itkThresholdMaximumConnectedComponentsImageFilterTest
           ${INPUTDATA}/CellsFluorescence1.png
           ${TEMP}/itkThresholdMaximumConnectedComponentsImageFilterTest1.png 10)

add_test( itkThresholdMaximumConnectedComponentsImageFilterTest2 ${TestDriver}
  --compare ${BASELINE}/itkThresholdMaximumConnectedComponentsImageFilterTest2.png
            ${TEMP}/itkThresholdMaximumConnectedComponentsImageFilterTest2.png
          itkThresholdMaximumConnectedComponentsImageFilterTest
          ${INPUTDATA}/CellsFluorescence2.png
          ${TEMP}/itkThresholdMaximumConnectedComponentsImageFilterTest2.png 10)

add_test(itkBinaryThinningImageFilterTest ${TestDriver}
   --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/BinaryThinningImageFilterTest.png
             ${ITK_TEST_OUTPUT_DIR}/BinaryThinningImageFilterTest.png
  itkBinaryThinningImageFilterTest
             ${ITK_DATA_ROOT}/Input/Shapes.png
             ${ITK_TEST_OUTPUT_DIR}/BinaryThinningImageFilterTest.png
             )

add_test(itkMatchCardinalityImageToImageMetricTest ${TestDriver}
  itkMatchCardinalityImageToImageMetricTest ${ITK_DATA_ROOT}/Input/Spots.png)

add_test(itkKappaStatisticImageToImageMetricTest ${TestDriver}
  itkKappaStatisticImageToImageMetricTest ${ITK_DATA_ROOT}/Input/Spots.png)

add_test(itkIsolatedWatershedImageFilterTest ${TestDriver}
   --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/IsolatedWatershedImageFilterTest.png
             ${ITK_TEST_OUTPUT_DIR}/IsolatedWatershedImageFilterTest.png
  itkIsolatedWatershedImageFilterTest
             ${ITK_DATA_ROOT}/Input/cthead1.png
             ${ITK_TEST_OUTPUT_DIR}/IsolatedWatershedImageFilterTest.png
             113 84 120 99)

add_test(itkOtsuThresholdImageFilterTest ${TestDriver}
  --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/OtsuThresholdImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/OtsuThresholdImageFilterTest.png
  itkOtsuThresholdImageFilterTest
            ${ITK_DATA_ROOT}/Input/cthead1.png
            ${ITK_TEST_OUTPUT_DIR}/OtsuThresholdImageFilterTest.png
            64)

add_test(itkOtsuMultipleThresholdsImageFilterTest ${TestDriver}
  --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/OtsuMultipleThresholdsImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/OtsuMultipleThresholdsImageFilterTest.png
  itkOtsuMultipleThresholdsImageFilterTest
            ${ITK_DATA_ROOT}/Input/peppers.png
            ${ITK_TEST_OUTPUT_DIR}/OtsuMultipleThresholdsImageFilterTest.png
            64 3 2)

add_test(itkVoronoiPartitioningImageFilterTest1 ${TestDriver}
  --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/VoronoiPartioningImageFilterTest1.png
            ${ITK_TEST_OUTPUT_DIR}/VoronoiPartioningImageFilterTest1.png
  itkVoronoiPartitioningImageFilterTest
            ${ITK_DATA_ROOT}/Input/sf4.png
            ${ITK_TEST_OUTPUT_DIR}/VoronoiPartioningImageFilterTest1.png 1)

add_test(itkVoronoiPartitioningImageFilterTest2 ${TestDriver}
  --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/VoronoiPartioningImageFilterTest2.png
            ${ITK_TEST_OUTPUT_DIR}/VoronoiPartioningImageFilterTest2.png
  itkVoronoiPartitioningImageFilterTest
            ${ITK_DATA_ROOT}/Input/sf4.png
            ${ITK_TEST_OUTPUT_DIR}/VoronoiPartioningImageFilterTest2.png 0)

add_test(itkVectorThresholdSegmentationLevelSetImageFilterTest ${TestDriver}
  --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/VectorThresholdSegmentationLevelSetImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/VectorThresholdSegmentationLevelSetImageFilterTest.png
  itkVectorThresholdSegmentationLevelSetImageFilterTest
            ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSliceInitialLevelSet.png
            ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png
            ${ITK_TEST_OUTPUT_DIR}/VectorThresholdSegmentationLevelSetImageFilterTest.png
            5.0 50
            )

add_test(itkSTAPLEImageFilterTest ${TestDriver}
   --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/STAPLEImageFilterTest.mha
             ${ITK_TEST_OUTPUT_DIR}/STAPLEImageFilterTest.mha
   itkSTAPLEImageFilterTest
             2 ${ITK_TEST_OUTPUT_DIR}/STAPLEImageFilterTest.mha 255 0.5
             ${ITK_DATA_ROOT}/Input/STAPLE1.png
             ${ITK_DATA_ROOT}/Input/STAPLE2.png
             ${ITK_DATA_ROOT}/Input/STAPLE3.png
             ${ITK_DATA_ROOT}/Input/STAPLE4.png
            )

endif(ITK_DATA_ROOT)

#
# FFT Tests are kept in a separate old-style test driver since
# one source file contains all of the tests
if(USE_FFTWD)
  set(CURVATUREREGISTRATION_SRCS itkCurvatureRegistrationFilterTest)
endif(USE_FFTWD)
set(FFTTest_SRCS
  itkFFTTest.cxx
  ${CURVATUREREGISTRATION_SRCS}
)
add_executable(itkFFTTests itkFFTTests.cxx ${FFTTest_SRCS})
target_link_libraries(itkFFTTests ITKAlgorithms ITKFEM ITKStatistics ITKNumerics)

set(FFTTestDriver ${CXX_TEST_PATH}/itkFFTTests)

add_test(itkVnlFFTTest ${FFTTestDriver} itkVnlFFTTest)
if(USE_FFTWF)
       add_test(itkFFTWF_FFTTest ${FFTTestDriver} itkFFTWF_FFTTest ${ITK_TEST_OUTPUT_DIR} )

       add_test(itkVnlFFTWF_FFTTest ${FFTTestDriver} itkVnlFFTWF_FFTTest )
       set_tests_properties(itkVnlFFTWF_FFTTest PROPERTIES ENVIRONMENT
         "ITK_FFTW_READ_WISDOM_CACHE=oN;ITK_FFTW_WRITE_WISDOM_CACHE=oN;ITK_FFTW_WISDOM_CACHE_FILE=${ITK_TEST_OUTPUT_DIR}/.wisdom_from_ITK_FFTW_WISDOM_CACHE_FILE;ITK_FFTW_PLAN_RIGOR=FFTW_EXHAUSTIVE"
         )

endif(USE_FFTWF)
if(USE_FFTWD)
       add_test(itkFFTWD_FFTTest ${FFTTestDriver} itkFFTWD_FFTTest ${ITK_TEST_OUTPUT_DIR} )

       add_test(itkVnlFFTWD_FFTTest ${FFTTestDriver} itkVnlFFTWD_FFTTest)
       set_tests_properties(itkVnlFFTWD_FFTTest PROPERTIES ENVIRONMENT
         "ITK_FFTW_READ_WISDOM_CACHE=oN;ITK_FFTW_WISDOM_CACHE_BASE=${ITK_TEST_OUTPUT_DIR};ITK_FFTW_PLAN_RIGOR=FFTW_EXHAUSTIVE;ITK_FFTW_WRITE_WISDOM_CACHE=oN")
endif(USE_FFTWD)
if(USE_FFTWD)
       add_test(itkCurvatureRegistrationFilterTest ${FFTTestDriver} itkCurvatureRegistrationFilterTest)
endif(USE_FFTWD)



if(ITK_OBJCXX_COMPILER_WORKS)
  set(ALGORITHMS_OBJCXX_TEST ${CXX_TEST_PATH}/itkAlgorithmsHeaderObjCxxTest)
  add_test(itkAlgorithmsHeaderObjCxxTest ${ALGORITHMS_OBJCXX_TEST})
  add_executable(itkAlgorithmsHeaderObjCxxTest itkAlgorithmsHeaderObjCxxTest.mm)
  target_link_libraries(itkAlgorithmsHeaderObjCxxTest ITKAlgorithms ITKFEM ITKNumerics)
endif(ITK_OBJCXX_COMPILER_WORKS)

set(ALGORITHMS_PRINT_TEST ${CXX_TEST_PATH}/itkAlgorithmsPrintTest)
set(ALGORITHMS_PRINT_TEST2 ${CXX_TEST_PATH}/itkAlgorithmsPrintTest2)
set(ALGORITHMS_PRINT_TEST3 ${CXX_TEST_PATH}/itkAlgorithmsPrintTest3)
set(ALGORITHMS_PRINT_TEST4 ${CXX_TEST_PATH}/itkAlgorithmsPrintTest4)

set(ALGORITHMS_HEADER_TESTS ${CXX_TEST_PATH}/itkAlgorithmsHeaderTest)

add_test(itkAlgorithmsPrintTest ${ALGORITHMS_PRINT_TEST})
add_test(itkAlgorithmsPrintTest2 ${ALGORITHMS_PRINT_TEST2})
add_test(itkAlgorithmsPrintTest3 ${ALGORITHMS_PRINT_TEST3})
add_test(itkAlgorithmsPrintTest4 ${ALGORITHMS_PRINT_TEST4})

add_executable(itkAlgorithmsHeaderTest itkAlgorithmsHeaderTest.cxx)
target_link_libraries(itkAlgorithmsHeaderTest ITKAlgorithms ITKFEM ITKNumerics)
add_test(itkAlgorithmsHeaderTest ${ALGORITHMS_HEADER_TESTS})

#
# Print Tests each have their own executable
#
add_executable(itkAlgorithmsPrintTest itkAlgorithmsPrintTest.cxx)
target_link_libraries(itkAlgorithmsPrintTest ITKAlgorithms ITKFEM ITKNumerics)

add_executable(itkAlgorithmsPrintTest2 itkAlgorithmsPrintTest2.cxx)
target_link_libraries(itkAlgorithmsPrintTest2 ITKAlgorithms ITKFEM ITKNumerics)

# Avoid bogus uninitialied warning in itkWatershedSegementTreeGenerator
# by Sun compilers
include(${ITK_SOURCE_DIR}/CMake/itkCheckCXXAcceptsFlags.cmake)
itkCHECK_CXX_ACCEPTS_FLAGS("-features=no%anachronisms" SUN_COMPILER)
if(SUN_COMPILER)
  set_source_files_properties(itkAlgorithmsPrintTest3.cxx
                        PROPERTIES COMPILE_FLAGS -w)
endif(SUN_COMPILER)

add_executable(itkAlgorithmsPrintTest3 itkAlgorithmsPrintTest3.cxx)
target_link_libraries(itkAlgorithmsPrintTest3 ITKAlgorithms ITKFEM ITKNumerics)

add_executable(itkAlgorithmsPrintTest4 itkAlgorithmsPrintTest4.cxx)
target_link_libraries(itkAlgorithmsPrintTest4 ITKAlgorithms ITKFEM ITKStatistics ITKNumerics)


endif( NOT ITK_DISABLE_CXX_TESTING )

############################################################
# TCl Testing
#
if( NOT ITK_DISABLE_TCL_TESTING )
if(ITK_CSWIG_TCL)

  add_test(itkCurvatureFlowTestTcl2 ${ITKWISH_EXECUTABLE}
           ${ITKTesting_SOURCE_DIR}/Code/Algorithms/itkCurvatureFlowTestTcl2.tcl)

  add_test(ImageRegistration3Tcl ${ITKWISH_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Registration/ImageRegistration3.tcl
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
           ${ITK_TEST_OUTPUT_DIR}/ImageRegistration3TclTest.png )

  add_test(ImageRegistration4Tcl ${ITKWISH_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Registration/ImageRegistration4.tcl
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
           ${ITK_TEST_OUTPUT_DIR}/ImageRegistration4TclTest.png )

  add_test(ImageRegistration5Tcl ${ITKWISH_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Registration/ImageRegistration5.tcl
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
           ${ITK_TEST_OUTPUT_DIR}/ImageRegistration5TclTest.png )

  add_test(VoronoiSegmentationTcl ${ITKWISH_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Segmentation/VoronoiSegmentation.tcl
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1Slice.png
           ${ITK_SOURCE_DIR}/Examples/Data/VentricleModel.png
           ${ITK_TEST_OUTPUT_DIR}/VoronoiSegmentationTcl.png  0.3 0.3 )

  add_test(WatershedSegmentation1Tcl ${ITKWISH_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Segmentation/WatershedSegmentation1.tcl
           ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
           ${ITK_TEST_OUTPUT_DIR}/WatershedSegmentation1Tcl.mhd )

endif(ITK_CSWIG_TCL)
endif( NOT ITK_DISABLE_TCL_TESTING )


############################################################
# Python Testing
#
if( NOT ITK_DISABLE_PYTHON_TESTING )
if(ITK_CSWIG_PYTHON)

  add_test(itkCurvatureFlowTestPython2 ${PYTHON_EXECUTABLE}
           ${ITKTesting_SOURCE_DIR}/Code/Algorithms/itkCurvatureFlowTestPython2.py)

  add_test(ImageRegistration3Python ${PYTHON_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Registration/ImageRegistration3.py
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
           ${ITK_TEST_OUTPUT_DIR}/ImageRegistration3PythonTest.png )

  add_test(ImageRegistration4Python ${PYTHON_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Registration/ImageRegistration4.py
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
           ${ITK_TEST_OUTPUT_DIR}/ImageRegistration4PythonTest.png )

  add_test(ImageRegistration5Python ${PYTHON_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Registration/ImageRegistration5.py
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
           ${ITK_TEST_OUTPUT_DIR}/ImageRegistration5PythonTest.png )

  add_test(VoronoiSegmentationPython ${PYTHON_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Segmentation/VoronoiSegmentation.py
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1Slice.png
           ${ITK_SOURCE_DIR}/Examples/Data/VentricleModel.png
           ${ITK_TEST_OUTPUT_DIR}/VoronoiSegmentationPython.png  0.3 0.3 )

  add_test(WatershedSegmentation1Python ${PYTHON_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Segmentation/WatershedSegmentation1.py
           ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
           ${ITK_TEST_OUTPUT_DIR}/WatershedSegmentation1Python.mhd )

endif(ITK_CSWIG_PYTHON)
endif( NOT ITK_DISABLE_PYTHON_TESTING )


############################################################
# Java Testing
#
if( NOT ITK_DISABLE_JAVA_TESTING )
if(ITK_CSWIG_JAVA AND ITK_BINARY_DIR)
  set(JAVACLASSPATH ${ITK_BINARY_DIR}/Wrapping/CSwig/Java/InsightToolkit.jar)
  foreach(cfgtype ${CMAKE_CONFIGURATION_TYPES})
    set(JAVACLASSPATH "${JAVACLASSPATH}\\;${ITK_BINARY_DIR}/Wrapping/CSwig/Java/${cfgtype}/InsightToolkit.jar")
  endforeach(cfgtype)

  foreach( javatestfile
     VoronoiSegmentation
     WatershedSegmentation1
     )

  add_custom_target(${javatestfile}Java ALL  DEPENDS ${ITK_BINARY_DIR}/Examples/Segmentation/${javatestfile}.class)

  add_custom_command(
    OUTPUT  ${ITK_BINARY_DIR}/Examples/Segmentation/${javatestfile}.class
    DEPENDS ${ITK_SOURCE_DIR}/Examples/Segmentation/${javatestfile}.java
    COMMAND ${JAVA_COMPILE}
    ARGS -classpath "${JAVACLASSPATH}" -d "${ITK_BINARY_DIR}/Examples/Segmentation"
    ${ITK_SOURCE_DIR}/Examples/Segmentation/${javatestfile}.java
    COMMENT "Java Class")

  endforeach( javatestfile )

  if(WIN32)
    set(TMPJAVACLASSPATH ${ITK_BINARY_DIR}/Examples/Segmentation/)
    foreach(javadirectory ${JAVACLASSPATH})
      # Set path separator for Windows
      set(TMPJAVACLASSPATH "${TMPJAVACLASSPATH}\\;${javadirectory}")
    endforeach(javadirectory)
    set(JAVACLASSPATH ${TMPJAVACLASSPATH})
  else(WIN32)
    set(TMPJAVACLASSPATH ${ITK_BINARY_DIR}/Examples/Segmentation/)
    foreach(javadirectory ${JAVACLASSPATH})
      # Set path separator for UNIX
      set(TMPJAVACLASSPATH "${TMPJAVACLASSPATH}:${javadirectory}")
    endforeach(javadirectory)
    set(JAVACLASSPATH ${TMPJAVACLASSPATH})
  endif(WIN32)

  add_test(VoronoiSegmentationJava ${JAVA_RUNTIME} -classpath "${JAVACLASSPATH}"
           VoronoiSegmentation
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1Slice.png
           ${ITK_SOURCE_DIR}/Examples/Data/VentricleModel.png
           ${ITK_TEST_OUTPUT_DIR}/VoronoiSegmentationJava.png  0.3 0.3 )

  add_test(WatershedSegmentation1Java ${JAVA_RUNTIME} -classpath "${JAVACLASSPATH}"
           WatershedSegmentation1
           ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
           ${ITK_TEST_OUTPUT_DIR}/WatershedSegmentation1Java.mhd  )


endif(ITK_CSWIG_JAVA AND ITK_BINARY_DIR)
endif( NOT ITK_DISABLE_JAVA_TESTING )
