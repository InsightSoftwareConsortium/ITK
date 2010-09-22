if( NOT ITK_DISABLE_CXX_TESTING )
if(ITK_BINARY_DIR)
  if(TCL_TCLSH)
    add_test(PrintSelf-Algorithms ${TCL_TCLSH}
             ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl
             ${ITK_SOURCE_DIR}/Code/Algorithms)
  endif(TCL_TCLSH)
endif(ITK_BINARY_DIR)

set(BASELINE ${ITK_DATA_ROOT}/Baseline/Algorithms)
set(INPUTDATA ${ITK_DATA_ROOT}/Input)
set(TEMP ${ITKTesting_BINARY_DIR}/Temporary)

set(ALGORITHMS_TESTS ${CXX_TEST_PATH}/itkAlgorithmsTests)
set(ALGORITHMS_TESTS2 ${CXX_TEST_PATH}/itkAlgorithmsTests2)
set(ALGORITHMS_TESTS3 ${CXX_TEST_PATH}/itkAlgorithmsTests3)
set(ALGORITHMS_TESTS4 ${CXX_TEST_PATH}/itkAlgorithmsTests4)

set(ALGORITHMS_PRINT_TEST ${CXX_TEST_PATH}/itkAlgorithmsPrintTest)
set(ALGORITHMS_PRINT_TEST2 ${CXX_TEST_PATH}/itkAlgorithmsPrintTest2)
set(ALGORITHMS_PRINT_TEST3 ${CXX_TEST_PATH}/itkAlgorithmsPrintTest3)
set(ALGORITHMS_PRINT_TEST4 ${CXX_TEST_PATH}/itkAlgorithmsPrintTest4)

set(ALGORITHMS_HEADER_TESTS ${CXX_TEST_PATH}/itkAlgorithmsHeaderTest)

add_test(itkAlgorithmsPrintTest ${ALGORITHMS_PRINT_TEST})
add_test(itkAlgorithmsPrintTest2 ${ALGORITHMS_PRINT_TEST2})
add_test(itkAlgorithmsPrintTest3 ${ALGORITHMS_PRINT_TEST3})
add_test(itkAlgorithmsPrintTest4 ${ALGORITHMS_PRINT_TEST4})

add_test(itkAnisotropicFourthOrderLevelSetImageFilterTest ${ALGORITHMS_TESTS} itkAnisotropicFourthOrderLevelSetImageFilterTest)
add_test(itkAntiAliasBinaryImageFilterTest ${ALGORITHMS_TESTS} itkAntiAliasBinaryImageFilterTest)
add_test(itkAutomaticTopologyMeshSourceTest ${ALGORITHMS_TESTS} itkAutomaticTopologyMeshSourceTest)
add_test(itkBinaryMinMaxCurvatureFlowImageFilterTest ${ALGORITHMS_TESTS} itkBinaryMinMaxCurvatureFlowImageFilterTest )
add_test(itkBinaryMask3DMeshSourceTest ${ALGORITHMS_TESTS} itkBinaryMask3DMeshSourceTest )
add_test(itkBioCellTest ${ALGORITHMS_TESTS} itkBioCellTest )
add_test(itkBioCellularAggregateTest ${ALGORITHMS_TESTS} itkBioCellularAggregateTest )
add_test(itkBioGeneTest ${ALGORITHMS_TESTS} itkBioGeneTest )
add_test(itkBioGenomeTest ${ALGORITHMS_TESTS} itkBioGenomeTest )
add_test(itkBioGeneNetworkTest ${ALGORITHMS_TESTS} itkBioGeneNetworkTest )
add_test(itkCannySegmentationLevelSetImageFilterTest ${ALGORITHMS_TESTS} itkCannySegmentationLevelSetImageFilterTest)
add_test(itkCorrelationCoefficientHistogramImageToImageMetricTest ${ALGORITHMS_TESTS} itkCorrelationCoefficientHistogramImageToImageMetricTest)
add_test(itkCompareHistogramImageToImageMetricTest ${ALGORITHMS_TESTS2} itkCompareHistogramImageToImageMetricTest)
add_test(itkCurvesLevelSetImageFilterTest ${ALGORITHMS_TESTS} itkCurvesLevelSetImageFilterTest)
add_test(itkDemonsRegistrationFilterTest ${ALGORITHMS_TESTS} itkDemonsRegistrationFilterTest)
add_test(itkExtensionVelocitiesImageFilterTest ${ALGORITHMS_TESTS} itkExtensionVelocitiesImageFilterTest)
add_test(itkExtractMeshConnectedRegionsTest ${ALGORITHMS_TESTS} itkExtractMeshConnectedRegionsTest)
add_test(itkFastChamferDistanceImageFilterTest ${ALGORITHMS_TESTS} itkFastChamferDistanceImageFilterTest)
add_test(itkFastMarchingTest ${ALGORITHMS_TESTS} itkFastMarchingTest)
add_test(itkFastMarchingExtensionImageFilterTest ${ALGORITHMS_TESTS} itkFastMarchingExtensionImageFilterTest)
add_test(itkFastMarchingUpwindGradientTest ${ALGORITHMS_TESTS4} itkFastMarchingUpwindGradientTest)

# This test uses inputs from BayesianClassifierInitializer.
# If that output changes, a new file should be placed in Examples/Data.
add_test(itkBayesianClassifierImageFilterTest ${ALGORITHMS_TESTS4}
  --compare ${BASELINE}/itkBayesianClassifierImageFilterTestLabelMap.png
            ${TEMP}/itkBayesianClassifierImageFilterTestLabelMap.png
  itkBayesianClassifierImageFilterTest
     ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
     ${TEMP}/itkBayesianClassifierImageFilterTestLabelMap.png
     4  # Number of classes
     2  # Number of smoothing iterations
)

add_test(itkCollidingFrontsImageFilterTest ${ALGORITHMS_TESTS4} itkCollidingFrontsImageFilterTest)
add_test(itkCurvesLevelSetImageFilterZeroSigmaTest ${ALGORITHMS_TESTS4} itkCurvesLevelSetImageFilterZeroSigmaTest)
add_test(itkGeodesicActiveContourLevelSetImageFilterZeroSigmaTest ${ALGORITHMS_TESTS4} itkGeodesicActiveContourLevelSetImageFilterZeroSigmaTest)
add_test(itkLabelVotingImageFilterTest ${ALGORITHMS_TESTS4} itkLabelVotingImageFilterTest)

add_test(itkLevelSetMotionRegistrationFilterTest ${ALGORITHMS_TESTS4}
  itkLevelSetMotionRegistrationFilterTest
  ${TEMP}/itkLevelSetMotionRegistrationFilterTestFixedImage.mha
  ${TEMP}/itkLevelSetMotionRegistrationFilterTestMovingImage.mha
  ${TEMP}/itkLevelSetMotionRegistrationFilterTestResampledImage.mha
)

add_test(itkFEMRegistrationFilterTest ${ALGORITHMS_TESTS} itkFEMRegistrationFilterTest)
add_test(itkGeodesicActiveContourLevelSetImageFilterTest ${ALGORITHMS_TESTS} itkGeodesicActiveContourLevelSetImageFilterTest)
add_test(itkGeodesicActiveContourShapePriorLevelSetImageFilterTest ${ALGORITHMS_TESTS} itkGeodesicActiveContourShapePriorLevelSetImageFilterTest)
add_test(itkGeodesicActiveContourShapePriorLevelSetImageFilterTest_2 ${ALGORITHMS_TESTS} itkGeodesicActiveContourShapePriorLevelSetImageFilterTest_2)
add_test(itkGradientVectorFlowImageFilterTest ${ALGORITHMS_TESTS} itkGradientVectorFlowImageFilterTest)
add_test(itkGradientDifferenceImageToImageMetricTest ${ALGORITHMS_TESTS2} itkGradientDifferenceImageToImageMetricTest)
add_test(itkHistogramMatchingImageFilterTest ${ALGORITHMS_TESTS} itkHistogramMatchingImageFilterTest)
add_test(itkHistogramImageToImageMetricTest ${ALGORITHMS_TESTS} itkHistogramImageToImageMetricTest)
add_test(itkImageMomentsTest ${ALGORITHMS_TESTS} itkImageMomentsTest)
add_test(itkImagePCAShapeModelEstimatorTest ${ALGORITHMS_TESTS2} itkImagePCAShapeModelEstimatorTest)
add_test(itkImagePCADecompositionCalculatorTest ${ALGORITHMS_TESTS2} itkImagePCADecompositionCalculatorTest)
add_test(itkImageRegistrationMethodTest ${ALGORITHMS_TESTS}   itkImageRegistrationMethodTest  )
add_test(itkImageRegistrationMethodTest_1 ${ALGORITHMS_TESTS} itkImageRegistrationMethodTest_1)
add_test(itkImageRegistrationMethodTest_2 ${ALGORITHMS_TESTS} itkImageRegistrationMethodTest_2)
add_test(itkImageRegistrationMethodTest_3 ${ALGORITHMS_TESTS} itkImageRegistrationMethodTest_3)
add_test(itkImageRegistrationMethodTest_4 ${ALGORITHMS_TESTS} itkImageRegistrationMethodTest_4)
add_test(itkImageRegistrationMethodTest_5 ${ALGORITHMS_TESTS} itkImageRegistrationMethodTest_5)
add_test(itkImageRegistrationMethodTest_6 ${ALGORITHMS_TESTS} itkImageRegistrationMethodTest_6)
add_test(itkImageRegistrationMethodTest_7 ${ALGORITHMS_TESTS} itkImageRegistrationMethodTest_7)
add_test(itkImageRegistrationMethodTest_8 ${ALGORITHMS_TESTS} itkImageRegistrationMethodTest_8)
add_test(itkImageRegistrationMethodTest_9 ${ALGORITHMS_TESTS} itkImageRegistrationMethodTest_9)
add_test(itkImageRegistrationMethodTest_10 ${ALGORITHMS_TESTS} itkImageRegistrationMethodTest_10)
add_test(itkImageRegistrationMethodTest_11 ${ALGORITHMS_TESTS} itkImageRegistrationMethodTest_11)
add_test(itkImageRegistrationMethodTest_12 ${ALGORITHMS_TESTS} itkImageRegistrationMethodTest_12)
add_test(itkImageRegistrationMethodTest_13 ${ALGORITHMS_TESTS} itkImageRegistrationMethodTest_13)
add_test(itkImageRegistrationMethodTest_14 ${ALGORITHMS_TESTS} itkImageRegistrationMethodTest_14)
add_test(itkImageRegistrationMethodTest_15 ${ALGORITHMS_TESTS} itkImageRegistrationMethodTest_15)
add_test(itkImageRegistrationMethodTest_16 ${ALGORITHMS_TESTS} itkImageRegistrationMethodTest_16)
add_test(itkInterpolateTest ${ALGORITHMS_TESTS} itkInterpolateTest)
add_test(itkIsoContourDistanceImageFilterTest ${ALGORITHMS_TESTS} itkIsoContourDistanceImageFilterTest)
add_test(itkIsotropicFourthOrderLevelSetImageFilterTest ${ALGORITHMS_TESTS} itkIsotropicFourthOrderLevelSetImageFilterTest)
add_test(itkKalmanLinearEstimatorTest ${ALGORITHMS_TESTS} itkKalmanLinearEstimatorTest)
add_test(itkKmeansModelEstimatorTest ${ALGORITHMS_TESTS} itkKmeansModelEstimatorTest)
add_test(itkScalarImageKmeansImageFilter3DTest ${ALGORITHMS_TESTS4}
    --compare ${ITK_SOURCE_DIR}/Examples/Data/KmeansTest_T1KmeansPrelimSegmentation.nii.gz
            ${ITK_TEST_OUTPUT_DIR}/KmeansTest_T1KmeansPrelimSegmentation.nii.gz
  itkScalarImageKmeansImageFilter3DTest ${ITK_SOURCE_DIR}/Examples/Data/KmeansTest_T1UCharRaw.nii.gz
  ${ITK_SOURCE_DIR}/Examples/Data/KmeansTest_T1RawSkullStrip.nii.gz ${ITK_TEST_OUTPUT_DIR}/KmeansTest_T1KmeansPrelimSegmentation.nii.gz )
add_test(itkKullbackLeiblerCompareHistogramImageToImageMetricTest ${ALGORITHMS_TESTS2} itkKullbackLeiblerCompareHistogramImageToImageMetricTest)
add_test(itkLaplacianSegmentationLevelSetImageFilterTest ${ALGORITHMS_TESTS} itkLaplacianSegmentationLevelSetImageFilterTest)
add_test(itkLevelSetNeighborhoodExtractorTest ${ALGORITHMS_TESTS} itkLevelSetNeighborhoodExtractorTest)
add_test(itkLevelSetVelocityNeighborhoodExtractorTest ${ALGORITHMS_TESTS} itkLevelSetVelocityNeighborhoodExtractorTest)

add_test(itkMattesMutualInformationImageToImageMetricTest
  ${ALGORITHMS_TESTS3} itkMattesMutualInformationImageToImageMetricTest 1 1)

add_test(itkMattesMutualInformationImageToImageMetricTest2
  ${ALGORITHMS_TESTS3} itkMattesMutualInformationImageToImageMetricTest 1 0)

add_test(itkMattesMutualInformationImageToImageMetricTest3
  ${ALGORITHMS_TESTS3} itkMattesMutualInformationImageToImageMetricTest 0 1)

add_test(itkMattesMutualInformationImageToImageMetricTest4
  ${ALGORITHMS_TESTS3} itkMattesMutualInformationImageToImageMetricTest 0 0)

add_test(itkMeanSquaresImageMetricTest ${ALGORITHMS_TESTS3} itkMeanSquaresImageMetricTest)
add_test(itkMeanSquaresHistogramImageToImageMetricTest ${ALGORITHMS_TESTS3} itkMeanSquaresHistogramImageToImageMetricTest)
add_test(itkMinMaxCurvatureFlowImageFilterTest ${ALGORITHMS_TESTS3} itkMinMaxCurvatureFlowImageFilterTest)
add_test(itkMRFImageFilterTest ${ALGORITHMS_TESTS3} itkMRFImageFilterTest)
add_test(itkMRIBiasFieldCorrectionFilterTest ${ALGORITHMS_TESTS3} itkMRIBiasFieldCorrectionFilterTest)
add_test(itkMultiResolutionPyramidImageFilterWithResampleFilterTest ${ALGORITHMS_TESTS3} itkMultiResolutionPyramidImageFilterTest Resample)
add_test(itkMultiResolutionPyramidImageFilterWithShrinkFilterTest ${ALGORITHMS_TESTS3} itkMultiResolutionPyramidImageFilterTest Shrink)

add_test(itkRecursiveMultiResolutionPyramidImageFilterWithResampleFilterTest ${ALGORITHMS_TESTS3} itkRecursiveMultiResolutionPyramidImageFilterTest Resample)
add_test(itkRecursiveMultiResolutionPyramidImageFilterWithShrinkFilterTest ${ALGORITHMS_TESTS3} itkRecursiveMultiResolutionPyramidImageFilterTest Shrink)
add_test(itkRecursiveMultiResolutionPyramidImageFilterWithResampleFilterTest2 ${ALGORITHMS_TESTS3} itkMultiResolutionPyramidImageFilterTest Resample TestRecursive)
add_test(itkRecursiveMultiResolutionPyramidImageFilterWithShrinkFilterTest2 ${ALGORITHMS_TESTS3} itkMultiResolutionPyramidImageFilterTest Shrink TestRecursive)

add_test(itkMutualInformationHistogramImageToImageMetricTest ${ALGORITHMS_TESTS3} itkMutualInformationHistogramImageToImageMetricTest)
add_test(itkMultiResolutionPDEDeformableRegistrationTest ${ALGORITHMS_TESTS3}
  --compare ${BASELINE}/itkMultiResolutionPDEDeformableRegistrationTestPixelCentered.png
            ${TEMP}/itkMultiResolutionPDEDeformableRegistrationTestPixelCentered.png
  --compareNumberOfPixelsTolerance 20
  itkMultiResolutionPDEDeformableRegistrationTest
            ${TEMP}/itkMultiResolutionPDEDeformableRegistrationTestPixelCentered.png)
add_test(itkMultiResolutionImageRegistrationMethodTest ${ALGORITHMS_TESTS3}
itkMultiResolutionImageRegistrationMethodTest  )
add_test(itkMultiResolutionImageRegistrationMethodTest_1 ${ALGORITHMS_TESTS3}
itkMultiResolutionImageRegistrationMethodTest_1  )
add_test(itkMultiResolutionImageRegistrationMethodTest_2 ${ALGORITHMS_TESTS3}
itkMultiResolutionImageRegistrationMethodTest_2  )
add_test(itkMutualInformationMetricTest ${ALGORITHMS_TESTS3} itkMutualInformationMetricTest)
add_test(itkNarrowBandCurvesLevelSetImageFilterTest1 ${ALGORITHMS_TESTS4}
  --compare ${BASELINE}/itkNarrowBandCurvesLevelSetImageFilterTest.png
            ${TEMP}/itkNarrowBandCurvesLevelSetImageFilterTest.png
  itkNarrowBandCurvesLevelSetImageFilterTest
            ${TEMP}/itkNarrowBandCurvesLevelSetImageFilterTest.png)
add_test(itkNarrowBandThresholdSegmentationLevelSetImageFilterTest ${ALGORITHMS_TESTS2} itkNarrowBandThresholdSegmentationLevelSetImageFilterTest)
add_test(itkNewTest ${ALGORITHMS_TESTS3} itkNewTest)
add_test(itkNormalizedCorrelationImageMetricTest ${ALGORITHMS_TESTS3} itkNormalizedCorrelationImageMetricTest)
add_test(itkNormalizedCorrelationPointSetToImageMetricTest ${ALGORITHMS_TESTS3} itkNormalizedCorrelationPointSetToImageMetricTest)
add_test(itkMeanSquaresPointSetToImageMetricTest ${ALGORITHMS_TESTS3} itkMeanSquaresPointSetToImageMetricTest)
add_test(itkMeanReciprocalSquareDifferencePointSetToImageMetricTest ${ALGORITHMS_TESTS4} itkMeanReciprocalSquareDifferencePointSetToImageMetricTest)
add_test(itkNormalizedMutualInformationHistogramImageToImageMetricTest ${ALGORITHMS_TESTS3} itkNormalizedMutualInformationHistogramImageToImageMetricTest)
add_test(itkOtsuThresholdImageCalculatorTest ${ALGORITHMS_TESTS3} itkOtsuThresholdImageCalculatorTest)
add_test(itkOtsuMultipleThresholdsCalculatorTest ${ALGORITHMS_TESTS2} itkOtsuMultipleThresholdsCalculatorTest)
add_test(itkOrthogonalSwath2DPathFilterTest ${ALGORITHMS_TESTS3} itkOrthogonalSwath2DPathFilterTest)
add_test(itkMeanReciprocalSquareDifferenceImageMetricTest ${ALGORITHMS_TESTS3}  itkMeanReciprocalSquareDifferenceImageMetricTest)
add_test(itkPointSetToImageRegistrationTest_1 ${ALGORITHMS_TESTS3}  itkPointSetToImageRegistrationTest_1)
add_test(itkRegionGrow2DTest ${ALGORITHMS_TESTS3} itkRegionGrow2DTest)
add_test(itkReinitializeLevelSetImageFilterTest ${ALGORITHMS_TESTS3} itkReinitializeLevelSetImageFilterTest)
add_test(itkRegularSphereMeshSourceTest ${ALGORITHMS_TESTS2} itkRegularSphereMeshSourceTest)
add_test(itkRayCastInterpolateImageFunctionTest ${ALGORITHMS_TESTS2} itkRayCastInterpolateImageFunctionTest)
add_test(itkSimplexMeshVolumeCalculatorTest ${ALGORITHMS_TESTS2} itkSimplexMeshVolumeCalculatorTest)
add_test(itkShapeDetectionLevelSetImageFilterTest ${ALGORITHMS_TESTS3} itkShapeDetectionLevelSetImageFilterTest)
add_test(itkShapePriorMAPCostFunctionTest ${ALGORITHMS_TESTS3} itkShapePriorMAPCostFunctionTest)
add_test(itkShapePriorSegmentationLevelSetFunctionTest ${ALGORITHMS_TESTS3} itkShapePriorSegmentationLevelSetFunctionTest)
add_test(itkSpatialObjectToImageRegistrationTest ${ALGORITHMS_TESTS3} itkSpatialObjectToImageRegistrationTest)
add_test(itkSupervisedImageClassifierTest ${ALGORITHMS_TESTS3} itkSupervisedImageClassifierTest)
add_test( itkThresholdMaximumConnectedComponentsImageFilterTest1
          ${ALGORITHMS_TESTS4}
  --compare ${BASELINE}/itkThresholdMaximumConnectedComponentsImageFilterTest1.png
            ${TEMP}/itkThresholdMaximumConnectedComponentsImageFilterTest1.png
          itkThresholdMaximumConnectedComponentsImageFilterTest
          ${INPUTDATA}/CellsFluorescence1.png
          ${TEMP}/itkThresholdMaximumConnectedComponentsImageFilterTest1.png 10)

add_test( itkThresholdMaximumConnectedComponentsImageFilterTest2
          ${ALGORITHMS_TESTS4}
  --compare ${BASELINE}/itkThresholdMaximumConnectedComponentsImageFilterTest2.png
            ${TEMP}/itkThresholdMaximumConnectedComponentsImageFilterTest2.png
          itkThresholdMaximumConnectedComponentsImageFilterTest
          ${INPUTDATA}/CellsFluorescence2.png
          ${TEMP}/itkThresholdMaximumConnectedComponentsImageFilterTest2.png 10)

add_test(itkThresholdSegmentationLevelSetImageFilterTest ${ALGORITHMS_TESTS2} itkThresholdSegmentationLevelSetImageFilterTest)
add_test(itkGibbsTest ${ALGORITHMS_TESTS3} itkGibbsTest)
add_test(itkDeformableTest ${ALGORITHMS_TESTS3} itkDeformableTest)
add_test(itkDeformableSimplexMesh3DBalloonForceFilterTest ${ALGORITHMS_TESTS2} itkDeformableSimplexMesh3DBalloonForceFilterTest)
add_test(itkDeformableSimplexMesh3DGradientConstaintForceFilterTest ${ALGORITHMS_TESTS2} itkDeformableSimplexMesh3DGradientConstraintForceFilterTest)
add_test(itkDeformableSimplexMesh3DFilterTest ${ALGORITHMS_TESTS2} itkDeformableSimplexMesh3DFilterTest)
add_test(itk2DDeformableTest ${ALGORITHMS_TESTS3} itk2DDeformableTest)
add_test(itkBinaryThinningImageFilterTest ${ALGORITHMS_TESTS3}
   --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/BinaryThinningImageFilterTest.png
             ${ITK_TEST_OUTPUT_DIR}/BinaryThinningImageFilterTest.png
  itkBinaryThinningImageFilterTest
             ${ITK_DATA_ROOT}/Input/Shapes.png
             ${ITK_TEST_OUTPUT_DIR}/BinaryThinningImageFilterTest.png
             )
add_test(itkSphereMeshSourceTest ${ALGORITHMS_TESTS3} itkSphereMeshSourceTest)
add_test(itkUnsharpMaskLevelSetImageFilterTest ${ALGORITHMS_TESTS3} itkUnsharpMaskLevelSetImageFilterTest)
add_test(itkVnlFFTTest ${ALGORITHMS_TESTS4} itkVnlFFTTest)
if(USE_FFTWF)
       add_test(itkFFTWF_FFTTest ${ALGORITHMS_TESTS4} itkFFTWF_FFTTest)
       add_test(itkVnlFFTWF_FFTTest ${ALGORITHMS_TESTS4} itkVnlFFTWF_FFTTest)
endif(USE_FFTWF)
if(USE_FFTWD)
       add_test(itkFFTWD_FFTTest ${ALGORITHMS_TESTS4} itkFFTWD_FFTTest)
       add_test(itkVnlFFTWD_FFTTest ${ALGORITHMS_TESTS4} itkVnlFFTWD_FFTTest)
endif(USE_FFTWD)
if(USE_FFTWD)
       add_test(itkCurvatureRegistrationFilterTest ${ALGORITHMS_TESTS4} itkCurvatureRegistrationFilterTest)
endif(USE_FFTWD)

add_test(itkMIRegistrationFunctionTest ${ALGORITHMS_TESTS2} itkMIRegistrationFunctionTest)
add_test(itkSymmetricForcesDemonsRegistrationFilterTest ${ALGORITHMS_TESTS2} itkSymmetricForcesDemonsRegistrationFilterTest)
add_test(itkVoronoiDiagram2DTest ${ALGORITHMS_TESTS2} itkVoronoiDiagram2DTest)
add_test(itkVoronoiSegmentationImageFilterTest ${ALGORITHMS_TESTS2} itkVoronoiSegmentationImageFilterTest)
add_test(itkVoronoiSegmentationRGBImageFilterTest ${ALGORITHMS_TESTS4} itkVoronoiSegmentationRGBImageFilterTest)
add_test(itkWatershedImageFilterTest ${ALGORITHMS_TESTS4} itkWatershedImageFilterTest)
add_test(itkPointSetToPointSetRegistrationTest ${ALGORITHMS_TESTS2}  itkPointSetToPointSetRegistrationTest)
add_test(itkPointSetToSpatialObjectDemonsRegistrationTest ${ALGORITHMS_TESTS2}  itkPointSetToSpatialObjectDemonsRegistrationTest)

add_test(itkCurvatureFlowTest ${ALGORITHMS_TESTS}
    itkCurvatureFlowTest ${ITK_TEST_OUTPUT_DIR}/itkCurvatureFlowTest.vtk)

# Metric fails when centered pixel coordinates not in use
add_test(itkBinaryMedialNodeMetricTest ${ALGORITHMS_TESTS4} itkBinaryMedialNodeMetricTest)

set(AlgorithmsTest_SRCS
itkAnisotropicFourthOrderLevelSetImageFilterTest.cxx
itkAntiAliasBinaryImageFilterTest.cxx
itkAutomaticTopologyMeshSourceTest.cxx
itkBinaryMask3DMeshSourceTest.cxx
itkBinaryMinMaxCurvatureFlowImageFilterTest.cxx
itkBioCellTest.cxx
itkBioCellularAggregateTest.cxx
itkBioGeneNetworkTest.cxx
itkBioGeneTest.cxx
itkBioGenomeTest.cxx
itkCannySegmentationLevelSetImageFilterTest.cxx
itkCorrelationCoefficientHistogramImageToImageMetricTest.cxx
itkCurvatureFlowTest.cxx
itkCurvesLevelSetImageFilterTest.cxx
itkDemonsRegistrationFilterTest.cxx
itkExtensionVelocitiesImageFilterTest.cxx
itkExtractMeshConnectedRegionsTest.cxx
itkFEMRegistrationFilterTest.cxx
itkFastChamferDistanceImageFilterTest.cxx
itkFastMarchingExtensionImageFilterTest.cxx
itkFastMarchingTest.cxx
itkGeodesicActiveContourLevelSetImageFilterTest.cxx
itkGeodesicActiveContourShapePriorLevelSetImageFilterTest.cxx
itkGeodesicActiveContourShapePriorLevelSetImageFilterTest_2.cxx
itkGradientVectorFlowImageFilterTest.cxx
itkHistogramImageToImageMetricTest.cxx
itkHistogramMatchingImageFilterTest.cxx
itkImageMomentsTest.cxx
itkImageRegistrationMethodTest.cxx
itkImageRegistrationMethodTest_1.cxx
itkImageRegistrationMethodTest_10.cxx
itkImageRegistrationMethodTest_11.cxx
itkImageRegistrationMethodTest_12.cxx
itkImageRegistrationMethodTest_13.cxx
itkImageRegistrationMethodTest_14.cxx
itkImageRegistrationMethodTest_15.cxx
itkImageRegistrationMethodTest_16.cxx
itkImageRegistrationMethodTest_2.cxx
itkImageRegistrationMethodTest_3.cxx
itkImageRegistrationMethodTest_4.cxx
itkImageRegistrationMethodTest_5.cxx
itkImageRegistrationMethodTest_6.cxx
itkImageRegistrationMethodTest_7.cxx
itkImageRegistrationMethodTest_8.cxx
itkImageRegistrationMethodTest_9.cxx
itkInterpolateTest.cxx
itkIsoContourDistanceImageFilterTest.cxx
itkIsotropicFourthOrderLevelSetImageFilterTest.cxx
itkKalmanLinearEstimatorTest.cxx
itkKmeansModelEstimatorTest.cxx
itkLaplacianSegmentationLevelSetImageFilterTest.cxx
itkLevelSetNeighborhoodExtractorTest.cxx
itkLevelSetVelocityNeighborhoodExtractorTest.cxx
)

set(AlgorithmsTest2_SRCS
itkCompareHistogramImageToImageMetricTest.cxx
itkDeformableSimplexMesh3DBalloonForceFilterTest.cxx
itkDeformableSimplexMesh3DGradientConstraintForceFilterTest.cxx
itkDeformableSimplexMesh3DFilterTest.cxx
itkGradientDifferenceImageToImageMetricTest.cxx
itkImagePCADecompositionCalculatorTest.cxx
itkImagePCAShapeModelEstimatorTest.cxx
itkIsolatedWatershedImageFilterTest.cxx
itkKappaStatisticImageToImageMetricTest.cxx
itkKullbackLeiblerCompareHistogramImageToImageMetricTest.cxx
itkMIRegistrationFunctionTest.cxx
itkMatchCardinalityImageToImageMetricTest.cxx
itkNarrowBandThresholdSegmentationLevelSetImageFilterTest.cxx
itkOtsuMultipleThresholdsCalculatorTest.cxx
itkOtsuMultipleThresholdsImageFilterTest.cxx
itkPointSetToPointSetRegistrationTest.cxx
itkPointSetToSpatialObjectDemonsRegistrationTest.cxx
itkRayCastInterpolateImageFunctionTest.cxx
itkRegularSphereMeshSourceTest.cxx
itkSTAPLEImageFilterTest.cxx
itkSimplexMeshVolumeCalculatorTest.cxx
itkSymmetricForcesDemonsRegistrationFilterTest.cxx
itkThresholdSegmentationLevelSetImageFilterTest.cxx
itkVoronoiDiagram2DTest.cxx
itkVoronoiSegmentationImageFilterTest.cxx
)

set(AlgorithmsTest3_SRCS
itk2DDeformableTest.cxx
itkBinaryThinningImageFilterTest.cxx
itkDeformableTest.cxx
itkGibbsTest.cxx
itkMRFImageFilterTest.cxx
itkMRIBiasFieldCorrectionFilterTest.cxx
itkMattesMutualInformationImageToImageMetricTest.cxx
itkMeanReciprocalSquareDifferenceImageMetricTest.cxx
itkMeanSquaresHistogramImageToImageMetricTest.cxx
itkMeanSquaresImageMetricTest.cxx
itkMeanSquaresPointSetToImageMetricTest.cxx
itkMinMaxCurvatureFlowImageFilterTest.cxx
itkMultiResolutionImageRegistrationMethodTest.cxx
itkMultiResolutionImageRegistrationMethodTest_1.cxx
itkMultiResolutionImageRegistrationMethodTest_2.cxx
itkMultiResolutionPDEDeformableRegistrationTest.cxx
itkMultiResolutionPyramidImageFilterTest.cxx
itkMutualInformationHistogramImageToImageMetricTest.cxx
itkMutualInformationMetricTest.cxx
itkNewTest.cxx
itkNormalizedCorrelationImageMetricTest.cxx
itkNormalizedCorrelationPointSetToImageMetricTest.cxx
itkNormalizedMutualInformationHistogramImageToImageMetricTest.cxx
itkOrthogonalSwath2DPathFilterTest.cxx
itkOtsuThresholdImageCalculatorTest.cxx
itkOtsuThresholdImageFilterTest.cxx
itkPointSetToImageRegistrationTest_1.cxx
itkRecursiveMultiResolutionPyramidImageFilterTest.cxx
itkRegionGrow2DTest.cxx
itkReinitializeLevelSetImageFilterTest.cxx
itkShapeDetectionLevelSetImageFilterTest.cxx
itkShapePriorMAPCostFunctionTest.cxx
itkShapePriorSegmentationLevelSetFunctionTest.cxx
itkSpatialObjectToImageRegistrationTest.cxx
itkSphereMeshSourceTest.cxx
itkSupervisedImageClassifierTest.cxx
itkUnsharpMaskLevelSetImageFilterTest.cxx
)

if(USE_FFTWD)
  set(CURVATUREREGISTRATION_SRCS itkCurvatureRegistrationFilterTest.cxx)
endif(USE_FFTWD)
set(AlgorithmsTest4_SRCS
itkBayesianClassifierImageFilterTest.cxx
itkScalarImageKmeansImageFilter3DTest.cxx
itkFastMarchingUpwindGradientTest.cxx
itkCollidingFrontsImageFilterTest.cxx
itkCurvesLevelSetImageFilterZeroSigmaTest.cxx
itkGeodesicActiveContourLevelSetImageFilterZeroSigmaTest.cxx
itkLabelVotingImageFilterTest.cxx
itkLevelSetMotionRegistrationFilterTest
itkNarrowBandCurvesLevelSetImageFilterTest.cxx
itkFFTTest.cxx
${CURVATUREREGISTRATION_SRCS}
itkWatershedImageFilterTest.cxx
itkVoronoiPartitioningImageFilterTest.cxx
itkVectorThresholdSegmentationLevelSetImageFilterTest.cxx
itkMeanReciprocalSquareDifferencePointSetToImageMetricTest.cxx
itkThresholdMaximumConnectedComponentsImageFilterTest.cxx
itkBinaryMedialNodeMetricTest.cxx
itkVoronoiSegmentationRGBImageFilterTest.cxx
)

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
target_link_libraries(itkAlgorithmsPrintTest4 ITKAlgorithms ITKFEM
                                              ITKStatistics ITKNumerics)

if(ITK_OBJCXX_COMPILER_WORKS)
  set(ALGORITHMS_OBJCXX_TEST ${CXX_TEST_PATH}/itkAlgorithmsHeaderObjCxxTest)
  add_test(itkAlgorithmsHeaderObjCxxTest ${ALGORITHMS_OBJCXX_TEST})
  add_executable(itkAlgorithmsHeaderObjCxxTest itkAlgorithmsHeaderObjCxxTest.mm)
  target_link_libraries(itkAlgorithmsHeaderObjCxxTest ITKAlgorithms ITKFEM ITKNumerics)
endif(ITK_OBJCXX_COMPILER_WORKS)

add_executable(itkAlgorithmsHeaderTest itkAlgorithmsHeaderTest.cxx)
target_link_libraries(itkAlgorithmsHeaderTest ITKAlgorithms ITKFEM ITKNumerics)
add_test(itkAlgorithmsHeaderTest ${ALGORITHMS_HEADER_TESTS})

add_executable(itkAlgorithmsTests itkAlgorithmsTests.cxx
                                  ${AlgorithmsTest_SRCS})
target_link_libraries(itkAlgorithmsTests
                      ITKAlgorithms ITKFEM ITKStatistics ITKNumerics)

add_executable(itkAlgorithmsTests2 itkAlgorithmsTests2.cxx ${AlgorithmsTest2_SRCS})
target_link_libraries(itkAlgorithmsTests2 ITKAlgorithms ITKFEM ITKStatistics ITKNumerics)

add_executable(itkAlgorithmsTests3 itkAlgorithmsTests3.cxx ${AlgorithmsTest3_SRCS})
target_link_libraries(itkAlgorithmsTests3 ITKAlgorithms ITKFEM ITKStatistics ITKNumerics)

add_executable(itkAlgorithmsTests4 itkAlgorithmsTests4.cxx ${AlgorithmsTest4_SRCS})
target_link_libraries(itkAlgorithmsTests4 ITKAlgorithms ITKFEM ITKStatistics ITKNumerics)

if(ITK_DATA_ROOT)

add_test(itkMatchCardinalityImageToImageMetricTest ${ALGORITHMS_TESTS2}
  itkMatchCardinalityImageToImageMetricTest ${ITK_DATA_ROOT}/Input/Spots.png)

add_test(itkKappaStatisticImageToImageMetricTest ${ALGORITHMS_TESTS2}
  itkKappaStatisticImageToImageMetricTest ${ITK_DATA_ROOT}/Input/Spots.png)

add_test(itkIsolatedWatershedImageFilterTest ${ALGORITHMS_TESTS2}
   --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/IsolatedWatershedImageFilterTest.png
             ${ITK_TEST_OUTPUT_DIR}/IsolatedWatershedImageFilterTest.png
  itkIsolatedWatershedImageFilterTest
             ${ITK_DATA_ROOT}/Input/cthead1.png
             ${ITK_TEST_OUTPUT_DIR}/IsolatedWatershedImageFilterTest.png
             113 84 120 99)

add_test(itkOtsuThresholdImageFilterTest ${ALGORITHMS_TESTS3}
  --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/OtsuThresholdImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/OtsuThresholdImageFilterTest.png
  itkOtsuThresholdImageFilterTest
            ${ITK_DATA_ROOT}/Input/cthead1.png
            ${ITK_TEST_OUTPUT_DIR}/OtsuThresholdImageFilterTest.png
            64)

add_test(itkOtsuMultipleThresholdsImageFilterTest ${ALGORITHMS_TESTS2}
  --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/OtsuMultipleThresholdsImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/OtsuMultipleThresholdsImageFilterTest.png
  itkOtsuMultipleThresholdsImageFilterTest
            ${ITK_DATA_ROOT}/Input/peppers.png
            ${ITK_TEST_OUTPUT_DIR}/OtsuMultipleThresholdsImageFilterTest.png
            64 3 2)

add_test(itkVoronoiPartitioningImageFilterTest1 ${ALGORITHMS_TESTS4}
  --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/VoronoiPartioningImageFilterTest1.png
            ${ITK_TEST_OUTPUT_DIR}/VoronoiPartioningImageFilterTest1.png
  itkVoronoiPartitioningImageFilterTest
            ${ITK_DATA_ROOT}/Input/sf4.png
            ${ITK_TEST_OUTPUT_DIR}/VoronoiPartioningImageFilterTest1.png 1)

add_test(itkVoronoiPartitioningImageFilterTest2 ${ALGORITHMS_TESTS4}
  --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/VoronoiPartioningImageFilterTest2.png
            ${ITK_TEST_OUTPUT_DIR}/VoronoiPartioningImageFilterTest2.png
  itkVoronoiPartitioningImageFilterTest
            ${ITK_DATA_ROOT}/Input/sf4.png
            ${ITK_TEST_OUTPUT_DIR}/VoronoiPartioningImageFilterTest2.png 0)

add_test(itkVectorThresholdSegmentationLevelSetImageFilterTest ${ALGORITHMS_TESTS4}
  --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/VectorThresholdSegmentationLevelSetImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/VectorThresholdSegmentationLevelSetImageFilterTest.png
  itkVectorThresholdSegmentationLevelSetImageFilterTest
            ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSliceInitialLevelSet.png
            ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png
            ${ITK_TEST_OUTPUT_DIR}/VectorThresholdSegmentationLevelSetImageFilterTest.png
            5.0 50
            )

add_test(itkSTAPLEImageFilterTest ${ALGORITHMS_TESTS2}
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

endif( NOT ITK_DISABLE_CXX_TESTING )


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
