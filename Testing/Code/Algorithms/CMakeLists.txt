IF( NOT ITK_DISABLE_CXX_TESTING )
IF(ITK_BINARY_DIR)
  IF(TCL_TCLSH)
    ADD_TEST(PrintSelf-Algorithms ${TCL_TCLSH}
             ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl  
             ${ITK_SOURCE_DIR}/Code/Algorithms)
  ENDIF(TCL_TCLSH)
ENDIF(ITK_BINARY_DIR)


SET(ALGORITHMS_TESTS ${CXX_TEST_PATH}/itkAlgorithmsTests)
SET(ALGORITHMS_TESTS2 ${CXX_TEST_PATH}/itkAlgorithmsTests2)
SET(ALGORITHMS_TESTS3 ${CXX_TEST_PATH}/itkAlgorithmsTests3)
SET(ALGORITHMS_TESTS4 ${CXX_TEST_PATH}/itkAlgorithmsTests4)

SET(ALGORITHMS_PRINT_TEST ${CXX_TEST_PATH}/itkAlgorithmsPrintTest)
SET(ALGORITHMS_PRINT_TEST2 ${CXX_TEST_PATH}/itkAlgorithmsPrintTest2)
SET(ALGORITHMS_PRINT_TEST3 ${CXX_TEST_PATH}/itkAlgorithmsPrintTest3)
SET(ALGORITHMS_PRINT_TEST4 ${CXX_TEST_PATH}/itkAlgorithmsPrintTest4)

SET(ALGORITHMS_HEADER_TESTS ${CXX_TEST_PATH}/itkAlgorithmsHeaderTest)

ADD_TEST(itkAlgorithmsPrintTest ${ALGORITHMS_PRINT_TEST})
ADD_TEST(itkAlgorithmsPrintTest2 ${ALGORITHMS_PRINT_TEST2})
ADD_TEST(itkAlgorithmsPrintTest3 ${ALGORITHMS_PRINT_TEST3})
ADD_TEST(itkAlgorithmsPrintTest4 ${ALGORITHMS_PRINT_TEST4})

ADD_TEST(itkAnisotropicFourthOrderLevelSetImageFilterTest ${ALGORITHMS_TESTS} itkAnisotropicFourthOrderLevelSetImageFilterTest)
ADD_TEST(itkAntiAliasBinaryImageFilterTest ${ALGORITHMS_TESTS} itkAntiAliasBinaryImageFilterTest)
ADD_TEST(itkAutomaticTopologyMeshSourceTest ${ALGORITHMS_TESTS} itkAutomaticTopologyMeshSourceTest)
ADD_TEST(itkBinaryMinMaxCurvatureFlowImageFilterTest ${ALGORITHMS_TESTS} itkBinaryMinMaxCurvatureFlowImageFilterTest )

ADD_TEST(itkBayesianClassifierImageFilterTest ${ALGORITHMS_TESTS4} itkBayesianClassifierImageFilterTest 
    ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png
    ${ITK_TEST_OUTPUT_DIR}/itkBayesianClassifierImageFilterTestOutput.png 
    4 )

ADD_TEST(itkBinaryMask3DMeshSourceTest ${ALGORITHMS_TESTS} itkBinaryMask3DMeshSourceTest )
ADD_TEST(itkBioCellTest ${ALGORITHMS_TESTS} itkBioCellTest )
ADD_TEST(itkBioCellularAggregateTest ${ALGORITHMS_TESTS} itkBioCellularAggregateTest )
ADD_TEST(itkBioGeneTest ${ALGORITHMS_TESTS} itkBioGeneTest )
ADD_TEST(itkBioGenomeTest ${ALGORITHMS_TESTS} itkBioGenomeTest )
ADD_TEST(itkBioGeneNetworkTest ${ALGORITHMS_TESTS} itkBioGeneNetworkTest )
ADD_TEST(itkCannySegmentationLevelSetImageFilterTest ${ALGORITHMS_TESTS} itkCannySegmentationLevelSetImageFilterTest)
ADD_TEST(itkCorrelationCoefficientHistogramImageToImageMetricTest ${ALGORITHMS_TESTS} itkCorrelationCoefficientHistogramImageToImageMetricTest)
ADD_TEST(itkCompareHistogramImageToImageMetricTest ${ALGORITHMS_TESTS2} itkCompareHistogramImageToImageMetricTest)
ADD_TEST(itkCurvesLevelSetImageFilterTest ${ALGORITHMS_TESTS} itkCurvesLevelSetImageFilterTest)
ADD_TEST(itkDemonsRegistrationFilterTest ${ALGORITHMS_TESTS} itkDemonsRegistrationFilterTest)
ADD_TEST(itkExtensionVelocitiesImageFilterTest ${ALGORITHMS_TESTS} itkExtensionVelocitiesImageFilterTest)
ADD_TEST(itkExtractMeshConnectedRegionsTest ${ALGORITHMS_TESTS} itkExtractMeshConnectedRegionsTest)
ADD_TEST(itkFastChamferDistanceImageFilterTest ${ALGORITHMS_TESTS} itkFastChamferDistanceImageFilterTest)
ADD_TEST(itkFastMarchingTest ${ALGORITHMS_TESTS} itkFastMarchingTest)
ADD_TEST(itkFastMarchingExtensionImageFilterTest ${ALGORITHMS_TESTS} itkFastMarchingExtensionImageFilterTest)
ADD_TEST(itkFastMarchingUpwindGradientTest ${ALGORITHMS_TESTS4} itkFastMarchingUpwindGradientTest)
ADD_TEST(itkCollidingFrontsImageFilterTest ${ALGORITHMS_TESTS4} itkCollidingFrontsImageFilterTest)
ADD_TEST(itkCurvesLevelSetImageFilterZeroSigmaTest ${ALGORITHMS_TESTS4} itkCurvesLevelSetImageFilterZeroSigmaTest)
ADD_TEST(itkGeodesicActiveContourLevelSetImageFilterZeroSigmaTest ${ALGORITHMS_TESTS4} itkGeodesicActiveContourLevelSetImageFilterZeroSigmaTest)
ADD_TEST(itkFEMRegistrationFilterTest ${ALGORITHMS_TESTS} itkFEMRegistrationFilterTest)
ADD_TEST(itkGeodesicActiveContourLevelSetImageFilterTest ${ALGORITHMS_TESTS} itkGeodesicActiveContourLevelSetImageFilterTest)
ADD_TEST(itkGeodesicActiveContourShapePriorLevelSetImageFilterTest ${ALGORITHMS_TESTS} itkGeodesicActiveContourShapePriorLevelSetImageFilterTest)
ADD_TEST(itkGeodesicActiveContourShapePriorLevelSetImageFilterTest_2 ${ALGORITHMS_TESTS} itkGeodesicActiveContourShapePriorLevelSetImageFilterTest_2)
ADD_TEST(itkGradientVectorFlowImageFilterTest ${ALGORITHMS_TESTS} itkGradientVectorFlowImageFilterTest)
ADD_TEST(itkGradientDifferenceImageToImageMetricTest ${ALGORITHMS_TESTS2} itkGradientDifferenceImageToImageMetricTest)
ADD_TEST(itkHistogramMatchingImageFilterTest ${ALGORITHMS_TESTS} itkHistogramMatchingImageFilterTest)
ADD_TEST(itkHistogramImageToImageMetricTest ${ALGORITHMS_TESTS} itkHistogramImageToImageMetricTest)
ADD_TEST(itkImageMomentsTest ${ALGORITHMS_TESTS} itkImageMomentsTest)
ADD_TEST(itkImagePCAShapeModelEstimatorTest ${ALGORITHMS_TESTS2} itkImagePCAShapeModelEstimatorTest)
ADD_TEST(itkImagePCADecompositionCalculatorTest ${ALGORITHMS_TESTS2} itkImagePCADecompositionCalculatorTest)
ADD_TEST(itkImageRegistrationMethodTest ${ALGORITHMS_TESTS}   itkImageRegistrationMethodTest  )
ADD_TEST(itkImageRegistrationMethodTest_1 ${ALGORITHMS_TESTS} itkImageRegistrationMethodTest_1)
ADD_TEST(itkImageRegistrationMethodTest_2 ${ALGORITHMS_TESTS} itkImageRegistrationMethodTest_2)
ADD_TEST(itkImageRegistrationMethodTest_3 ${ALGORITHMS_TESTS} itkImageRegistrationMethodTest_3)
ADD_TEST(itkImageRegistrationMethodTest_4 ${ALGORITHMS_TESTS} itkImageRegistrationMethodTest_4)
ADD_TEST(itkImageRegistrationMethodTest_5 ${ALGORITHMS_TESTS} itkImageRegistrationMethodTest_5)
ADD_TEST(itkImageRegistrationMethodTest_6 ${ALGORITHMS_TESTS} itkImageRegistrationMethodTest_6)
ADD_TEST(itkImageRegistrationMethodTest_7 ${ALGORITHMS_TESTS} itkImageRegistrationMethodTest_7)
ADD_TEST(itkImageRegistrationMethodTest_8 ${ALGORITHMS_TESTS} itkImageRegistrationMethodTest_8)
ADD_TEST(itkImageRegistrationMethodTest_9 ${ALGORITHMS_TESTS} itkImageRegistrationMethodTest_9)
ADD_TEST(itkImageRegistrationMethodTest_10 ${ALGORITHMS_TESTS} itkImageRegistrationMethodTest_10)
ADD_TEST(itkImageRegistrationMethodTest_11 ${ALGORITHMS_TESTS} itkImageRegistrationMethodTest_11)
ADD_TEST(itkImageRegistrationMethodTest_12 ${ALGORITHMS_TESTS} itkImageRegistrationMethodTest_12)
ADD_TEST(itkImageRegistrationMethodTest_13 ${ALGORITHMS_TESTS} itkImageRegistrationMethodTest_13)
ADD_TEST(itkImageRegistrationMethodTest_14 ${ALGORITHMS_TESTS} itkImageRegistrationMethodTest_14)
ADD_TEST(itkImageRegistrationMethodTest_15 ${ALGORITHMS_TESTS} itkImageRegistrationMethodTest_15)
ADD_TEST(itkImageRegistrationMethodTest_16 ${ALGORITHMS_TESTS} itkImageRegistrationMethodTest_16)
ADD_TEST(itkInterpolateTest ${ALGORITHMS_TESTS} itkInterpolateTest)
ADD_TEST(itkIsoContourDistanceImageFilterTest ${ALGORITHMS_TESTS} itkIsoContourDistanceImageFilterTest)
ADD_TEST(itkIsotropicFourthOrderLevelSetImageFilterTest ${ALGORITHMS_TESTS} itkIsotropicFourthOrderLevelSetImageFilterTest)
ADD_TEST(itkKalmanLinearEstimatorTest ${ALGORITHMS_TESTS} itkKalmanLinearEstimatorTest)
ADD_TEST(itkKmeansModelEstimatorTest ${ALGORITHMS_TESTS} itkKmeansModelEstimatorTest)
ADD_TEST(itkKullbackLeiblerCompareHistogramImageToImageMetricTest ${ALGORITHMS_TESTS2} itkKullbackLeiblerCompareHistogramImageToImageMetricTest)
ADD_TEST(itkLaplacianSegmentationLevelSetImageFilterTest ${ALGORITHMS_TESTS} itkLaplacianSegmentationLevelSetImageFilterTest)
ADD_TEST(itkLevelSetNeighborhoodExtractorTest ${ALGORITHMS_TESTS} itkLevelSetNeighborhoodExtractorTest)
ADD_TEST(itkLevelSetVelocityNeighborhoodExtractorTest ${ALGORITHMS_TESTS} itkLevelSetVelocityNeighborhoodExtractorTest)
ADD_TEST(itkMattesMutualInformationImageToImageMetricTest ${ALGORITHMS_TESTS3} itkMattesMutualInformationImageToImageMetricTest)
ADD_TEST(itkMeanSquaresImageMetricTest ${ALGORITHMS_TESTS3} itkMeanSquaresImageMetricTest)
ADD_TEST(itkMeanSquaresHistogramImageToImageMetricTest ${ALGORITHMS_TESTS3} itkMeanSquaresHistogramImageToImageMetricTest)
ADD_TEST(itkMinMaxCurvatureFlowImageFilterTest ${ALGORITHMS_TESTS3} itkMinMaxCurvatureFlowImageFilterTest)
ADD_TEST(itkMRFImageFilterTest ${ALGORITHMS_TESTS3} itkMRFImageFilterTest)
ADD_TEST(itkMRIBiasFieldCorrectionFilterTest ${ALGORITHMS_TESTS3} itkMRIBiasFieldCorrectionFilterTest)
ADD_TEST(itkMultiResolutionPyramidImageFilterTest ${ALGORITHMS_TESTS3} itkMultiResolutionPyramidImageFilterTest)
ADD_TEST(itkMutualInformationHistogramImageToImageMetricTest ${ALGORITHMS_TESTS3} itkMutualInformationHistogramImageToImageMetricTest)
ADD_TEST(itkRecursiveMultiResolutionPyramidImageFilterTest ${ALGORITHMS_TESTS3} itkRecursiveMultiResolutionPyramidImageFilterTest)
ADD_TEST(itkMultiResolutionPDEDeformableRegistrationTest ${ALGORITHMS_TESTS3} itkMultiResolutionPDEDeformableRegistrationTest)
ADD_TEST(itkMultiResolutionImageRegistrationMethodTest ${ALGORITHMS_TESTS3}   
itkMultiResolutionImageRegistrationMethodTest  )
ADD_TEST(itkMultiResolutionImageRegistrationMethodTest_1 ${ALGORITHMS_TESTS3}   
itkMultiResolutionImageRegistrationMethodTest_1  )
ADD_TEST(itkMultiResolutionImageRegistrationMethodTest_2 ${ALGORITHMS_TESTS3}   
itkMultiResolutionImageRegistrationMethodTest_2  )
ADD_TEST(itkMutualInformationMetricTest ${ALGORITHMS_TESTS3} itkMutualInformationMetricTest)
ADD_TEST(itkNarrowBandCurvesLevelSetImageFilterTest ${ALGORITHMS_TESTS4} itkNarrowBandCurvesLevelSetImageFilterTest)
ADD_TEST(itkNarrowBandThresholdSegmentationLevelSetImageFilterTest ${ALGORITHMS_TESTS2} itkNarrowBandThresholdSegmentationLevelSetImageFilterTest)
ADD_TEST(itkNewTest ${ALGORITHMS_TESTS3} itkNewTest)
ADD_TEST(itkNormalizedCorrelationImageMetricTest ${ALGORITHMS_TESTS3} itkNormalizedCorrelationImageMetricTest)
ADD_TEST(itkNormalizedCorrelationPointSetToImageMetricTest ${ALGORITHMS_TESTS3} itkNormalizedCorrelationPointSetToImageMetricTest)
ADD_TEST(itkMeanSquaresPointSetToImageMetricTest ${ALGORITHMS_TESTS3} itkMeanSquaresPointSetToImageMetricTest)
ADD_TEST(itkMeanReciprocalSquareDifferencePointSetToImageMetricTest ${ALGORITHMS_TESTS4} itkMeanReciprocalSquareDifferencePointSetToImageMetricTest)
ADD_TEST(itkNormalizedMutualInformationHistogramImageToImageMetricTest ${ALGORITHMS_TESTS3} itkNormalizedMutualInformationHistogramImageToImageMetricTest)
ADD_TEST(itkOtsuThresholdImageCalculatorTest ${ALGORITHMS_TESTS3} itkOtsuThresholdImageCalculatorTest)
ADD_TEST(itkOtsuMultipleThresholdsCalculatorTest ${ALGORITHMS_TESTS2} itkOtsuMultipleThresholdsCalculatorTest)
ADD_TEST(itkOrthogonalSwath2DPathFilterTest ${ALGORITHMS_TESTS3} itkOrthogonalSwath2DPathFilterTest)
ADD_TEST(itkMeanReciprocalSquareDifferenceImageMetricTest ${ALGORITHMS_TESTS3}  itkMeanReciprocalSquareDifferenceImageMetricTest)
ADD_TEST(itkPointSetToImageRegistrationTest_1 ${ALGORITHMS_TESTS3}  itkPointSetToImageRegistrationTest_1)
ADD_TEST(itkRegionGrow2DTest ${ALGORITHMS_TESTS3} itkRegionGrow2DTest)
ADD_TEST(itkReinitializeLevelSetImageFilterTest ${ALGORITHMS_TESTS3} itkReinitializeLevelSetImageFilterTest)
ADD_TEST(itkScalarImageKmeansImageFilterTest ${ALGORITHMS_TESTS3} 
   --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/itkScalarImageKmeansImageFilterTestOutput.png
             ${ITK_TEST_OUTPUT_DIR}/itkScalarImageKmeansImageFilterTestOutput.png 
    itkScalarImageKmeansImageFilterTest
    ${ITK_SOURCE_DIR}/Examples/Data/BrainT1Slice.png 
    ${ITK_TEST_OUTPUT_DIR}/itkScalarImageKmeansImageFilterTestOutput.png 1
    4 0.0 40.0 90.0 150.0 )
ADD_TEST(itkRegularSphereMeshSourceTest ${ALGORITHMS_TESTS2} itkRegularSphereMeshSourceTest)
ADD_TEST(itkRayCastInterpolateImageFunctionTest ${ALGORITHMS_TESTS2} itkRayCastInterpolateImageFunctionTest)
ADD_TEST(itkSimplexMeshVolumeCalculatorTest ${ALGORITHMS_TESTS2} itkSimplexMeshVolumeCalculatorTest)
ADD_TEST(itkShapeDetectionLevelSetImageFilterTest ${ALGORITHMS_TESTS3} itkShapeDetectionLevelSetImageFilterTest)
ADD_TEST(itkShapePriorMAPCostFunctionTest ${ALGORITHMS_TESTS3} itkShapePriorMAPCostFunctionTest)
ADD_TEST(itkShapePriorSegmentationLevelSetFunctionTest ${ALGORITHMS_TESTS3} itkShapePriorSegmentationLevelSetFunctionTest)
ADD_TEST(itkSpatialObjectToImageRegistrationTest ${ALGORITHMS_TESTS3} itkSpatialObjectToImageRegistrationTest)
ADD_TEST(itkSupervisedImageClassifierTest ${ALGORITHMS_TESTS3} itkSupervisedImageClassifierTest)
ADD_TEST(itkThresholdSegmentationLevelSetImageFilterTest ${ALGORITHMS_TESTS2} itkThresholdSegmentationLevelSetImageFilterTest)
ADD_TEST(itkGibbsTest ${ALGORITHMS_TESTS3} itkGibbsTest)
ADD_TEST(itkDeformableTest ${ALGORITHMS_TESTS3} itkDeformableTest)
ADD_TEST(itkDeformableSimplexMesh3DBalloonForceFilterTest ${ALGORITHMS_TESTS2} itkDeformableSimplexMesh3DBalloonForceFilterTest)
ADD_TEST(itkDeformableSimplexMesh3DFilterTest ${ALGORITHMS_TESTS2} itkDeformableSimplexMesh3DFilterTest)
ADD_TEST(itk2DDeformableTest ${ALGORITHMS_TESTS3} itk2DDeformableTest)
ADD_TEST(itkBinaryThinningImageFilterTest ${ALGORITHMS_TESTS3}
   --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/BinaryThinningImageFilterTest.png
             ${ITK_TEST_OUTPUT_DIR}/BinaryThinningImageFilterTest.png
  itkBinaryThinningImageFilterTest
             ${ITK_DATA_ROOT}/Input/Shapes.png
             ${ITK_TEST_OUTPUT_DIR}/BinaryThinningImageFilterTest.png 
             )
ADD_TEST(itkSphereMeshSourceTest ${ALGORITHMS_TESTS3} itkSphereMeshSourceTest)
ADD_TEST(itkUnsharpMaskLevelSetImageFilterTest ${ALGORITHMS_TESTS3} itkUnsharpMaskLevelSetImageFilterTest) 
ADD_TEST(itkVnlFFTTest ${ALGORITHMS_TESTS4} itkVnlFFTTest)
IF(USE_FFTW)
       ADD_TEST(itkFFTWFFTTest ${ALGORITHMS_TESTS4} itkFFTWFFTTest)
ENDIF(USE_FFTW)

IF(USE_SCSL)
       ADD_TEST(itkSCSLFFTTest ${ALGORITHMS_TESTS4} itkSCSLFFTTest)        
ENDIF(USE_SCSL)


ADD_TEST(itkMIRegistrationFunctionTest ${ALGORITHMS_TESTS2} itkMIRegistrationFunctionTest)
ADD_TEST(itkSymmetricForcesDemonsRegistrationFilterTest ${ALGORITHMS_TESTS2} itkSymmetricForcesDemonsRegistrationFilterTest)
ADD_TEST(itkVoronoiDiagram2DTest ${ALGORITHMS_TESTS2} itkVoronoiDiagram2DTest)
ADD_TEST(itkVoronoiSegmentationImageFilterTest ${ALGORITHMS_TESTS2} itkVoronoiSegmentationImageFilterTest)
ADD_TEST(itkWatershedImageFilterTest ${ALGORITHMS_TESTS4} itkWatershedImageFilterTest)
ADD_TEST(itkPointSetToPointSetRegistrationTest ${ALGORITHMS_TESTS2}  itkPointSetToPointSetRegistrationTest)
ADD_TEST(itkPointSetToSpatialObjectDemonsRegistrationTest ${ALGORITHMS_TESTS2}  itkPointSetToSpatialObjectDemonsRegistrationTest)

ADD_TEST(itkCurvatureFlowTest ${ALGORITHMS_TESTS}
    itkCurvatureFlowTest ${ITK_TEST_OUTPUT_DIR}/itkCurvatureFlowTest.vtk)

SET(AlgorithmsTest_SRCS
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
itkKullbackLeiblerCompareHistogramImageToImageMetricTest.cxx
itkLaplacianSegmentationLevelSetImageFilterTest.cxx
itkLevelSetNeighborhoodExtractorTest.cxx
itkLevelSetVelocityNeighborhoodExtractorTest.cxx
)

SET(AlgorithmsTest2_SRCS
itkCompareHistogramImageToImageMetricTest.cxx
itkDeformableSimplexMesh3DBalloonForceFilterTest.cxx
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

SET(AlgorithmsTest3_SRCS
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
itkScalarImageKmeansImageFilterTest.cxx
itkShapeDetectionLevelSetImageFilterTest.cxx
itkShapePriorMAPCostFunctionTest.cxx
itkShapePriorSegmentationLevelSetFunctionTest.cxx
itkSpatialObjectToImageRegistrationTest.cxx
itkSphereMeshSourceTest.cxx
itkSupervisedImageClassifierTest.cxx
itkUnsharpMaskLevelSetImageFilterTest.cxx
)

SET(AlgorithmsTest4_SRCS
itkBayesianClassifierImageFilterTest.cxx
itkFastMarchingUpwindGradientTest.cxx
itkCollidingFrontsImageFilterTest.cxx
itkCurvesLevelSetImageFilterZeroSigmaTest.cxx
itkGeodesicActiveContourLevelSetImageFilterZeroSigmaTest.cxx
itkNarrowBandCurvesLevelSetImageFilterTest.cxx
itkFFTTest.cxx
itkWatershedImageFilterTest.cxx
itkVoronoiPartitioningImageFilterTest.cxx
itkVectorThresholdSegmentationLevelSetImageFilterTest.cxx
itkMeanReciprocalSquareDifferencePointSetToImageMetricTest.cxx
)

ADD_EXECUTABLE(itkAlgorithmsPrintTest itkAlgorithmsPrintTest.cxx)
TARGET_LINK_LIBRARIES(itkAlgorithmsPrintTest ITKAlgorithms ITKIO 
                                              ITKBasicFilters ITKFEM)
ADD_EXECUTABLE(itkAlgorithmsPrintTest2 itkAlgorithmsPrintTest2.cxx)
TARGET_LINK_LIBRARIES(itkAlgorithmsPrintTest2 ITKAlgorithms ITKIO 
                                              ITKBasicFilters ITKFEM)
ADD_EXECUTABLE(itkAlgorithmsPrintTest3 itkAlgorithmsPrintTest3.cxx)
TARGET_LINK_LIBRARIES(itkAlgorithmsPrintTest3 ITKAlgorithms ITKIO 
                                              ITKBasicFilters ITKFEM)
ADD_EXECUTABLE(itkAlgorithmsPrintTest4 itkAlgorithmsPrintTest4.cxx)
TARGET_LINK_LIBRARIES(itkAlgorithmsPrintTest4 ITKAlgorithms ITKIO 
                                              ITKBasicFilters ITKFEM)

ADD_EXECUTABLE(itkAlgorithmsHeaderTest itkAlgorithmsHeaderTest.cxx)
TARGET_LINK_LIBRARIES(itkAlgorithmsHeaderTest ITKAlgorithms ITKIO 
                                              ITKBasicFilters ITKFEM)
ADD_TEST(itkAlgorithmsHeaderTest ${ALGORITHMS_HEADER_TESTS})

ADD_EXECUTABLE(itkAlgorithmsTests itkAlgorithmsTests.cxx 
                                  ${AlgorithmsTest_SRCS})
TARGET_LINK_LIBRARIES(itkAlgorithmsTests 
                      ITKAlgorithms ITKIO 
                      ITKBasicFilters ITKFEM ITKStatistics)

ADD_EXECUTABLE(itkAlgorithmsTests2 itkAlgorithmsTests2.cxx ${AlgorithmsTest2_SRCS})
TARGET_LINK_LIBRARIES(itkAlgorithmsTests2 ITKAlgorithms ITKIO ITKBasicFilters ITKFEM ITKStatistics)

ADD_EXECUTABLE(itkAlgorithmsTests3 itkAlgorithmsTests3.cxx ${AlgorithmsTest3_SRCS})
TARGET_LINK_LIBRARIES(itkAlgorithmsTests3 ITKAlgorithms ITKIO ITKBasicFilters ITKFEM ITKStatistics)

ADD_EXECUTABLE(itkAlgorithmsTests4 itkAlgorithmsTests4.cxx ${AlgorithmsTest4_SRCS})
TARGET_LINK_LIBRARIES(itkAlgorithmsTests4 ITKAlgorithms ITKIO ITKBasicFilters ITKFEM ITKStatistics)

IF(USE_FFTW)
        TARGET_LINK_LIBRARIES(itkAlgorithmsTests4 fftw3 fftw3f)
ENDIF(USE_FFTW)

IF(USE_SCSL)
        TARGET_LINK_LIBRARIES(itkAlgorithmsTests4 scs )
ENDIF(USE_SCSL)

IF(ITK_DATA_ROOT)

ADD_TEST(itkMatchCardinalityImageToImageMetricTest ${ALGORITHMS_TESTS2}
  itkMatchCardinalityImageToImageMetricTest ${ITK_DATA_ROOT}/Input/Spots.png)
                                                   
ADD_TEST(itkKappaStatisticImageToImageMetricTest ${ALGORITHMS_TESTS2}
  itkKappaStatisticImageToImageMetricTest ${ITK_DATA_ROOT}/Input/Spots.png)
                                                   
ADD_TEST(itkIsolatedWatershedImageFilterTest ${ALGORITHMS_TESTS2}
   --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/IsolatedWatershedImageFilterTest.png
             ${ITK_TEST_OUTPUT_DIR}/IsolatedWatershedImageFilterTest.png
  itkIsolatedWatershedImageFilterTest
             ${ITK_DATA_ROOT}/Input/cthead1.png
             ${ITK_TEST_OUTPUT_DIR}/IsolatedWatershedImageFilterTest.png
             113 84 120 99)

ADD_TEST(itkOtsuThresholdImageFilterTest ${ALGORITHMS_TESTS3}
  --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/OtsuThresholdImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/OtsuThresholdImageFilterTest.png
  itkOtsuThresholdImageFilterTest
            ${ITK_DATA_ROOT}/Input/cthead1.png
            ${ITK_TEST_OUTPUT_DIR}/OtsuThresholdImageFilterTest.png
            64)

ADD_TEST(itkOtsuMultipleThresholdsImageFilterTest ${ALGORITHMS_TESTS2}
  --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/OtsuMultipleThresholdsImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/OtsuMultipleThresholdsImageFilterTest.png
  itkOtsuMultipleThresholdsImageFilterTest
            ${ITK_DATA_ROOT}/Input/peppers.png
            ${ITK_TEST_OUTPUT_DIR}/OtsuMultipleThresholdsImageFilterTest.png
            64 3 2)

ADD_TEST(itkVoronoiPartitioningImageFilterTest1 ${ALGORITHMS_TESTS4}
  --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/VoronoiPartioningImageFilterTest1.png
            ${ITK_TEST_OUTPUT_DIR}/VoronoiPartioningImageFilterTest1.png
  itkVoronoiPartitioningImageFilterTest
            ${ITK_DATA_ROOT}/Input/sf4.png
            ${ITK_TEST_OUTPUT_DIR}/VoronoiPartioningImageFilterTest1.png 1)

ADD_TEST(itkVoronoiPartitioningImageFilterTest2 ${ALGORITHMS_TESTS4}
  --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/VoronoiPartioningImageFilterTest2.png
            ${ITK_TEST_OUTPUT_DIR}/VoronoiPartioningImageFilterTest2.png
  itkVoronoiPartitioningImageFilterTest
            ${ITK_DATA_ROOT}/Input/sf4.png
            ${ITK_TEST_OUTPUT_DIR}/VoronoiPartioningImageFilterTest2.png 0)

ADD_TEST(itkVectorThresholdSegmentationLevelSetImageFilterTest ${ALGORITHMS_TESTS4}
  --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/VectorThresholdSegmentationLevelSetImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/VectorThresholdSegmentationLevelSetImageFilterTest.png
  itkVectorThresholdSegmentationLevelSetImageFilterTest
            ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSliceInitialLevelSet.png
            ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png
            ${ITK_TEST_OUTPUT_DIR}/VectorThresholdSegmentationLevelSetImageFilterTest.png 
            5.0 50
            )

ADD_TEST(itkSTAPLEImageFilterTest ${ALGORITHMS_TESTS2}
   --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/STAPLEImageFilterTest.mha
             ${ITK_TEST_OUTPUT_DIR}/STAPLEImageFilterTest.mha
   itkSTAPLEImageFilterTest 
             2 ${ITK_TEST_OUTPUT_DIR}/STAPLEImageFilterTest.mha 255 0.5
             ${ITK_DATA_ROOT}/Input/STAPLE1.png
             ${ITK_DATA_ROOT}/Input/STAPLE2.png
             ${ITK_DATA_ROOT}/Input/STAPLE3.png
             ${ITK_DATA_ROOT}/Input/STAPLE4.png
            )   

ENDIF(ITK_DATA_ROOT)

ENDIF( NOT ITK_DISABLE_CXX_TESTING )


IF( NOT ITK_DISABLE_TCL_TESTING )
IF(ITK_CSWIG_TCL)

  ADD_TEST(itkCurvatureFlowTestTcl2 ${ITKWISH_EXECUTABLE}
           ${ITKTesting_SOURCE_DIR}/Code/Algorithms/itkCurvatureFlowTestTcl2.tcl)
  
  ADD_TEST(ImageRegistration3Tcl ${ITKWISH_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Registration/ImageRegistration3.tcl
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
           ${ITK_TEST_OUTPUT_DIR}/ImageRegistration3TclTest.png )

  ADD_TEST(ImageRegistration4Tcl ${ITKWISH_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Registration/ImageRegistration4.tcl
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
           ${ITK_TEST_OUTPUT_DIR}/ImageRegistration4TclTest.png )

  ADD_TEST(ImageRegistration5Tcl ${ITKWISH_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Registration/ImageRegistration5.tcl
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
           ${ITK_TEST_OUTPUT_DIR}/ImageRegistration5TclTest.png )

  ADD_TEST(VoronoiSegmentationTcl ${ITKWISH_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Segmentation/VoronoiSegmentation.tcl
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1Slice.png
           ${ITK_SOURCE_DIR}/Examples/Data/VentricleModel.png
           ${ITK_TEST_OUTPUT_DIR}/VoronoiSegmentationTcl.png  0.3 0.3 )

  ADD_TEST(WatershedSegmentation1Tcl ${ITKWISH_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Segmentation/WatershedSegmentation1.tcl
           ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
           ${ITK_TEST_OUTPUT_DIR}/WatershedSegmentation1Tcl.mhd )

ENDIF(ITK_CSWIG_TCL)
ENDIF( NOT ITK_DISABLE_TCL_TESTING )


IF( NOT ITK_DISABLE_PYTHON_TESTING )
IF(ITK_CSWIG_PYTHON)

  ADD_TEST(itkCurvatureFlowTestPython2 ${PYTHON_EXECUTABLE}
           ${ITKTesting_SOURCE_DIR}/Code/Algorithms/itkCurvatureFlowTestPython2.py)

  ADD_TEST(ImageRegistration3Python ${PYTHON_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Registration/ImageRegistration3.py
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
           ${ITK_TEST_OUTPUT_DIR}/ImageRegistration3PythonTest.png )

  ADD_TEST(ImageRegistration4Python ${PYTHON_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Registration/ImageRegistration4.py
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
           ${ITK_TEST_OUTPUT_DIR}/ImageRegistration4PythonTest.png )

  ADD_TEST(ImageRegistration5Python ${PYTHON_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Registration/ImageRegistration5.py
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceShifted13x17y.png
           ${ITK_TEST_OUTPUT_DIR}/ImageRegistration5PythonTest.png )
  
  ADD_TEST(VoronoiSegmentationPython ${PYTHON_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Segmentation/VoronoiSegmentation.py
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1Slice.png
           ${ITK_SOURCE_DIR}/Examples/Data/VentricleModel.png
           ${ITK_TEST_OUTPUT_DIR}/VoronoiSegmentationPython.png  0.3 0.3 )

  ADD_TEST(WatershedSegmentation1Python ${PYTHON_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Segmentation/WatershedSegmentation1.py
           ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
           ${ITK_TEST_OUTPUT_DIR}/WatershedSegmentation1Python.mhd )

ENDIF(ITK_CSWIG_PYTHON)
ENDIF( NOT ITK_DISABLE_PYTHON_TESTING )


IF( NOT ITK_DISABLE_JAVA_TESTING )
IF(ITK_CSWIG_JAVA AND ITK_BINARY_DIR)
  SET(JAVACLASSPATH ${ITK_BINARY_DIR}/Wrapping/CSwig/Java/InsightToolkit.jar)
  FOREACH(cfgtype ${CMAKE_CONFIGURATION_TYPES})
    SET(JAVACLASSPATH "${JAVACLASSPATH}\\;${ITK_BINARY_DIR}/Wrapping/CSwig/Java/${cfgtype}/InsightToolkit.jar")
  ENDFOREACH(cfgtype)

  FOREACH( javatestfile 
     VoronoiSegmentation
     WatershedSegmentation1
     )

  ADD_CUSTOM_TARGET(${javatestfile}Java ALL  DEPENDS ${ITK_BINARY_DIR}/Examples/Segmentation/${javatestfile}.class)

  ADD_CUSTOM_COMMAND(
    OUTPUT  ${ITK_BINARY_DIR}/Examples/Segmentation/${javatestfile}.class
    DEPENDS ${ITK_SOURCE_DIR}/Examples/Segmentation/${javatestfile}.java
    COMMAND ${JAVA_COMPILE}
    ARGS -classpath "${JAVACLASSPATH}" -d "${ITK_BINARY_DIR}/Examples/Segmentation"
    ${ITK_SOURCE_DIR}/Examples/Segmentation/${javatestfile}.java
    COMMENT "Java Class")

  ENDFOREACH( javatestfile )

  IF(WIN32)
    SET(TMPJAVACLASSPATH ${ITK_BINARY_DIR}/Examples/Segmentation/)
    FOREACH(javadirectory ${JAVACLASSPATH})
      # Set path separator for Windows
      SET(TMPJAVACLASSPATH "${TMPJAVACLASSPATH}\\;${javadirectory}")
    ENDFOREACH(javadirectory)
    SET(JAVACLASSPATH ${TMPJAVACLASSPATH})
  ELSE(WIN32)
    SET(TMPJAVACLASSPATH ${ITK_BINARY_DIR}/Examples/Segmentation/)
    FOREACH(javadirectory ${JAVACLASSPATH})
      # Set path separator for UNIX
      SET(TMPJAVACLASSPATH "${TMPJAVACLASSPATH}:${javadirectory}")
    ENDFOREACH(javadirectory)
    SET(JAVACLASSPATH ${TMPJAVACLASSPATH})
  ENDIF(WIN32)
 
  ADD_TEST(VoronoiSegmentationJava ${JAVA_RUNTIME} -classpath "${JAVACLASSPATH}"
           VoronoiSegmentation
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1Slice.png
           ${ITK_SOURCE_DIR}/Examples/Data/VentricleModel.png
           ${ITK_TEST_OUTPUT_DIR}/VoronoiSegmentationJava.png  0.3 0.3 )

  ADD_TEST(WatershedSegmentation1Java ${JAVA_RUNTIME} -classpath "${JAVACLASSPATH}"
           WatershedSegmentation1
           ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
           ${ITK_TEST_OUTPUT_DIR}/WatershedSegmentation1Java.mhd  )


ENDIF(ITK_CSWIG_JAVA AND ITK_BINARY_DIR)
ENDIF( NOT ITK_DISABLE_JAVA_TESTING )
