IF(ITK_BINARY_DIR)
  IF(TCL_TCLSH)
    ADD_TEST(PrintSelf-Algorithms ${TCL_TCLSH}
             ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl  
             ${ITK_SOURCE_DIR}/Code/Algorithms)
  ENDIF(TCL_TCLSH)
ENDIF(ITK_BINARY_DIR)

IF( NOT ITK_DISABLE_CXX_TESTING )

SET(ALGORITHMS_TESTS ${CXX_TEST_PATH}/itkAlgorithmsTests)
SET(ALGORITHMS_TESTS2 ${CXX_TEST_PATH}/itkAlgorithmsTests2)
SET(ALGORITHMS_PRINT_TESTS ${CXX_TEST_PATH}/itkAlgorithmsPrintTest)
SET(ALGORITHMS_HEADER_TESTS ${CXX_TEST_PATH}/itkAlgorithmsHeaderTest)

ADD_TEST(itkAlgorithmsPrintTest ${ALGORITHMS_TESTS2} itkAlgorithmsPrintTest)
ADD_TEST(itkAlgorithmsPrintTest2 ${ALGORITHMS_TESTS2} itkAlgorithmsPrintTest2)
ADD_TEST(itkAlgorithmsPrintTest3 ${ALGORITHMS_TESTS2} itkAlgorithmsPrintTest3)
ADD_TEST(itkAnisotropicFourthOrderLevelSetImageFilterTest ${ALGORITHMS_TESTS} itkAnisotropicFourthOrderLevelSetImageFilterTest)
ADD_TEST(itkAntiAliasBinaryImageFilterTest ${ALGORITHMS_TESTS} itkAntiAliasBinaryImageFilterTest)
ADD_TEST(itkAutomaticTopologyMeshSourceTest ${ALGORITHMS_TESTS} itkAutomaticTopologyMeshSourceTest)
ADD_TEST(itkBinaryMinMaxCurvatureFlowImageFilterTest ${ALGORITHMS_TESTS} itkBinaryMinMaxCurvatureFlowImageFilterTest )
ADD_TEST(itkBinaryMask3DMeshSourceTest ${ALGORITHMS_TESTS} itkBinaryMask3DMeshSourceTest )
ADD_TEST(itkBioGeneTest ${ALGORITHMS_TESTS} itkBioGeneTest )
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
ADD_TEST(itkFEMRegistrationFilterTest ${ALGORITHMS_TESTS} itkFEMRegistrationFilterTest)
ADD_TEST(itkGeodesicActiveContourLevelSetImageFilterTest ${ALGORITHMS_TESTS} itkGeodesicActiveContourLevelSetImageFilterTest)
ADD_TEST(itkGeodesicActiveContourShapePriorLevelSetImageFilterTest ${ALGORITHMS_TESTS} itkGeodesicActiveContourShapePriorLevelSetImageFilterTest)
ADD_TEST(itkGeodesicActiveContourShapePriorLevelSetImageFilterTest_2 ${ALGORITHMS_TESTS} itkGeodesicActiveContourShapePriorLevelSetImageFilterTest_2)
ADD_TEST(itkGradientVectorFlowImageFilterTest ${ALGORITHMS_TESTS} itkGradientVectorFlowImageFilterTest)
ADD_TEST(itkGradientDifferenceImageToImageMetricTest ${ALGORITHMS_TESTS2} itkGradientDifferenceImageToImageMetricTest)
ADD_TEST(itkSimpleFuzzyConnectednessScalarImageFilterTest ${ALGORITHMS_TESTS} itkSimpleFuzzyConnectednessScalarImageFilterTest)
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
ADD_TEST(itkMattesMutualInformationImageToImageMetricTest ${ALGORITHMS_TESTS} itkMattesMutualInformationImageToImageMetricTest)
ADD_TEST(itkMeanSquaresImageMetricTest ${ALGORITHMS_TESTS} itkMeanSquaresImageMetricTest)
ADD_TEST(itkMeanSquaresHistogramImageToImageMetricTest ${ALGORITHMS_TESTS} itkMeanSquaresHistogramImageToImageMetricTest)
ADD_TEST(itkMinMaxCurvatureFlowImageFilterTest ${ALGORITHMS_TESTS} itkMinMaxCurvatureFlowImageFilterTest)
ADD_TEST(itkMRFImageFilterTest ${ALGORITHMS_TESTS} itkMRFImageFilterTest)
ADD_TEST(itkMRIBiasFieldCorrectionFilterTest ${ALGORITHMS_TESTS} itkMRIBiasFieldCorrectionFilterTest)
ADD_TEST(itkMultiResolutionPyramidImageFilterTest ${ALGORITHMS_TESTS} itkMultiResolutionPyramidImageFilterTest)
ADD_TEST(itkMutualInformationHistogramImageToImageMetricTest ${ALGORITHMS_TESTS} itkMutualInformationHistogramImageToImageMetricTest)
ADD_TEST(itkRecursiveMultiResolutionPyramidImageFilterTest ${ALGORITHMS_TESTS} itkRecursiveMultiResolutionPyramidImageFilterTest)
ADD_TEST(itkMultiResolutionPDEDeformableRegistrationTest ${ALGORITHMS_TESTS} itkMultiResolutionPDEDeformableRegistrationTest)
ADD_TEST(itkMultiResolutionImageRegistrationMethodTest ${ALGORITHMS_TESTS}   
itkMultiResolutionImageRegistrationMethodTest  )
ADD_TEST(itkMultiResolutionImageRegistrationMethodTest_1 ${ALGORITHMS_TESTS}   
itkMultiResolutionImageRegistrationMethodTest_1  )
ADD_TEST(itkMultiResolutionImageRegistrationMethodTest_2 ${ALGORITHMS_TESTS}   
itkMultiResolutionImageRegistrationMethodTest_2  )
ADD_TEST(itkMutualInformationMetricTest ${ALGORITHMS_TESTS} itkMutualInformationMetricTest)
ADD_TEST(itkNarrowBandCurvesLevelSetImageFilterTest ${ALGORITHMS_TESTS} itkNarrowBandCurvesLevelSetImageFilterTest)
ADD_TEST(itkNarrowBandThresholdSegmentationLevelSetImageFilterTest ${ALGORITHMS_TESTS} itkNarrowBandThresholdSegmentationLevelSetImageFilterTest)
ADD_TEST(itkNewTest ${ALGORITHMS_TESTS} itkNewTest)
ADD_TEST(itkNormalizedCorrelationImageMetricTest ${ALGORITHMS_TESTS} itkNormalizedCorrelationImageMetricTest)
ADD_TEST(itkNormalizedCorrelationPointSetToImageMetricTest ${ALGORITHMS_TESTS} itkNormalizedCorrelationPointSetToImageMetricTest)
ADD_TEST(itkMeanSquaresPointSetToImageMetricTest ${ALGORITHMS_TESTS} itkMeanSquaresPointSetToImageMetricTest)
ADD_TEST(itkNormalizedMutualInformationHistogramImageToImageMetricTest ${ALGORITHMS_TESTS} itkNormalizedMutualInformationHistogramImageToImageMetricTest)
ADD_TEST(itkOtsuThresholdImageCalculatorTest ${ALGORITHMS_TESTS} itkOtsuThresholdImageCalculatorTest)
ADD_TEST(itkOtsuMultipleThresholdsCalculatorTest ${ALGORITHMS_TESTS2} itkOtsuMultipleThresholdsCalculatorTest)
ADD_TEST(itkOrthogonalSwath2DPathFilterTest ${ALGORITHMS_TESTS} itkOrthogonalSwath2DPathFilterTest)
ADD_TEST(itkMeanReciprocalSquareDifferenceImageMetricTest ${ALGORITHMS_TESTS}  itkMeanReciprocalSquareDifferenceImageMetricTest)
ADD_TEST(itkPointSetToImageRegistrationTest_1 ${ALGORITHMS_TESTS}  itkPointSetToImageRegistrationTest_1)
ADD_TEST(itkRegionGrow2DTest ${ALGORITHMS_TESTS} itkRegionGrow2DTest)
ADD_TEST(itkReinitializeLevelSetImageFilterTest ${ALGORITHMS_TESTS} itkReinitializeLevelSetImageFilterTest)
ADD_TEST(itkRegularSphereMeshSourceTest ${ALGORITHMS_TESTS2} itkRegularSphereMeshSourceTest)
ADD_TEST(itkRayCastInterpolateImageFunctionTest ${ALGORITHMS_TESTS2} itkRayCastInterpolateImageFunctionTest)
ADD_TEST(itkShapeDetectionLevelSetImageFilterTest ${ALGORITHMS_TESTS} itkShapeDetectionLevelSetImageFilterTest)
ADD_TEST(itkShapePriorMAPCostFunctionTest ${ALGORITHMS_TESTS} itkShapePriorMAPCostFunctionTest)
ADD_TEST(itkShapePriorSegmentationLevelSetFunctionTest ${ALGORITHMS_TESTS} itkShapePriorSegmentationLevelSetFunctionTest)
ADD_TEST(itkSpatialObjectToImageRegistrationTest ${ALGORITHMS_TESTS} itkSpatialObjectToImageRegistrationTest)
ADD_TEST(itkSupervisedImageClassifierTest ${ALGORITHMS_TESTS} itkSupervisedImageClassifierTest)
ADD_TEST(itkSwathChainCodePathFilterTest ${ALGORITHMS_TESTS} itkSwathChainCodePathFilterTest)
ADD_TEST(itkThresholdSegmentationLevelSetImageFilterTest ${ALGORITHMS_TESTS} itkThresholdSegmentationLevelSetImageFilterTest)
ADD_TEST(itkGibbsTest ${ALGORITHMS_TESTS} itkGibbsTest)
ADD_TEST(itkDeformableTest ${ALGORITHMS_TESTS} itkDeformableTest)
ADD_TEST(itkDeformableSimplexMesh3DBalloonForceFilterTest ${ALGORITHMS_TESTS2} itkDeformableSimplexMesh3DBalloonForceFilterTest)
ADD_TEST(itkDeformableSimplexMesh3DFilterTest ${ALGORITHMS_TESTS2} itkDeformableSimplexMesh3DFilterTest)
ADD_TEST(itk2DDeformableTest ${ALGORITHMS_TESTS} itk2DDeformableTest)
ADD_TEST(itkSphereMeshSourceTest ${ALGORITHMS_TESTS} itkSphereMeshSourceTest)
ADD_TEST(itkUnsharpMaskLevelSetImageFilterTest ${ALGORITHMS_TESTS} itkUnsharpMaskLevelSetImageFilterTest) 
ADD_TEST(itkVnlFFTTest ${ALGORITHMS_TESTS2} itkVnlFFTTest)
IF(USE_FFTW)
       ADD_TEST(itkFFTWFFTTest ${ALGORITHMS_TESTS2} itkFFTWFFTTest)
ENDIF(USE_FFTW)

IF(USE_SCSL)
       ADD_TEST(itkSCSLFFTTest ${ALGORITHMS_TESTS2} itkSCSLFFTTest)        
ENDIF(USE_SCSL)


ADD_TEST(itkMIRegistrationFunctionTest ${ALGORITHMS_TESTS2} itkMIRegistrationFunctionTest)
ADD_TEST(itkSymmetricForcesDemonsRegistrationFilterTest ${ALGORITHMS_TESTS2} itkSymmetricForcesDemonsRegistrationFilterTest)
ADD_TEST(itkVectorFuzzyConnectednessImageFilterTest ${ALGORITHMS_TESTS2} itkVectorFuzzyConnectednessImageFilterTest)
ADD_TEST(itkVoronoiDiagram2DTest ${ALGORITHMS_TESTS2} itkVoronoiDiagram2DTest)
ADD_TEST(itkVoronoiSegmentationImageFilterTest ${ALGORITHMS_TESTS2} itkVoronoiSegmentationImageFilterTest)
ADD_TEST(itkWatershedImageFilterTest ${ALGORITHMS_TESTS2} itkWatershedImageFilterTest)
ADD_TEST(itkPointSetToPointSetRegistrationTest ${ALGORITHMS_TESTS2}  itkPointSetToPointSetRegistrationTest)
ADD_TEST(itkPointSetToSpatialObjectDemonsRegistrationTest ${ALGORITHMS_TESTS2}  itkPointSetToSpatialObjectDemonsRegistrationTest)


ADD_TEST(itkCurvatureFlowTest ${ALGORITHMS_TESTS}
    itkCurvatureFlowTest ${ITK_TEST_OUTPUT_DIR}/itkCurvatureFlowTest.vtk)

SET(AlgorithmsTest_SRCS
itkAnisotropicFourthOrderLevelSetImageFilterTest.cxx
itkAntiAliasBinaryImageFilterTest.cxx
itkAutomaticTopologyMeshSourceTest.cxx
itkBinaryMinMaxCurvatureFlowImageFilterTest.cxx
itkBinaryMask3DMeshSourceTest.cxx
itkBioGeneTest.cxx
itkBioGeneNetworkTest.cxx
itkCannySegmentationLevelSetImageFilterTest.cxx
itkCorrelationCoefficientHistogramImageToImageMetricTest.cxx
itkCurvatureFlowTest.cxx
itkDemonsRegistrationFilterTest.cxx
itkExtensionVelocitiesImageFilterTest.cxx
itkExtractMeshConnectedRegionsTest.cxx
itkFastMarchingTest.cxx
itkFastMarchingExtensionImageFilterTest.cxx
itkFEMRegistrationFilterTest.cxx
itkGeodesicActiveContourLevelSetImageFilterTest.cxx
itkGeodesicActiveContourShapePriorLevelSetImageFilterTest.cxx
itkGeodesicActiveContourShapePriorLevelSetImageFilterTest_2.cxx
itkGradientVectorFlowImageFilterTest.cxx
itkSimpleFuzzyConnectednessScalarImageFilterTest.cxx
itkHistogramMatchingImageFilterTest.cxx
itkHistogramImageToImageMetricTest.cxx
itkImageMomentsTest.cxx
itkKullbackLeiblerCompareHistogramImageToImageMetricTest.cxx
itkSphereMeshSourceTest.cxx
itkImageRegistrationMethodTest.cxx
itkImageRegistrationMethodTest_1.cxx
itkImageRegistrationMethodTest_2.cxx
itkImageRegistrationMethodTest_3.cxx
itkImageRegistrationMethodTest_4.cxx
itkImageRegistrationMethodTest_5.cxx
itkImageRegistrationMethodTest_6.cxx
itkImageRegistrationMethodTest_7.cxx
itkImageRegistrationMethodTest_8.cxx
itkImageRegistrationMethodTest_9.cxx
itkImageRegistrationMethodTest_10.cxx
itkImageRegistrationMethodTest_11.cxx
itkImageRegistrationMethodTest_12.cxx
itkImageRegistrationMethodTest_13.cxx
itkImageRegistrationMethodTest_14.cxx
itkImageRegistrationMethodTest_15.cxx
itkImageRegistrationMethodTest_16.cxx
itkInterpolateTest.cxx
itkIsotropicFourthOrderLevelSetImageFilterTest.cxx
itkKalmanLinearEstimatorTest.cxx
itkKmeansModelEstimatorTest.cxx
itkLaplacianSegmentationLevelSetImageFilterTest.cxx
itkLevelSetNeighborhoodExtractorTest.cxx
itkLevelSetVelocityNeighborhoodExtractorTest.cxx
itkMattesMutualInformationImageToImageMetricTest.cxx
itkMeanSquaresImageMetricTest.cxx
itkMeanSquaresHistogramImageToImageMetricTest.cxx
itkMinMaxCurvatureFlowImageFilterTest.cxx
itkMRFImageFilterTest.cxx
itkMRIBiasFieldCorrectionFilterTest.cxx
itkMultiResolutionPyramidImageFilterTest.cxx
itkRecursiveMultiResolutionPyramidImageFilterTest.cxx
itkMultiResolutionPDEDeformableRegistrationTest.cxx
itkMultiResolutionImageRegistrationMethodTest.cxx
itkMultiResolutionImageRegistrationMethodTest_1.cxx
itkMultiResolutionImageRegistrationMethodTest_2.cxx
itkMutualInformationHistogramImageToImageMetricTest.cxx
itkMutualInformationMetricTest.cxx
itkNewTest.cxx
itkNormalizedCorrelationImageMetricTest.cxx
itkNormalizedCorrelationPointSetToImageMetricTest.cxx
itkMeanSquaresPointSetToImageMetricTest.cxx
itkNormalizedMutualInformationHistogramImageToImageMetricTest.cxx
itkOtsuThresholdImageCalculatorTest.cxx
itkOtsuThresholdImageFilterTest.cxx
itkOrthogonalSwath2DPathFilterTest.cxx
itkMeanReciprocalSquareDifferenceImageMetricTest.cxx
itkPointSetToImageRegistrationTest_1.cxx
itkRegionGrow2DTest.cxx
itkReinitializeLevelSetImageFilterTest.cxx
itkShapeDetectionLevelSetImageFilterTest.cxx
itkShapePriorMAPCostFunctionTest.cxx
itkShapePriorSegmentationLevelSetFunctionTest.cxx
itkSpatialObjectToImageRegistrationTest.cxx
itkSupervisedImageClassifierTest.cxx
itkSwathChainCodePathFilterTest.cxx
itkThresholdSegmentationLevelSetImageFilterTest.cxx
itkUnsharpMaskLevelSetImageFilterTest.cxx
itkGibbsTest.cxx
itkDeformableTest.cxx
itk2DDeformableTest.cxx
itkIsoContourDistanceImageFilterTest.cxx
itkFastChamferDistanceImageFilterTest.cxx
itkCurvesLevelSetImageFilterTest.cxx
itkNarrowBandCurvesLevelSetImageFilterTest.cxx
itkNarrowBandThresholdSegmentationLevelSetImageFilterTest.cxx
)

SET(AlgorithmsTest2_SRCS
itkAlgorithmsPrintTest.cxx
itkAlgorithmsPrintTest2.cxx
itkCompareHistogramImageToImageMetricTest.cxx
itkGradientDifferenceImageToImageMetricTest.cxx
itkKullbackLeiblerCompareHistogramImageToImageMetricTest.cxx
itkImagePCAShapeModelEstimatorTest.cxx
itkImagePCADecompositionCalculatorTest.cxx
itkIsolatedWatershedImageFilterTest.cxx
itkMIRegistrationFunctionTest.cxx
itkOtsuMultipleThresholdsCalculatorTest.cxx
itkPointSetToPointSetRegistrationTest.cxx
itkPointSetToSpatialObjectDemonsRegistrationTest.cxx
itkDeformableSimplexMesh3DBalloonForceFilterTest.cxx
itkDeformableSimplexMesh3DFilterTest.cxx
itkRegularSphereMeshSourceTest.cxx
itkRayCastInterpolateImageFunctionTest.cxx
itkSymmetricForcesDemonsRegistrationFilterTest.cxx
itkSTAPLEImageFilterTest.cxx
itkVectorFuzzyConnectednessImageFilterTest.cxx
itkVectorThresholdSegmentationLevelSetImageFilterTest.cxx
itkVoronoiDiagram2DTest.cxx
itkVoronoiPartitioningImageFilterTest.cxx
itkVoronoiSegmentationImageFilterTest.cxx
itkWatershedImageFilterTest.cxx
itkFFTTest.cxx
)

SET(AlgorithmsPrintTest_SRCS
itkAlgorithmsPrintTest3.cxx
)

ADD_EXECUTABLE(itkAlgorithmsPrintTest itkAlgorithmsPrintTests.cxx ${AlgorithmsPrintTest_SRCS})
TARGET_LINK_LIBRARIES(itkAlgorithmsPrintTest ITKAlgorithms ITKIO 
                                              ITKBasicFilters ITKFEM)
ADD_TEST(itkAlgorithmsPrintTest ${ALGORITHMS_PRINT_TESTS})


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
IF(USE_FFTW)
        TARGET_LINK_LIBRARIES(itkAlgorithmsTests2 fftw3 fftw3f)
ENDIF(USE_FFTW)

IF(USE_SCSL)
        TARGET_LINK_LIBRARIES(itkAlgorithmsTests2 scs )
ENDIF(USE_SCSL)

IF(ITK_DATA_ROOT)

ADD_TEST(itkIsolatedWatershedImageFilterTest ${ALGORITHMS_TESTS2}
   --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/IsolatedWatershedImageFilterTest.png
             ${ITK_TEST_OUTPUT_DIR}/IsolatedWatershedImageFilterTest.png
  itkIsolatedWatershedImageFilterTest
             ${ITK_DATA_ROOT}/Input/cthead1.png
             ${ITK_TEST_OUTPUT_DIR}/IsolatedWatershedImageFilterTest.png
             113 84 120 99)

ADD_TEST(itkOtsuThresholdImageFilterTest ${ALGORITHMS_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/OtsuThresholdImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/OtsuThresholdImageFilterTest.png
  itkOtsuThresholdImageFilterTest
            ${ITK_DATA_ROOT}/Input/cthead1.png
            ${ITK_TEST_OUTPUT_DIR}/OtsuThresholdImageFilterTest.png
            64)

ADD_TEST(itkVoronoiPartitioningImageFilterTest1 ${ALGORITHMS_TESTS2}
  --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/VoronoiPartioningImageFilterTest1.png
            ${ITK_TEST_OUTPUT_DIR}/VoronoiPartioningImageFilterTest1.png
  itkVoronoiPartitioningImageFilterTest
            ${ITK_DATA_ROOT}/Input/sf4.png
            ${ITK_TEST_OUTPUT_DIR}/VoronoiPartioningImageFilterTest1.png 1)

ADD_TEST(itkVoronoiPartitioningImageFilterTest2 ${ALGORITHMS_TESTS2}
  --compare ${ITK_DATA_ROOT}/Baseline/Algorithms/VoronoiPartioningImageFilterTest2.png
            ${ITK_TEST_OUTPUT_DIR}/VoronoiPartioningImageFilterTest2.png
  itkVoronoiPartitioningImageFilterTest
            ${ITK_DATA_ROOT}/Input/sf4.png
            ${ITK_TEST_OUTPUT_DIR}/VoronoiPartioningImageFilterTest2.png 0)

ADD_TEST(itkVectorThresholdSegmentationLevelSetImageFilterTest ${ALGORITHMS_TESTS2}
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


ENDIF(ITK_CSWIG_TCL)



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

ENDIF(ITK_CSWIG_PYTHON)

