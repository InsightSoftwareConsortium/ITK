SET(KIT Algorithms)
CREATE_TEST_SOURCELIST(Tests itk${KIT}Tests.cxx 
  itkAntiAliasBinaryImageFilterTest.cxx
  itkBinaryMinMaxCurvatureFlowImageFilterTest.cxx
  itkBinaryMask3DMeshSourceTest.cxx
  itkCannySegmentationLevelSetImageFilterTest.cxx
  itkCurvatureFlowTest.cxx
  itkDemonsRegistrationFilterTest.cxx
  itkExtractMeshConnectedRegionsTest.cxx
  itkFastMarchingTest.cxx
  itkFastMarchingExtensionImageFilterTest.cxx
  #itkFEMRegistrationFilterTest.cxx
  itkGeodesicActiveContourLevelSetImageFilterTest.cxx
  itkGradientVectorFlowImageFilterTest.cxx
  itkSimpleFuzzyConnectednessScalarImageFilterTest.cxx
  itkHistogramMatchingImageFilterTest.cxx
  itkImageMomentsTest.cxx
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
  itkInterpolateTest.cxx
  itkKalmanLinearEstimatorTest.cxx
  itkKmeansModelEstimatorTest.cxx
  itkLaplacianSegmentationLevelSetImageFilterTest.cxx
  itkMattesMutualInformationImageToImageMetricTest.cxx
  itkMeanSquaresImageMetricTest.cxx
  itkMinimumMaximumImageCalculatorTest.cxx
  itkMinMaxCurvatureFlowImageFilterTest.cxx
  itkMRFImageFilterTest.cxx
  itkMultiResolutionPyramidImageFilterTest.cxx
  itkRecursiveMultiResolutionPyramidImageFilterTest.cxx
  itkMultiResolutionPDEDeformableRegistrationTest.cxx
  itkMultiResolutionImageRegistrationMethodTest.cxx
  itkMultiResolutionImageRegistrationMethodTest_1.cxx
  itkMultiResolutionImageRegistrationMethodTest_2.cxx
  itkMutualInformationMetricTest.cxx
  itkNewTest.cxx
  itkNormalizedCorrelationImageMetricTest.cxx
  itkOtsuThresholdImageCalculatorTest.cxx
  itkPatternIntensityImageMetricTest.cxx
  itkRegionGrow2DTest.cxx
  itkShapeDetectionLevelSetImageFilterTest.cxx
  itkSpatialObjectToImageRegistrationTest.cxx
  itkSupervisedImageClassifierTest.cxx
  itkThresholdSegmentationLevelSetImageFilterTest.cxx
  itkGibbsTest.cxx
  itkDeformableTest.cxx
  itk2DDeformableTest.cxx
  itkVectorFuzzyConnectednessImageFilterTest.cxx
  itkVoronoiDiagram2DTest.cxx
  itkVoronoiSegmentationImageFilterTest.cxx
  itkWatershedImageFilterTest.cxx
)

ADD_EXECUTABLE(itk${KIT}Tests ${Tests})
TARGET_LINK_LIBRARIES(itk${KIT}Tests ITKAlgorithms ITKIO ITKFEM ITKStatistics)

SET (TestsToRun ${Tests})
REMOVE (TestsToRun itk${KIT}Tests.cxx)

FOREACH (test ${TestsToRun})
  GET_FILENAME_COMPONENT(TName ${test} NAME_WE)
  ADD_TEST(${TName} ${CXX_TEST_PATH}/itk${KIT}Tests ${TName}) 
ENDFOREACH (test)

# Add PrintSelf test.
IF (TCL_TCLSH)
  ADD_TEST(PrintSelf-${KIT} ${TCL_TCLSH}
           ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl  
           ${ITK_SOURCE_DIR}/Code/${KIT})
ENDIF (TCL_TCLSH)

# Add header test.
ADD_EXECUTABLE(itk${KIT}HeaderTest itk${KIT}HeaderTest.cxx)
TARGET_LINK_LIBRARIES(itk${KIT}HeaderTest ITKAlgorithms ITKIO ITKFEM ITKStatistics)
ADD_TEST(itk${KIT}HeaderTest ${CXX_TEST_PATH}/itk${KIT}HeaderTest)

IF(ITK_WRAP_TCL)
  ADD_TEST(itkCurvatureFlowTestTcl ${ITKTCL_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Testing/Code/Algorithms/itkCurvatureFlowTestTcl.tcl)
ENDIF(ITK_WRAP_TCL)
