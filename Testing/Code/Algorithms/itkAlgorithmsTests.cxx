// this file defines the itkBasicFiltersTest for the test driver
// and all it expects is that you have a function called RegisterTests

#include "itkTestMain.h" 


void RegisterTests()
{
REGISTER_TEST(itkAlgorithmsPrintTest  );
REGISTER_TEST(itkAlgorithmsPrintTest2  );
REGISTER_TEST(itkAlgorithmsPrintTest3  );
REGISTER_TEST(itkAntiAliasBinaryImageFilterTest );
REGISTER_TEST(itkAutomaticTopologyMeshSourceTest );
REGISTER_TEST(itkAntiAliasBinaryImageFilterTest  );
REGISTER_TEST(itkBinaryMinMaxCurvatureFlowImageFilterTest  );  
REGISTER_TEST(itkBinaryMask3DMeshSourceTest  );  
REGISTER_TEST(itkBioGeneTest  );  
REGISTER_TEST(itkBioGeneNetworkTest  );  
REGISTER_TEST(itkCannySegmentationLevelSetImageFilterTest );
REGISTER_TEST(itkCurvatureFlowTest );
REGISTER_TEST(itkDemonsRegistrationFilterTest );
REGISTER_TEST(itkExtensionVelocitiesImageFilterTest );
REGISTER_TEST(itkExtractMeshConnectedRegionsTest );
REGISTER_TEST(itkFastMarchingTest );
REGISTER_TEST(itkFastMarchingExtensionImageFilterTest );
//REGISTER_TEST(itkFEMRegistrationFilterTest );
REGISTER_TEST(itkGeodesicActiveContourLevelSetImageFilterTest );
REGISTER_TEST(itkGeodesicActiveContourShapePriorLevelSetImageFilterTest );
REGISTER_TEST(itkGeodesicActiveContourShapePriorLevelSetImageFilterTest_2 );
REGISTER_TEST(itkGradientVectorFlowImageFilterTest );
REGISTER_TEST(itkSimpleFuzzyConnectednessScalarImageFilterTest );
REGISTER_TEST(itkHistogramMatchingImageFilterTest );
REGISTER_TEST(itkImageMomentsTest );
REGISTER_TEST(itkImagePCAShapeModelEstimatorTest);
REGISTER_TEST(itkImageRegistrationMethodTest     );
REGISTER_TEST(itkImageRegistrationMethodTest_1 );
REGISTER_TEST(itkImageRegistrationMethodTest_2 );
REGISTER_TEST(itkImageRegistrationMethodTest_3 );
REGISTER_TEST(itkImageRegistrationMethodTest_4 );
REGISTER_TEST(itkImageRegistrationMethodTest_5 );
REGISTER_TEST(itkImageRegistrationMethodTest_6 );
REGISTER_TEST(itkImageRegistrationMethodTest_7 );
REGISTER_TEST(itkImageRegistrationMethodTest_8 );
REGISTER_TEST(itkImageRegistrationMethodTest_9 );
REGISTER_TEST(itkImageRegistrationMethodTest_10);
REGISTER_TEST(itkImageRegistrationMethodTest_11);
REGISTER_TEST(itkImageRegistrationMethodTest_12);
REGISTER_TEST(itkImageRegistrationMethodTest_13);
REGISTER_TEST(itkImageRegistrationMethodTest_14);
REGISTER_TEST(itkImageRegistrationMethodTest_15);
REGISTER_TEST(itkImageRegistrationMethodTest_16);
REGISTER_TEST(itkInterpolateTest );
REGISTER_TEST(itkKalmanLinearEstimatorTest );
REGISTER_TEST(itkKmeansModelEstimatorTest );
REGISTER_TEST(itkLaplacianSegmentationLevelSetImageFilterTest );
REGISTER_TEST(itkLevelSetNeighborhoodExtractorTest );
REGISTER_TEST(itkLevelSetVelocityNeighborhoodExtractorTest );
REGISTER_TEST(itkMattesMutualInformationImageToImageMetricTest );
REGISTER_TEST(itkMeanSquaresImageMetricTest );
REGISTER_TEST(itkMinMaxCurvatureFlowImageFilterTest );
REGISTER_TEST(itkMRFImageFilterTest );
REGISTER_TEST(itkMRIBiasFieldCorrectionFilterTest );
REGISTER_TEST(itkMultiResolutionPyramidImageFilterTest );
REGISTER_TEST(itkRecursiveMultiResolutionPyramidImageFilterTest );
REGISTER_TEST(itkMultiResolutionPDEDeformableRegistrationTest );
REGISTER_TEST(itkMultiResolutionImageRegistrationMethodTest);
REGISTER_TEST(itkMultiResolutionImageRegistrationMethodTest_1 );
REGISTER_TEST(itkMultiResolutionImageRegistrationMethodTest_2 );
REGISTER_TEST(itkMutualInformationMetricTest );
REGISTER_TEST(itkNewTest );
REGISTER_TEST(itkNormalizedCorrelationImageMetricTest );
REGISTER_TEST(itkNormalizedCorrelationPointSetToImageMetricTest );
REGISTER_TEST(itkOtsuThresholdImageCalculatorTest );
REGISTER_TEST(itkMeanReciprocalSquareDifferenceImageMetricTest  );
REGISTER_TEST(itkPointSetToImageRegistrationTest_1  );
REGISTER_TEST(itkRegionGrow2DTest );
REGISTER_TEST(itkReinitializeLevelSetImageFilterTest );
REGISTER_TEST(itkShapeDetectionLevelSetImageFilterTest );
REGISTER_TEST(itkShapePriorMAPCostFunctionTest );
REGISTER_TEST(itkShapePriorSegmentationLevelSetFunctionTest );
REGISTER_TEST(itkSpatialObjectToImageRegistrationTest );
REGISTER_TEST(itkSupervisedImageClassifierTest);
REGISTER_TEST(itkGibbsTest );
REGISTER_TEST(itkDeformableTest );
REGISTER_TEST(itk2DDeformableTest );
REGISTER_TEST(itkSphereMeshSourceTest );
REGISTER_TEST(itkThresholdSegmentationLevelSetImageFilterTest );
REGISTER_TEST(itkVectorFuzzyConnectednessImageFilterTest );
REGISTER_TEST(itkVoronoiDiagram2DTest );
REGISTER_TEST(itkVoronoiSegmentationImageFilterTest );
REGISTER_TEST(itkWatershedImageFilterTest );
}

