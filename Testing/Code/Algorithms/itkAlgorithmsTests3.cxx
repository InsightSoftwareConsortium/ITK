// this file defines the itkBasicFiltersTest for the test driver
// and all it expects is that you have a function called RegisterTests

#include "vnl/vnl_sample.h"
#include "itkTestMain.h" 


void RegisterTests()
{
  vnl_sample_reseed(8775070);
  REGISTER_TEST(itk2DDeformableTest );
  REGISTER_TEST(itkDeformableTest );
  REGISTER_TEST(itkGibbsTest );
  REGISTER_TEST(itkMRFImageFilterTest );
  REGISTER_TEST(itkMRIBiasFieldCorrectionFilterTest );
  REGISTER_TEST(itkMattesMutualInformationImageToImageMetricTest );
  REGISTER_TEST(itkMeanReciprocalSquareDifferenceImageMetricTest  );
  REGISTER_TEST(itkMeanSquaresHistogramImageToImageMetricTest );
  REGISTER_TEST(itkMeanSquaresImageMetricTest );
  REGISTER_TEST(itkMeanSquaresPointSetToImageMetricTest );
  REGISTER_TEST(itkMinMaxCurvatureFlowImageFilterTest );
  REGISTER_TEST(itkMultiResolutionImageRegistrationMethodTest);
  REGISTER_TEST(itkMultiResolutionImageRegistrationMethodTest_1 );
  REGISTER_TEST(itkMultiResolutionImageRegistrationMethodTest_2 );
  REGISTER_TEST(itkMultiResolutionPDEDeformableRegistrationTest );
  REGISTER_TEST(itkMultiResolutionPyramidImageFilterTest );
  REGISTER_TEST(itkMutualInformationHistogramImageToImageMetricTest );
  REGISTER_TEST(itkMutualInformationMetricTest );
  REGISTER_TEST(itkNewTest );
  REGISTER_TEST(itkNormalizedCorrelationImageMetricTest );
  REGISTER_TEST(itkNormalizedCorrelationPointSetToImageMetricTest );
  REGISTER_TEST(itkNormalizedMutualInformationHistogramImageToImageMetricTest );
  REGISTER_TEST(itkOrthogonalSwath2DPathFilterTest );
  REGISTER_TEST(itkOtsuThresholdImageCalculatorTest );
  REGISTER_TEST(itkOtsuThresholdImageFilterTest );
  REGISTER_TEST(itkPointSetToImageRegistrationTest_1  );
  REGISTER_TEST(itkRecursiveMultiResolutionPyramidImageFilterTest );
  REGISTER_TEST(itkRegionGrow2DTest );
  REGISTER_TEST(itkReinitializeLevelSetImageFilterTest );
  REGISTER_TEST(itkShapeDetectionLevelSetImageFilterTest );
  REGISTER_TEST(itkShapePriorMAPCostFunctionTest );
  REGISTER_TEST(itkShapePriorSegmentationLevelSetFunctionTest );
  REGISTER_TEST(itkSpatialObjectToImageRegistrationTest );
  REGISTER_TEST(itkSphereMeshSourceTest );
  REGISTER_TEST(itkSupervisedImageClassifierTest);
  REGISTER_TEST(itkSwathChainCodePathFilterTest);
  REGISTER_TEST(itkUnsharpMaskLevelSetImageFilterTest );
}

