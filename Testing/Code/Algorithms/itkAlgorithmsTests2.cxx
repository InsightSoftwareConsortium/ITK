// this file defines the itkBasicFiltersTest for the test driver
// and all it expects is that you have a function called RegisterTests

#include "vnl/vnl_sample.h"
#include "itkTestMain.h" 


void RegisterTests()
{
  vnl_sample_reseed(8775070);
  REGISTER_TEST(itkNarrowBandThresholdSegmentationLevelSetImageFilterTest );
  REGISTER_TEST(itkThresholdSegmentationLevelSetImageFilterTest );
  REGISTER_TEST(itkCompareHistogramImageToImageMetricTest   );
  REGISTER_TEST(itkDeformableSimplexMesh3DFilterTest   );
  REGISTER_TEST(itkDeformableSimplexMesh3DBalloonForceFilterTest  );
  REGISTER_TEST(itkGradientDifferenceImageToImageMetricTest );
  REGISTER_TEST(itkKappaStatisticImageToImageMetricTest );
  REGISTER_TEST(itkKullbackLeiblerCompareHistogramImageToImageMetricTest );
  REGISTER_TEST(itkImagePCAShapeModelEstimatorTest);
  REGISTER_TEST(itkImagePCADecompositionCalculatorTest);
  REGISTER_TEST(itkIsolatedWatershedImageFilterTest );
  REGISTER_TEST(itkMIRegistrationFunctionTest );
  REGISTER_TEST(itkMatchCardinalityImageToImageMetricTest );
  REGISTER_TEST(itkOtsuMultipleThresholdsCalculatorTest );
  REGISTER_TEST(itkOtsuMultipleThresholdsImageFilterTest );
  REGISTER_TEST(itkPointSetToPointSetRegistrationTest );
  REGISTER_TEST(itkPointSetToSpatialObjectDemonsRegistrationTest );
  REGISTER_TEST(itkRegularSphereMeshSourceTest );
  REGISTER_TEST(itkRayCastInterpolateImageFunctionTest );
  REGISTER_TEST(itkSimplexMeshVolumeCalculatorTest );
  REGISTER_TEST(itkSTAPLEImageFilterTest );
  REGISTER_TEST(itkSymmetricForcesDemonsRegistrationFilterTest );
  REGISTER_TEST(itkVoronoiDiagram2DTest );
  REGISTER_TEST(itkVoronoiSegmentationImageFilterTest );
}

