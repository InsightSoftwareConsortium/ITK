// this file defines the itkStatisticsTest for the test driver
// and all it expects is that you have a function called RegisterTests
#include <iostream>
#include "itkTestMain.h" 


void RegisterTests()
{
  REGISTER_TEST(itkAffineGeometryFrameTest);
  REGISTER_TEST(itkArrowSpatialObjectTest);
  REGISTER_TEST(itkBlobSpatialObjectTest);
  REGISTER_TEST(itkEllipseSpatialObjectTest);
  REGISTER_TEST(itkGaussianSpatialObjectTest);
  REGISTER_TEST(itkImageMaskSpatialObjectTest);
  REGISTER_TEST(itkImageSpatialObjectTest);
  REGISTER_TEST(itkLandmarkSpatialObjectTest);
  REGISTER_TEST(itkLineSpatialObjectTest);
}
