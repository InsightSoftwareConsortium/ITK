// this file defines the itkStatisticsTest for the test driver
// and all it expects is that you have a function called RegisterTests
#include <iostream>
#include "itkTestMain.h" 


void RegisterTests()
{
  REGISTER_TEST(itkSpatialObjectPrintTest);
  REGISTER_TEST(itkImageSpatialObjectTest);
  REGISTER_TEST(itkTubeSpatialObjectTest);
  REGISTER_TEST(itkBlobSpatialObjectTest);
  REGISTER_TEST(itkLandmarkSpatialObjectTest);
  REGISTER_TEST(itkLineSpatialObjectTest);
  REGISTER_TEST(itkEllipseSpatialObjectTest);
  REGISTER_TEST(itkSurfaceSpatialObjectTest);
  REGISTER_TEST(itkPolygonGroupSpatialObjectTest);
}
