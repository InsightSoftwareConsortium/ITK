// this file defines the itkStatisticsTest for the test driver
// and all it expects is that you have a function called RegisterTests
#include <iostream>
#include "itkTestMain.h" 


void RegisterTests()
{
  REGISTER_TEST(itkMeshSpatialObjectTest);
  REGISTER_TEST(itkPlaneSpatialObjectTest);
  REGISTER_TEST(itkPolygonGroupSpatialObjectTest);
  REGISTER_TEST(itkSceneSpatialObjectTest);
  REGISTER_TEST(itkSpatialObjectPrintTest);
  REGISTER_TEST(itkSpatialObjectTreeContainerTest);
  REGISTER_TEST(itkSurfaceSpatialObjectTest);
  REGISTER_TEST(itkTubeSpatialObjectTest);
}
