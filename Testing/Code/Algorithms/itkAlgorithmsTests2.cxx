// this file defines the itkBasicFiltersTest for the test driver
// and all it expects is that you have a function called RegisterTests

#include "itkTestMain.h" 


void RegisterTests()
{
REGISTER_TEST(itkVectorFuzzyConnectednessImageFilterTest );
REGISTER_TEST(itkVoronoiDiagram2DTest );
REGISTER_TEST(itkVoronoiSegmentationImageFilterTest );
REGISTER_TEST(itkVoronoiPartitioningImageFilterTest );
REGISTER_TEST(itkWatershedImageFilterTest );
}

