// this file defines the SegmentationExamples for the test driver
// and all it expects is that you have a function called RegisterTests
#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif
#include <iostream>
#include "itkTestMain.h" 


void RegisterTests()
{
REGISTER_TEST(CellularSegmentation1Test);
REGISTER_TEST(GeodesicActiveContourShapePriorLevelSetImageFilterTest);
REGISTER_TEST(WatershedSegmentation1Test);
}

#undef main
#define main GeodesicActiveContourShapePriorLevelSetImageFilterTest
#include "GeodesicActiveContourShapePriorLevelSetImageFilter.cxx"

#undef main
#define main WatershedSegmentation1Test
#include "WatershedSegmentation1.cxx"

#undef main
#define main CellularSegmentation1Test
#include "CellularSegmentation1.cxx"

