// this file defines the SegmentationExamples for the test driver
// and all it expects is that you have a function called RegisterTests
#include "itkTestMain.h" 
#include <iostream>


void RegisterTests()
{
REGISTER_TEST(CellularSegmentation1Test);
REGISTER_TEST(CurvesLevelSetImageFilterTest);
}

#undef main
#define main CellularSegmentation1Test
#include "CellularSegmentation1.cxx"

#undef main
#define main CurvesLevelSetImageFilterTest
#include "CurvesLevelSetImageFilter.cxx"

