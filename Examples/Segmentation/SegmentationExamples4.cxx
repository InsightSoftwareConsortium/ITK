// this file defines the SegmentationExamples for the test driver
// and all it expects is that you have a function called RegisterTests
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif
#include "itkTestMain.h" 
#include <iostream>


void RegisterTests()
{
REGISTER_TEST(CurvesLevelSetImageFilterTest);
}

#undef main
#define main CurvesLevelSetImageFilterTest
#include "CurvesLevelSetImageFilter.cxx"

