// this file defines the SegmentationExamples for the test driver
// and all it expects is that you have a function called RegisterTests
#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif
#include <iostream>
#include "itkTestMain.h" 


void RegisterTests()
{
REGISTER_TEST(WatershedSegmentation1Test);
}

#undef main
#define main WatershedSegmentation1Test
#include "WatershedSegmentation1.cxx"
