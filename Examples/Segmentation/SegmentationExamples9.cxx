// this file defines the SegmentationExamples for the test driver
// and all it expects is that you have a function called RegisterTests
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif
#ifdef __BORLANDC__
#define ITK_LEAN_AND_MEAN
#endif

#include "itkTestMain.h" 
#include <iostream>


void RegisterTests()
{
REGISTER_TEST(CellularSegmentation1Test);
}

#undef main
#define main CellularSegmentation1Test
#include "CellularSegmentation1.cxx"

