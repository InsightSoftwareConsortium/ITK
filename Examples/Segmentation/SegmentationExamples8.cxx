// this file defines the SegmentationExamples for the test driver
// and all it expects is that you have a function called RegisterTests
#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif
#include <iostream>
#include "itkTestMain.h" 


void RegisterTests()
{
REGISTER_TEST(VectorConfidenceConnectedTest);
}

#undef main
#define main VectorConfidenceConnectedTest
#include "VectorConfidenceConnected.cxx"
