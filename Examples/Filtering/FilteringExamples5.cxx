// this file defines the FilterExamples for the test driver
// and all it expects is that you have a function called RegisterTests
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif
#include <iostream>
#include "itkTestMain.h" 


void RegisterTests()
{
REGISTER_TEST(OtsuThresholdImageFilterTest);
REGISTER_TEST(OtsuMultipleThresholdImageFilterTest);
}

#undef main
#define main OtsuThresholdImageFilterTest
#include "OtsuThresholdImageFilter.cxx"

#undef main
#define main OtsuMultipleThresholdImageFilterTest
#include "OtsuMultipleThresholdImageFilter.cxx"
