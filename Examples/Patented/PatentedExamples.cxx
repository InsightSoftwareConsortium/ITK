// this file defines the SegmentationExamples for the test driver
// and all it expects is that you have a function called RegisterTests
#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif
#include <iostream>
#include "itkTestMain.h" 


void RegisterTests()
{
REGISTER_TEST(FuzzyConnectednessImageFilterTest);
REGISTER_TEST(IterativeClosestPoint1Test);
REGISTER_TEST(IterativeClosestPoint2Test);
REGISTER_TEST(IterativeClosestPoint3Test);
}

#undef main
#define main FuzzyConnectednessImageFilterTest
#include "FuzzyConnectednessImageFilter.cxx"

#undef main
#define main  IterativeClosestPoint1Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate12
#include "IterativeClosestPoint1.cxx"

#undef main
#define main  IterativeClosestPoint2Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate13
#include "IterativeClosestPoint2.cxx"

#undef main
#define main  IterativeClosestPoint3Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate14
#include "IterativeClosestPoint3.cxx"

