// this file defines the SegmentationExamples for the test driver
// and all it expects is that you have a function called RegisterTests
#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif
#include <iostream>
#include "itkTestMain.h" 


void RegisterTests()
{
REGISTER_TEST(CannySegmentationLevelSetImageFilterTest);
REGISTER_TEST(ConfidenceConnectedTest);
REGISTER_TEST(ConnectedThresholdImageFilterTest);
REGISTER_TEST(FastMarchingImageFilterTest);
REGISTER_TEST(GeodesicActiveContourImageFilterTest);
REGISTER_TEST(GibbsPriorImageFilter1Test);
}

#undef main
#define main CannySegmentationLevelSetImageFilterTest
#include "CannySegmentationLevelSetImageFilter.cxx"

#undef main
#define main ConfidenceConnectedTest
#include "ConfidenceConnected.cxx"

#undef main
#define main ConnectedThresholdImageFilterTest
#include "ConnectedThresholdImageFilter.cxx"

#undef main
#define main FastMarchingImageFilterTest
#include "FastMarchingImageFilter.cxx"

#undef main
#define main GeodesicActiveContourImageFilterTest
#include "GeodesicActiveContourImageFilter.cxx"

#undef main
#define main GibbsPriorImageFilter1Test
#include "GibbsPriorImageFilter1.cxx"
