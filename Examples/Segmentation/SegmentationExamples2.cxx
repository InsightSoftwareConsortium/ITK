// this file defines the SegmentationExamples for the test driver
// and all it expects is that you have a function called RegisterTests
#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif
#include <iostream>
#include "itkTestMain.h" 


void RegisterTests()
{
REGISTER_TEST(HoughTransform2DCirclesImageFilterTest);
REGISTER_TEST(HoughTransform2DLinesImageFilterTest);
REGISTER_TEST(IsolatedConnectedImageFilterTest);
REGISTER_TEST(NeighborhoodConnectedImageFilterTest);
REGISTER_TEST(ShapeDetectionLevelSetFilterTest);
REGISTER_TEST(ThresholdSegmentationLevelSetImageFilterTest);
REGISTER_TEST(WatershedSegmentation1Test);
}

#undef main
#define main HoughTransform2DLinesImageFilterTest
#include "HoughTransform2DLinesImageFilter.cxx"

#undef main
#define main HoughTransform2DCirclesImageFilterTest
#include "HoughTransform2DCirclesImageFilter.cxx"

#undef main
#define main IsolatedConnectedImageFilterTest
#include "IsolatedConnectedImageFilter.cxx"

#undef main
#define main NeighborhoodConnectedImageFilterTest
#include "NeighborhoodConnectedImageFilter.cxx"

#undef main
#define main ShapeDetectionLevelSetFilterTest
#include "ShapeDetectionLevelSetFilter.cxx"

#undef main
#define main ThresholdSegmentationLevelSetImageFilterTest
#include "ThresholdSegmentationLevelSetImageFilter.cxx"

#undef main
#define main WatershedSegmentation1Test
#include "WatershedSegmentation1.cxx"
