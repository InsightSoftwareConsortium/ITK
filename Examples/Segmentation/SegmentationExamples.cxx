// this file defines the SegmentationExamples for the test driver
// and all it expects is that you have a function called RegisterTests
#include <iostream>
#include "itkTestMain.h" 


void RegisterTests()
{
REGISTER_TEST(ConfidenceConnectedTest);
REGISTER_TEST(ConnectedThresholdImageFilterTest);
REGISTER_TEST(DeformableModel1Test);
REGISTER_TEST(FastMarchingImageFilterTest);
REGISTER_TEST(GeodesicActiveContourImageFilterTest);
REGISTER_TEST(GibbsPriorImageFilter1Test);
REGISTER_TEST(HybridSegmentationFuzzyVoronoiTest);
REGISTER_TEST(IsolatedConnectedImageFilterTest);
REGISTER_TEST(NeighborhoodConnectedImageFilterTest);
REGISTER_TEST(ShapeDetectionLevelSetFilterTest);
REGISTER_TEST(WatershedSegmentation1Test);
}

#undef main
#define main ConfidenceConnectedTest
#include "ConfidenceConnected.cxx"

#undef main
#define main ConnectedThresholdImageFilterTest
#include "ConnectedThresholdImageFilter.cxx"

#undef main
#define main DeformableModel1Test
#include "DeformableModel1.cxx"

#undef main
#define main FastMarchingImageFilterTest
#include "FastMarchingImageFilter.cxx"

#undef main
#define main GeodesicActiveContourImageFilterTest
#include "GeodesicActiveContourImageFilter.cxx"

#undef main
#define main GibbsPriorImageFilter1Test
#include "GibbsPriorImageFilter1.cxx"

#undef main
#define main HybridSegmentationFuzzyVoronoiTest
#include "HybridSegmentationFuzzyVoronoi.cxx"

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
#define main WatershedSegmentation1Test
#include "WatershedSegmentation1.cxx"
