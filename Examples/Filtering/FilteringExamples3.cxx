// this file defines the FilterExamples for the test driver
// and all it expects is that you have a function called RegisterTests
#include <iostream>
#include "itkTestMain.h" 


void RegisterTests()
{
REGISTER_TEST(MorphologicalImageEnhancementTest);
REGISTER_TEST(OtsuThresholdImageFilterTest);
REGISTER_TEST(OtsuMultipleThresholdImageFilterTest);
REGISTER_TEST(AntiAliasBinaryImageFilterTest);
REGISTER_TEST(CannyEdgeDetectionImageFilterTest);
REGISTER_TEST(FlipImageFilterTest);
REGISTER_TEST(GaussianBlurImageFunctionTest);
REGISTER_TEST(FFTImageFilterTest);
 REGISTER_TEST(RGBToGrayscaleTest);
}

#undef main
#define main MorphologicalImageEnhancementTest
#include "MorphologicalImageEnhancement.cxx"

#undef main
#define main OtsuThresholdImageFilterTest
#include "OtsuThresholdImageFilter.cxx"

#undef main
#define main OtsuMultipleThresholdImageFilterTest
#include "OtsuMultipleThresholdImageFilter.cxx"

#undef main
#define main AntiAliasBinaryImageFilterTest
#include "AntiAliasBinaryImageFilter.cxx"

#undef main
#define main CannyEdgeDetectionImageFilterTest
#include "CannyEdgeDetectionImageFilter.cxx"

#undef main
#define main FlipImageFilterTest
#include "FlipImageFilter.cxx"

#undef main
#define main GaussianBlurImageFunctionTest
#include "GaussianBlurImageFunction.cxx"

#undef main
#define main FFTImageFilterTest
#include "FFTImageFilter.cxx"

#undef main
#define main RGBToGrayscaleTest
#include "RGBToGrayscale.cxx"
