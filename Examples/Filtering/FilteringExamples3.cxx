// this file defines the FilterExamples for the test driver
// and all it expects is that you have a function called RegisterTests
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif
#include <iostream>
#include "itkTestMain.h" 


void RegisterTests()
{
REGISTER_TEST(MorphologicalImageEnhancementTest);
REGISTER_TEST(AntiAliasBinaryImageFilterTest);
REGISTER_TEST(CannyEdgeDetectionImageFilterTest);
REGISTER_TEST(FlipImageFilterTest);
REGISTER_TEST(GaussianBlurImageFunctionTest);
REGISTER_TEST(FFTImageFilterTest);
REGISTER_TEST(RGBToGrayscaleTest);
REGISTER_TEST(LaplacianRecursiveGaussianImageFilter1Test);
REGISTER_TEST(LaplacianRecursiveGaussianImageFilter2Test);
}

#undef main
#define main MorphologicalImageEnhancementTest
#include "MorphologicalImageEnhancement.cxx"

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

#undef main
#define main LaplacianRecursiveGaussianImageFilter1Test
#include "LaplacianRecursiveGaussianImageFilter1.cxx"

#undef main
#define main LaplacianRecursiveGaussianImageFilter2Test
#include "LaplacianRecursiveGaussianImageFilter2.cxx"
