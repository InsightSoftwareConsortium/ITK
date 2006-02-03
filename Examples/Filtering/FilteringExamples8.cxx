// this file defines the FilterExamples for the test driver
// and all it expects is that you have a function called RegisterTests
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#ifdef __BORLANDC__
#define ITK_LEAN_AND_MEAN
#endif

#include <iostream>
#include "itkTestMain.h" 


void RegisterTests()
{
REGISTER_TEST(GradientAnisotropicDiffusionImageFilterTest);
REGISTER_TEST(GradientMagnitudeImageFilterTest);
REGISTER_TEST(GradientMagnitudeRecursiveGaussianImageFilterTest);
REGISTER_TEST(LaplacianImageFilterTest);
REGISTER_TEST(MathematicalMorphologyBinaryFiltersTest);
REGISTER_TEST(MathematicalMorphologyGrayscaleFiltersTest);
REGISTER_TEST(MeanImageFilterTest);
REGISTER_TEST(MedianImageFilterTest);
REGISTER_TEST(MinMaxCurvatureFlowImageFilterTest);
REGISTER_TEST(RGBCurvatureAnisotropicDiffusionImageFilterTest);
REGISTER_TEST(RGBGradientAnisotropicDiffusionImageFilterTest);
}

#undef main
#define main GradientAnisotropicDiffusionImageFilterTest
#include "GradientAnisotropicDiffusionImageFilter.cxx"

#undef main
#define main GradientMagnitudeImageFilterTest
#include "GradientMagnitudeImageFilter.cxx"

#undef main
#define main GradientMagnitudeRecursiveGaussianImageFilterTest
#include "GradientMagnitudeRecursiveGaussianImageFilter.cxx"

#undef main
#define main LaplacianImageFilterTest
#include "LaplacianImageFilter.cxx"

#undef main
#define main MathematicalMorphologyBinaryFiltersTest
#include "MathematicalMorphologyBinaryFilters.cxx"

#undef main
#define main MathematicalMorphologyGrayscaleFiltersTest
#include "MathematicalMorphologyGrayscaleFilters.cxx"

#undef main
#define main MeanImageFilterTest
#include "MeanImageFilter.cxx"

#undef main
#define main MedianImageFilterTest
#include "MedianImageFilter.cxx"

#undef main
#define main MinMaxCurvatureFlowImageFilterTest
#include "MinMaxCurvatureFlowImageFilter.cxx"

#undef main
#define main RGBCurvatureAnisotropicDiffusionImageFilterTest
#include "RGBCurvatureAnisotropicDiffusionImageFilter.cxx"

#undef main
#define main RGBGradientAnisotropicDiffusionImageFilterTest
#include "RGBGradientAnisotropicDiffusionImageFilter.cxx"


