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
REGISTER_TEST(BilateralImageFilterTest);
REGISTER_TEST(BinaryMinMaxCurvatureFlowImageFilterTest);
REGISTER_TEST(BinaryThresholdImageFilterTest);
REGISTER_TEST(BinomialBlurImageFilterTest);
REGISTER_TEST(CastingImageFiltersTest);
REGISTER_TEST(CurvatureAnisotropicDiffusionImageFilterTest);
REGISTER_TEST(CurvatureFlowImageFilterTest);
REGISTER_TEST(DanielssonDistanceMapImageFilterTest);
REGISTER_TEST(DerivativeImageFilterTest);
REGISTER_TEST(DiscreteGaussianImageFilterTest);
}

#undef main
#define main BilateralImageFilterTest
#include "BilateralImageFilter.cxx"

#undef main
#define main BinaryMinMaxCurvatureFlowImageFilterTest
#include "BinaryMinMaxCurvatureFlowImageFilter.cxx"

#undef main
#define main BinaryThresholdImageFilterTest
#include "BinaryThresholdImageFilter.cxx"

#undef main
#define main BinomialBlurImageFilterTest
#include "BinomialBlurImageFilter.cxx"

#undef main
#define main CastingImageFiltersTest
#include "CastingImageFilters.cxx"

#undef main
#define main CurvatureAnisotropicDiffusionImageFilterTest
#include "CurvatureAnisotropicDiffusionImageFilter.cxx"

#undef main
#define main CurvatureFlowImageFilterTest
#include "CurvatureFlowImageFilter.cxx"

#undef main
#define main DanielssonDistanceMapImageFilterTest
#include "DanielssonDistanceMapImageFilter.cxx"

#undef main
#define main DerivativeImageFilterTest
#include "DerivativeImageFilter.cxx"

#undef main
#define main DiscreteGaussianImageFilterTest
#include "DiscreteGaussianImageFilter.cxx"
