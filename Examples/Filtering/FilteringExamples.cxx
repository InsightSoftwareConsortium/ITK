// this file defines the FilterExamples for the test driver
// and all it expects is that you have a function called RegisterTests
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
REGISTER_TEST(ResampleImageFilterTest);
REGISTER_TEST(ResampleImageFilter2Test);
REGISTER_TEST(ResampleImageFilter3Test);
REGISTER_TEST(ResampleImageFilter4Test);
REGISTER_TEST(ResampleImageFilter5Test);
REGISTER_TEST(SigmoidImageFilterTest);
REGISTER_TEST(SmoothingRecursiveGaussianImageFilterTest);
REGISTER_TEST(ThresholdImageFilterTest);
REGISTER_TEST(VectorCurvatureAnisotropicDiffusionImageFilterTest);
REGISTER_TEST(VectorGradientAnisotropicDiffusionImageFilterTest);
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

#undef main
#define main ResampleImageFilterTest
#include "ResampleImageFilter.cxx"

#undef main
#define main ResampleImageFilter2Test
#include "ResampleImageFilter2.cxx"

#undef main
#define main ResampleImageFilter3Test
#include "ResampleImageFilter3.cxx"

#undef main
#define main ResampleImageFilter4Test
#include "ResampleImageFilter4.cxx"

#undef main
#define main ResampleImageFilter5Test
#include "ResampleImageFilter5.cxx"

#undef main
#define main SigmoidImageFilterTest
#include "SigmoidImageFilter.cxx"

#undef main
#define main SmoothingRecursiveGaussianImageFilterTest
#include "SmoothingRecursiveGaussianImageFilter.cxx"

#undef main
#define main ThresholdImageFilterTest
#include "ThresholdImageFilter.cxx"

#undef main
#define main VectorCurvatureAnisotropicDiffusionImageFilterTest
#include "VectorCurvatureAnisotropicDiffusionImageFilter.cxx"

#undef main
#define main VectorGradientAnisotropicDiffusionImageFilterTest
#include "VectorGradientAnisotropicDiffusionImageFilter.cxx"
