// this file defines the FilterExamples for the test driver
// and all it expects is that you have a function called RegisterTests
#include <iostream>
#include "itkTestMain.h" 


void RegisterTests()
{
REGISTER_TEST(BilateralImageFilter);
REGISTER_TEST(BinaryMinMaxCurvatureFlowImageFilter);
REGISTER_TEST(BinaryThresholdImageFilter);
REGISTER_TEST(BinomialBlurImageFilter);
REGISTER_TEST(CastingImageFilters);
REGISTER_TEST(CurvatureAnisotropicDiffusionImageFilter);
REGISTER_TEST(CurvatureFlowImageFilter);
REGISTER_TEST(DanielssonDistanceMapImageFilter);
REGISTER_TEST(DiscreteGaussianImageFilter);
REGISTER_TEST(GradientAnisotropicDiffusionImageFilter);
REGISTER_TEST(GradientMagnitudeImageFilter);
REGISTER_TEST(GradientMagnitudeRecursiveGaussianImageFilter);
REGISTER_TEST(LaplacianImageFilter);
REGISTER_TEST(MathematicalMorphologyBinaryFilters);
REGISTER_TEST(MathematicalMorphologyGrayscaleFilters);
REGISTER_TEST(MeanImageFilter);
REGISTER_TEST(MedianImageFilter);
REGISTER_TEST(MinMaxCurvatureFlowImageFilter);
REGISTER_TEST(RGBCurvatureAnisotropicDiffusionImageFilter);
REGISTER_TEST(RGBGradientAnisotropicDiffusionImageFilter);
REGISTER_TEST(ResampleImageFilter);
REGISTER_TEST(ResampleImageFilter2);
REGISTER_TEST(ResampleImageFilter3);
REGISTER_TEST(ResampleImageFilter4);
REGISTER_TEST(ResampleImageFilter5);
REGISTER_TEST(SigmoidImageFilter);
REGISTER_TEST(SmoothingRecursiveGaussianImageFilter);
REGISTER_TEST(ThresholdImageFilter);
REGISTER_TEST(VectorCurvatureAnisotropicDiffusionImageFilter);
REGISTER_TEST(VectorGradientAnisotropicDiffusionImageFilter);
}
#undef main
#define main BilateralImageFilter
#include "BilateralImageFilter.cxx"

#undef main
#define main BinaryMinMaxCurvatureFlowImageFilter
#include "BinaryMinMaxCurvatureFlowImageFilter.cxx"

#undef main
#define main BinaryThresholdImageFilter
#include "BinaryThresholdImageFilter.cxx"

#undef main
#define main BinomialBlurImageFilter
#include "BinomialBlurImageFilter.cxx"

#undef main
#define main CastingImageFilters
#include "CastingImageFilters.cxx"

#undef main
#define main CurvatureAnisotropicDiffusionImageFilter
#include "CurvatureAnisotropicDiffusionImageFilter.cxx"

#undef main
#define main CurvatureFlowImageFilter
#include "CurvatureFlowImageFilter.cxx"

#undef main
#define main DanielssonDistanceMapImageFilter
#include "DanielssonDistanceMapImageFilter.cxx"

#undef main
#define main DiscreteGaussianImageFilter
#include "DiscreteGaussianImageFilter.cxx"

#undef main
#define main GradientAnisotropicDiffusionImageFilter
#include "GradientAnisotropicDiffusionImageFilter.cxx"

#undef main
#define main GradientMagnitudeImageFilter
#include "GradientMagnitudeImageFilter.cxx"

#undef main
#define main GradientMagnitudeRecursiveGaussianImageFilter
#include "GradientMagnitudeRecursiveGaussianImageFilter.cxx"

#undef main
#define main LaplacianImageFilter
#include "LaplacianImageFilter.cxx"

#undef main
#define main MathematicalMorphologyBinaryFilters
#include "MathematicalMorphologyBinaryFilters.cxx"

#undef main
#define main MathematicalMorphologyGrayscaleFilters
#include "MathematicalMorphologyGrayscaleFilters.cxx"

#undef main
#define main MeanImageFilter
#include "MeanImageFilter.cxx"

#undef main
#define main MedianImageFilter
#include "MedianImageFilter.cxx"

#undef main
#define main MinMaxCurvatureFlowImageFilter
#include "MinMaxCurvatureFlowImageFilter.cxx"

#undef main
#define main RGBCurvatureAnisotropicDiffusionImageFilter
#include "RGBCurvatureAnisotropicDiffusionImageFilter.cxx"

#undef main
#define main RGBGradientAnisotropicDiffusionImageFilter
#include "RGBGradientAnisotropicDiffusionImageFilter.cxx"

#undef main
#define main ResampleImageFilter
#include "ResampleImageFilter.cxx"

#undef main
#define main ResampleImageFilter2
#include "ResampleImageFilter2.cxx"

#undef main
#define main ResampleImageFilter3
#include "ResampleImageFilter3.cxx"

#undef main
#define main ResampleImageFilter4
#include "ResampleImageFilter4.cxx"

#undef main
#define main ResampleImageFilter5
#include "ResampleImageFilter5.cxx"

#undef main
#define main SigmoidImageFilter
#include "SigmoidImageFilter.cxx"

#undef main
#define main SmoothingRecursiveGaussianImageFilter
#include "SmoothingRecursiveGaussianImageFilter.cxx"

#undef main
#define main ThresholdImageFilter
#include "ThresholdImageFilter.cxx"

#undef main
#define main VectorCurvatureAnisotropicDiffusionImageFilter
#include "VectorCurvatureAnisotropicDiffusionImageFilter.cxx"

#undef main
#define main VectorGradientAnisotropicDiffusionImageFilter
#include "VectorGradientAnisotropicDiffusionImageFilter.cxx"
