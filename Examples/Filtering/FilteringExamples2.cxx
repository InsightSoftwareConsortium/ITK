// this file defines the FilterExamples for the test driver
// and all it expects is that you have a function called RegisterTests
#include <iostream>
#include "itkTestMain.h" 


void RegisterTests()
{
REGISTER_TEST(SigmoidImageFilterTest);
REGISTER_TEST(SmoothingRecursiveGaussianImageFilterTest);
REGISTER_TEST(SmoothingRecursiveGaussianImageFilter2Test);
REGISTER_TEST(ThresholdImageFilterTest);
REGISTER_TEST(VectorCurvatureAnisotropicDiffusionImageFilterTest);
REGISTER_TEST(VectorGradientAnisotropicDiffusionImageFilterTest);
REGISTER_TEST(ResampleVolumesToBeIsotropicTest);
}
#undef main
#define main SigmoidImageFilterTest
#include "SigmoidImageFilter.cxx"

#undef main
#define main SmoothingRecursiveGaussianImageFilterTest
#include "SmoothingRecursiveGaussianImageFilter.cxx"

#undef main
#define main SmoothingRecursiveGaussianImageFilter2Test
#include "SmoothingRecursiveGaussianImageFilter2.cxx"

#undef main
#define main ThresholdImageFilterTest
#include "ThresholdImageFilter.cxx"

#undef main
#define main VectorCurvatureAnisotropicDiffusionImageFilterTest
#include "VectorCurvatureAnisotropicDiffusionImageFilter.cxx"

#undef main
#define main VectorGradientAnisotropicDiffusionImageFilterTest
#include "VectorGradientAnisotropicDiffusionImageFilter.cxx"

#undef main
#define main ResampleVolumesToBeIsotropicTest
#include "ResampleVolumesToBeIsotropic.cxx"
