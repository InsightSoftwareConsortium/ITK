// this file defines the FilterExamples for the test driver
// and all it expects is that you have a function called RegisterTests
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif
#include <iostream>
#include "itkTestMain.h" 


void RegisterTests()
{
REGISTER_TEST(BinaryMedianImageFilterTest);
REGISTER_TEST(LaplacianSharpeningImageFilterTest);
REGISTER_TEST(ResampleImageFilterTest);
REGISTER_TEST(ResampleOrientedImageFilterTest);
REGISTER_TEST(ResampleImageFilter2Test);
REGISTER_TEST(ResampleImageFilter3Test);
REGISTER_TEST(ResampleImageFilter4Test);
REGISTER_TEST(ResampleImageFilter5Test);
REGISTER_TEST(ResampleImageFilter8Test);
REGISTER_TEST(ResampleImageFilter9Test);
REGISTER_TEST(SigmoidImageFilterTest);
REGISTER_TEST(SmoothingRecursiveGaussianImageFilterTest);
REGISTER_TEST(SmoothingRecursiveGaussianImageFilter2Test);
REGISTER_TEST(ThresholdImageFilterTest);
REGISTER_TEST(VectorCurvatureAnisotropicDiffusionImageFilterTest);
REGISTER_TEST(VectorGradientAnisotropicDiffusionImageFilterTest);
REGISTER_TEST(VectorIndexSelectionTest);
REGISTER_TEST(VotingBinaryHoleFillingImageFilterTest);
REGISTER_TEST(VotingBinaryIterativeHoleFillingImageFilterTest);
}
#undef main
#define main BinaryMedianImageFilterTest
#include "BinaryMedianImageFilter.cxx"

#undef main
#define main ResampleImageFilterTest
#include "ResampleImageFilter.cxx"

#undef main
#define main ResampleOrientedImageFilterTest
#include "ResampleOrientedImageFilter.cxx"

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
#define main ResampleImageFilter8Test
#include "ResampleImageFilter8.cxx"

#undef main
#define main ResampleImageFilter9Test
#include "ResampleImageFilter9.cxx"

#undef main
#define main LaplacianSharpeningImageFilterTest
#include "LaplacianSharpeningImageFilter.cxx"

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
#define main VectorIndexSelectionTest
#include "VectorIndexSelection.cxx"

#undef main
#define main VotingBinaryHoleFillingImageFilterTest
#include "VotingBinaryHoleFillingImageFilter.cxx"

#undef main
#define main VotingBinaryIterativeHoleFillingImageFilterTest
#include "VotingBinaryIterativeHoleFillingImageFilter.cxx"
