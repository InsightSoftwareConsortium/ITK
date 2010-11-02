/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
// this file defines the FilterExamples for the test driver
// and all it expects is that you have a function called RegisterTests
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <iostream>
#include "itkTestMain.h"


void RegisterTests()
{
  REGISTER_TEST(GradientAnisotropicDiffusionImageFilterTest);
  REGISTER_TEST(GradientMagnitudeImageFilterTest);
  REGISTER_TEST(GradientMagnitudeRecursiveGaussianImageFilterTest);
  REGISTER_TEST(GradientRecursiveGaussianImageFilterTest);
  REGISTER_TEST(GradientVectorFlowImageFilterTest);
  REGISTER_TEST(LaplacianImageFilterTest);
  REGISTER_TEST(MathematicalMorphologyBinaryFiltersTest);
  REGISTER_TEST(MathematicalMorphologyGrayscaleFiltersTest);
  REGISTER_TEST(MeanImageFilterTest);
  REGISTER_TEST(MedianImageFilterTest);
  REGISTER_TEST(MinMaxCurvatureFlowImageFilterTest);
  REGISTER_TEST(RGBCurvatureAnisotropicDiffusionImageFilterTest);
  REGISTER_TEST(RGBGradientAnisotropicDiffusionImageFilterTest);
  REGISTER_TEST(ZeroCrossingBasedEdgeDetectionImageFilterTest);
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
#define main GradientVectorFlowImageFilterTest
#include "GradientVectorFlowImageFilter.cxx"

#undef main
#define main GradientRecursiveGaussianImageFilterTest
#include "GradientRecursiveGaussianImageFilter.cxx"

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
#define main ZeroCrossingBasedEdgeDetectionImageFilterTest
#include "ZeroCrossingBasedEdgeDetectionImageFilter.cxx"
