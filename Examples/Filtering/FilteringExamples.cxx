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
