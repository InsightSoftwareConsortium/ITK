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
  REGISTER_TEST(DigitallyReconstructedRadiograph1Test);
  REGISTER_TEST(FFTImageFilterTest);
  REGISTER_TEST(RGBToGrayscaleTest);
  REGISTER_TEST(LaplacianRecursiveGaussianImageFilter1Test);
  REGISTER_TEST(LaplacianRecursiveGaussianImageFilter2Test);
}


#undef main
#define main DigitallyReconstructedRadiograph1Test
#include "DigitallyReconstructedRadiograph1.cxx"

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
