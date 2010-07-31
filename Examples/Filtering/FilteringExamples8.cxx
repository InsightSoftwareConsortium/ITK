/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    FilteringExamples8.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
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
