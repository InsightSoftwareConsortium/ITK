/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    FilteringExamples3.cxx
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
  REGISTER_TEST(MorphologicalImageEnhancementTest);
  REGISTER_TEST(AntiAliasBinaryImageFilterTest);
  REGISTER_TEST(CannyEdgeDetectionImageFilterTest);
  REGISTER_TEST(FlipImageFilterTest);
  REGISTER_TEST(GaussianBlurImageFunctionTest);
}

#undef main
#define main MorphologicalImageEnhancementTest
#include "MorphologicalImageEnhancement.cxx"

#undef main
#define main AntiAliasBinaryImageFilterTest
#include "AntiAliasBinaryImageFilter.cxx"

#undef main
#define main CannyEdgeDetectionImageFilterTest
#include "CannyEdgeDetectionImageFilter.cxx"

#undef main
#define main FlipImageFilterTest
#include "FlipImageFilter.cxx"

#undef main
#define main GaussianBlurImageFunctionTest
#include "GaussianBlurImageFunction.cxx"
