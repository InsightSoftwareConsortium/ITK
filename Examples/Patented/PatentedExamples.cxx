/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    PatentedExamples.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// this file defines the PatentedExamples for the test driver
// and all it expects is that you have a function called RegisterTests
#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif
#include <iostream>
#include "itkTestMain.h" 


void RegisterTests()
{
  REGISTER_TEST(FuzzyConnectednessImageFilterTest);
}

#undef main
#define main FuzzyConnectednessImageFilterTest
#include "FuzzyConnectednessImageFilter.cxx"

#undef main
#define main  IterativeClosestPoint1Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate12
#include "IterativeClosestPoint1.cxx"

#undef main
#define main  IterativeClosestPoint2Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate13
#include "IterativeClosestPoint2.cxx"

#undef main
#define main  IterativeClosestPoint3Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate14
#include "IterativeClosestPoint3.cxx"
