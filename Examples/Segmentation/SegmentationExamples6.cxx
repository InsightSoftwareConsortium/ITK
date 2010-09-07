/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    SegmentationExamples6.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
// this file defines the SegmentationExamples for the test driver
// and all it expects is that you have a function called RegisterTests
#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif
#include <iostream>
#include "itkTestMain.h" 


void RegisterTests()
{
  REGISTER_TEST(HoughTransform2DCirclesImageFilterTest);
  REGISTER_TEST(HoughTransform2DLinesImageFilterTest);
}

#undef main
#define main HoughTransform2DLinesImageFilterTest
#include "HoughTransform2DLinesImageFilter.cxx"

#undef main
#define main HoughTransform2DCirclesImageFilterTest
#include "HoughTransform2DCirclesImageFilter.cxx"
