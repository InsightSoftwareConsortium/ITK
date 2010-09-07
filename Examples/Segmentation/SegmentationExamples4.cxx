/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    SegmentationExamples4.cxx
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
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif
#include "itkTestMain.h" 
#include <iostream>


void RegisterTests()
{
  REGISTER_TEST(CurvesLevelSetImageFilterTest);
}

#undef main
#define main CurvesLevelSetImageFilterTest
#include "CurvesLevelSetImageFilter.cxx"
