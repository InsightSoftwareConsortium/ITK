/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    SegmentationExamples5.cxx
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
  REGISTER_TEST(WatershedSegmentation1Test);
  REGISTER_TEST(RelabelComponentImageFilterTest);
}

#undef main
#define main WatershedSegmentation1Test
#include "WatershedSegmentation1.cxx"

#undef main
#define main RelabelComponentImageFilterTest
#include "RelabelComponentImageFilter.cxx"
