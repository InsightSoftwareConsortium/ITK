/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    FilteringExamples5.cxx
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
  REGISTER_TEST(OtsuThresholdImageFilterTest);
  REGISTER_TEST(OtsuMultipleThresholdImageFilterTest);
  REGISTER_TEST(DiffusionTensor3DReconstructionImageFilterTest);
}

#undef main
#define main OtsuThresholdImageFilterTest
#include "OtsuThresholdImageFilter.cxx"

#undef main
#define main OtsuMultipleThresholdImageFilterTest
#include "OtsuMultipleThresholdImageFilter.cxx"

#undef main
#define main DiffusionTensor3DReconstructionImageFilterTest
#include "DiffusionTensor3DReconstructionImageFilter.cxx"
