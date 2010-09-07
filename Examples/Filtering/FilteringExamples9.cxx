/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    FilteringExamples9.cxx
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
  REGISTER_TEST(SpatialObjectToImage1);
  REGISTER_TEST(SpatialObjectToImage2);
  REGISTER_TEST(SpatialObjectToImage3);
}

#undef main
#define main SpatialObjectToImage1
#include "SpatialObjectToImage1.cxx"

#undef main
#define main SpatialObjectToImage2
#include "SpatialObjectToImage2.cxx"

#undef main
#define main SpatialObjectToImage3
#include "SpatialObjectToImage3.cxx"

