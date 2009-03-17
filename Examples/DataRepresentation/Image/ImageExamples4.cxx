/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ImageExamples4.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
// this file defines the ImageExamples for the test driver
#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif
#include <iostream>
#include "itkTestMain.h" 


void RegisterTests()
{
  REGISTER_TEST(RGBImageTest);
  REGISTER_TEST(VectorImageTest);
}
#undef main
#define main RGBImageTest
#include "RGBImage.cxx"

#undef main
#define main VectorImageTest
#include "VectorImage.cxx"
