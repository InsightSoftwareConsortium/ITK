/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ImageExamples3.cxx
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
  REGISTER_TEST(ImageAdaptor2Test);
  REGISTER_TEST(ImageAdaptor3Test);
  REGISTER_TEST(ImageAdaptor4Test);
}


#undef main
#define main ImageAdaptor2Test
#include "ImageAdaptor2.cxx"

#undef main
#define main ImageAdaptor3Test
#include "ImageAdaptor3.cxx"

#undef main
#define main ImageAdaptor4Test
#include "ImageAdaptor4.cxx"
