/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    RegistrationExamples9.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// this file defines the RegistrationExamples for the test driver
// and all it expects is that you have a function called RegisterTests

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#ifdef __BORLANDC__
#define ITK_LEAN_AND_MEAN
#endif

#include <iostream>
#include "vnl/vnl_sample.h"
#include "itkTestMain.h" 


void RegisterTests()
{
  vnl_sample_reseed(8775070);
  REGISTER_TEST(ImageRegistration5Test);
  REGISTER_TEST(ImageRegistration6Test);
  REGISTER_TEST(ImageRegistration7Test);
  REGISTER_TEST(ModelToImageRegistration2Test);
}

#undef main
#define main  ImageRegistration5Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate7
#include "ImageRegistration5.cxx"

#undef main
#define main  ImageRegistration6Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate8
#include "ImageRegistration6.cxx"

#undef main
#define main  ImageRegistration7Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate9
#include "ImageRegistration7.cxx"



#undef main
#define main  ModelToImageRegistration2Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate10
#include "ModelToImageRegistration2.cxx"



