/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    RegistrationExamples6.cxx
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
  REGISTER_TEST(DeformableRegistration4Test);
  REGISTER_TEST(BSplineWarping1Test);
  REGISTER_TEST(LandmarkWarping2Test);
  vnl_sample_reseed(8775070);
  REGISTER_TEST(DeformableRegistration6Test);
}

#undef main
#define main  DeformableRegistration4Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate15
#include "DeformableRegistration4.cxx"

#undef main
#define main  BSplineWarping1Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate16
#include "BSplineWarping1.cxx"

#undef main
#define main  LandmarkWarping2Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate17
#include "LandmarkWarping2.cxx"


#undef main
#define main  DeformableRegistration6Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate18
#include "DeformableRegistration6.cxx"


