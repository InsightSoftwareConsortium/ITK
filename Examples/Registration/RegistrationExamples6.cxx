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

#include <iostream>
#include "vnl/vnl_sample.h"
#include "itkTestMain.h" 


void RegisterTests()
{
  REGISTER_TEST(BSplineWarping1Test);
  REGISTER_TEST(BSplineWarping2Test);
  REGISTER_TEST(LandmarkWarping2Test);
}

#undef main
#define main  BSplineWarping1Test
#undef CommandProgressUpdate
#define CommandProgressUpdate CommandProgressUpdate01
#include "BSplineWarping1.cxx"

#undef main
#define main  BSplineWarping2Test
#undef CommandProgressUpdate
#define CommandProgressUpdate CommandProgressUpdate02
#include "BSplineWarping2.cxx"

#undef main
#define main  LandmarkWarping2Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate18
#include "LandmarkWarping2.cxx"
