/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    RegistrationExamples14.cxx
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
#include "itkTestMain.h" 


void RegisterTests()
{
  REGISTER_TEST(DeformableRegistration15Test);
  REGISTER_TEST(IterativeClosestPoint1Test);
  REGISTER_TEST(IterativeClosestPoint2Test);
  REGISTER_TEST(IterativeClosestPoint3Test);
}

#undef main
#define main  DeformableRegistration15Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate15
#include "DeformableRegistration15.cxx"

#undef main
#define main  IterativeClosestPoint1Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdateICP1
#include "IterativeClosestPoint1.cxx"

#undef main
#define main  IterativeClosestPoint2Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdateICP2
#include "IterativeClosestPoint2.cxx"

#undef main
#define main  IterativeClosestPoint3Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdateICP3
#include "IterativeClosestPoint3.cxx"

