/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    RegistrationExamples12.cxx
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
  REGISTER_TEST(DeformableRegistration6Test);
  REGISTER_TEST(DeformableRegistration8Test);
  REGISTER_TEST(DeformableRegistration14Test);
}

#undef main
#define main  DeformableRegistration6Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate11
#include "DeformableRegistration6.cxx"

#undef main
#define main  DeformableRegistration8Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate12
#include "DeformableRegistration8.cxx"

#undef main
#define main  DeformableRegistration14Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate13
#include "DeformableRegistration14.cxx"

