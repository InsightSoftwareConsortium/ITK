/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    RegistrationExamples2.cxx
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
  REGISTER_TEST(ModelToImageRegistration1Test);
  REGISTER_TEST(MultiResImageRegistration1Test);
  REGISTER_TEST(MultiResImageRegistration2Test);
}

#undef main
#define main  ModelToImageRegistration1Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate12
#include "ModelToImageRegistration1.cxx"

#undef main
#define main  MultiResImageRegistration1Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate13
#undef RegistrationInterfaceCommand
#define RegistrationInterfaceCommand RegistrationInterfaceCommand1
#include "MultiResImageRegistration1.cxx"

#undef main
#define main  MultiResImageRegistration2Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate14
#undef RegistrationInterfaceCommand
#define RegistrationInterfaceCommand RegistrationInterfaceCommand2
#include "MultiResImageRegistration2.cxx"
