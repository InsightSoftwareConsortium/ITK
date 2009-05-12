/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAtanRegularizedHeavisideStepFunctionTest1.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkAtanRegularizedHeavisideStepFunction.h"
#include "itkTestingMacros.h"

int itkAtanRegularizedHeavisideStepFunctionTest1( int, char* [] )
{
  typedef double    InputType;
  typedef double    OutputType;

  typedef itk::AtanRegularizedHeavisideStepFunction< InputType, OutputType >  HeavisideFunctionBaseType;

  HeavisideFunctionBaseType::Pointer functionBase0 = HeavisideFunctionBaseType::New();

  std::cout << "GetNameOfClass() = " << functionBase0->GetNameOfClass() << std::endl;
  functionBase0->Print( std::cout );

  const double epsilon0 = 1.0;
  const double epsilon1 = 1e-4;

  TEST_SET_GET_VALUE( epsilon0, functionBase0->GetEpsilon() );
  
  functionBase0->SetEpsilon( epsilon1 );
  TEST_SET_GET_VALUE( epsilon1, functionBase0->GetEpsilon() );
  
  const double epsilon2 = 0.5;
  functionBase0->SetEpsilon( epsilon2 );

  const signed int minValue = -20;
  const signed int maxValue =  20;

  const InputType incValue = 0.1;

  for( signed int x = minValue; x < maxValue; x++ )
    {
    const InputType ix = x * incValue;
    OutputType f  = functionBase0->Evaluate( ix );
    OutputType df = functionBase0->EvaluateDerivative( ix );
    std::cout << ix << " " << f << " " << df << std::endl;
    }

  return EXIT_SUCCESS;
}
