/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFourierSeriesPathTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include <iostream>
#include "itkFourierSeriesPath.h"

int itkFourierSeriesPathTest(int, char*[])
{
  typedef  itk::FourierSeriesPath<2>   PathType;
  typedef  PathType::InputType         InputType;
  typedef  PathType::IndexType         IndexType;
  typedef  PathType::OffsetType        OffsetType;
  typedef  PathType::VectorType        VectorType;
  
  bool passed = true;

  InputType   input;
  OffsetType  offset;
  VectorType  cosV, sinV, v;

  PathType::Pointer path = PathType::New();
  

  // Average value is (5,5)
  cosV.Fill(5);
  sinV.Fill(0);
  path->AddHarmonic( cosV, sinV );
  
  cosV.Fill(2.7);
  sinV.Fill(3.2);
  path->AddHarmonic( cosV, sinV );
  
  std::cout << "Evaluating at 0, 0.5, and 1.0:  " << path->Evaluate(0) << ", "
       << path->Evaluate(0.5) << ", " << path->Evaluate(1.0) << std::endl;
  // Floating point can be inprecise, so convert to rounded int for comparison:
  if( int(0.5+1000*(path->Evaluate(1.0))[0]) !=
      int(0.5+1000*(path->Evaluate(0.0))[0]) ||
      int(0.5+1000*(path->Evaluate(1.0))[1]) !=
      int(0.5+1000*(path->Evaluate(0.0))[1]) )
    {
    std::cout << "FourierSeriesPathTest:  Evaluate() Failed" << std::endl;
    passed = false;
    }
  
  std::cout << "Evaluating to an index at 0, 0.5, and 1.0:  "
       << path->EvaluateToIndex(0) << ", " << path->EvaluateToIndex(0.5)
       << ", " << path->EvaluateToIndex(1.0) << std::endl;
  if( (path->EvaluateToIndex(1.0)) != (path->EvaluateToIndex(0.0)) )
    {
    std::cout << "FourierSeriesPathTest:  EvaluateToIndex() Failed" << std::endl;
    passed = false;
    }
  
  std::cout << "Evaluating the derivative at 0, 0.5, and 1.0:  "
       << path->EvaluateDerivative(0) << ", " << path->EvaluateDerivative(0.5)
       << ", " << path->EvaluateDerivative(1.0) << std::endl;
  // Floating point can be inprecise, so convert to rounded int for comparison:
  if( int(0.5+1000*(path->EvaluateDerivative(1.0))[0]) !=
      int(0.5+1000*(path->EvaluateDerivative(0.0))[0]) ||
      int(0.5+1000*(path->EvaluateDerivative(1.0))[1]) !=
      int(0.5+1000*(path->EvaluateDerivative(0.0))[1]) )
    {
    std::cout << "FourierSeriesPathTest:  EvaluateDerivative() Failed" << std::endl;
    passed = false;
    }
  
  input = 0;
  offset = path->IncrementInput( input );
  std::cout << "Incrementing the input from 0 to "<<input<<":  " << offset << std::endl;
  
  input = 0.5;
  offset = path->IncrementInput( input );
  std::cout << "Incrementing the input from 0.5 to "<<input<<":  " << offset << std::endl;
  if( offset[0]!=-1 || offset[1]!=-1 )
    {
    std::cout << "FourierSeriesPathTest:  IncrementInput() Failed" << std::endl;
    passed = false;
    }
  
  if (passed)
    {
    std::cout << "FourierSeries tests passed" << std::endl;
    return EXIT_SUCCESS;
    }
  else
    {
    std::cout << "FourierSeries tests failed" << std::endl;
    return EXIT_FAILURE;
    }
}
