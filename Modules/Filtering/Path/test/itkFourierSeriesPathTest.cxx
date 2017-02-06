/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include <iostream>
#include "itkFourierSeriesPath.h"
#include "itkTestingMacros.h"

int itkFourierSeriesPathTest( int, char*[] )
{
  const unsigned int Dimension = 2;
  typedef itk::FourierSeriesPath< Dimension >  PathType;
  typedef PathType::InputType                  InputType;
  typedef PathType::OffsetType                 OffsetType;
  typedef PathType::VectorType                 VectorType;

  bool passed = true;

  InputType  input;
  OffsetType offset;
  VectorType cosV, sinV, v;

  PathType::Pointer path = PathType::New();

  EXERCISE_BASIC_OBJECT_METHODS( path, FourierSeriesPath, ParametricPath );

  // Average value is (5,5)
  cosV.Fill( 5 );
  sinV.Fill( 0 );
  path->AddHarmonic( cosV, sinV );
  cosV.Fill( 2.7 );
  sinV.Fill( 3.2 );
  path->AddHarmonic( cosV, sinV );

  std::cout << "Evaluating at 0, 0.5, and 1.0: " << path->Evaluate( 0 ) << ", "
       << path->Evaluate( 0.5 ) << ", " << path->Evaluate( 1.0 ) << std::endl;
  // Floating point can be imprecise, so convert to rounded int for comparison
  if( int( 0.5 + 1000 * ( path->Evaluate( 1.0 ) )[0] ) !=
      int( 0.5 + 1000 * ( path->Evaluate( 0.0 ) )[0] ) ||
      int( 0.5 + 1000 * ( path->Evaluate( 1.0 ) )[1] ) !=
      int( 0.5 + 1000 * ( path->Evaluate( 0.0 ) )[1] ) )
    {
    std::cout << "Evaluate() Failed" << std::endl;
    passed = false;
    }

  std::cout << "Evaluating to an index at 0, 0.5, and 1.0: "
       << path->EvaluateToIndex(0) << ", " << path->EvaluateToIndex( 0.5 )
       << ", " << path->EvaluateToIndex( 1.0 ) << std::endl;
  if( path->EvaluateToIndex( 1.0 ) != path->EvaluateToIndex( 0.0 ) )
    {
    std::cout << "FourierSeriesPathTest: EvaluateToIndex() Failed" << std::endl;
    passed = false;
    }

  std::cout << "Evaluating the derivative at 0, 0.5, and 1.0: "
       << path->EvaluateDerivative( 0 ) << ", " << path->EvaluateDerivative( 0.5 )
       << ", " << path->EvaluateDerivative( 1.0 ) << std::endl;
  // Floating point can be imprecise, so convert to rounded int for comparison
  if( int( 0.5 + 1000 * ( path->EvaluateDerivative( 1.0 ) )[0] ) !=
      int( 0.5 + 1000 * ( path->EvaluateDerivative( 0.0 ) )[0] ) ||
      int( 0.5 + 1000 * ( path->EvaluateDerivative( 1.0 ) )[1] ) !=
      int( 0.5 + 1000 * ( path->EvaluateDerivative( 0.0 ) )[1] ) )
    {
    std::cout << "EvaluateDerivative() Failed" << std::endl;
    passed = false;
    }

  input = 0;
  offset = path->IncrementInput( input );
  std::cout << "Incrementing the input from 0 to " << input << ": " << offset
    << std::endl;

  input = 0.5;
  offset = path->IncrementInput( input );
  std::cout << "Incrementing the input from 0.5 to " << input << ": " << offset
    << std::endl;
  if( offset[0] != -1 || offset[1] != -1 )
    {
    std::cout << "IncrementInput() Failed" << std::endl;
    passed = false;
    }

  if( passed )
    {
    std::cout << "Test passed" << std::endl;
    return EXIT_SUCCESS;
    }
  else
    {
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
    }
}
