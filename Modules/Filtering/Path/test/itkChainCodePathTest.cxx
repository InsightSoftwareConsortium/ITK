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
#include "itkChainCodePath.h"
#include "itkTestingMacros.h"

int itkChainCodePathTest( int, char*[] )
{
  const unsigned int Dimension = 2;

  typedef itk::ChainCodePath< Dimension > PathType;
  typedef PathType::IndexType             IndexType;
  typedef PathType::OffsetType            OffsetType;

  bool passed = true;

  IndexType  index;
  OffsetType offset;

  PathType::Pointer path = PathType::New();

  EXERCISE_BASIC_OBJECT_METHODS( path, ChainCodePath, Path );

  index[0] = 3;
  index[1] = 5;
  path->SetStart( index );

  offset[0] = 0;  offset[1] = 1;  path->InsertStep( 0, offset );
  offset[0] = 1;  offset[1] = 1;  path->InsertStep( 1, offset );
  offset[0] = 1;  offset[1] = 0;  path->InsertStep( 2, offset );
  offset[0] = 1;  offset[1] = -1; path->InsertStep( 3, offset );
  offset[0] = 0;  offset[1] = -1; path->InsertStep( 4, offset );
  offset[0] = -1; offset[1] = -1; path->InsertStep( 5, offset );
  offset[0] = -1; offset[1] = 0;  path->InsertStep( 6, offset );
  offset[0] = -1; offset[1] = 1;  path->InsertStep( 7, offset );

  std::cout << "Path is " << path->NumberOfSteps() << " steps" << std::endl;

  offset[0] = 0; offset[1] = -1;
  path->InsertStep( 3, offset ); // insert new step 3
  offset = path->Evaluate( 3 );
  std::cout << "Inserted new step[3] = "<< offset << std::endl;

  offset[0] = 1; offset[1] = 0;
  path->ChangeStep( 4, offset ); // rotate the down-right step (now step 4) CCW
  offset = path->Evaluate( 4 );
  std::cout << "Changed step[4] to " << offset << std::endl;

  std::cout << "Path is " << path->NumberOfSteps() << " steps" << std::endl;
  if( path->NumberOfSteps() != 9 )
    {
    passed = false;
    }

  index = path->GetStart();
  std::cout << "Starting at index: " << index << std::endl;
  PathType::InputType endOfInput = path->EndOfInput();
  std::cout << "End of input: "
    << itk::NumericTraits< PathType::InputType >::PrintType( endOfInput )
    << std::endl;

  for( unsigned int input = 0;; )
    {
    offset = path->IncrementInput( input );
    if( offset[0] || offset[1] )
      {
      index = path->EvaluateToIndex( input );

      std::cout << "Step[" << input - 1 << "] is "<< offset;
      std::cout << "\t to index " << index << std::endl;
      }
    else
      {
      break;
      }
    }
    if( index != path->GetStart() )
    {
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
