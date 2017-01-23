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
#include "itkChainCodePath2D.h"
#include "itkTestingMacros.h"

int itkChainCodePath2DTest( int, char*[] )
{
  typedef itk::ChainCodePath2D  PathType;
  typedef PathType::IndexType   IndexType;
  typedef PathType::OffsetType  OffsetType;

  bool passed = true;

  IndexType   index;
  OffsetType  offset;

  PathType::Pointer path = PathType::New();

  EXERCISE_BASIC_OBJECT_METHODS( path, ChainCodePath2D, ChainCodePath );

  index[0] = 3;
  index[1] = 5;
  path->SetStart( index );

  for( int i = 0; i < 8; ++i )
    {
    path->InsertStep( i*2,   i+1 );
    path->InsertStep( i*2+1, i+1 );
    }

  std::cout << "Path is " << path->NumberOfSteps() << " steps: \""
       << path->GetChainCodeAsString() << "\"." << std::endl;

  offset[0] = 0;
  offset[1] = -1;
  path->InsertStep( 5, offset ); // insert new step 5 = 5
  offset = path->Evaluate( 5 );
  std::cout << "Inserted new step[5] of 5 = (" << offset[0] << ","
    << offset[1] << ")" << std::endl;

  path->ChangeStep( 8, 3 ); // rotate the second 4 (now step 8) up to a 3
  offset = path->Evaluate( 8 );
  std::cout << "Changed step[8] to 3 = (" << offset[0] << "," << offset[1]
    << ")" << std::endl;


  path->ChangeStep( 6, offset ); // rotate the second 4 (now step 8) up to a 3
  offset = path->Evaluate( 6 );
  std::cout << "Changed step[6] to = (" << offset[0] << "," << offset[1]
    << ")" << std::endl;


  std::cout << "Path is " << path->NumberOfSteps() << " steps: \""
       << path->GetChainCodeAsString() << "\"." << std::endl;
  if( path->NumberOfSteps() != 17 )
    {
    passed = false;
    }


  index = path->GetStart();
  std::cout << "Starting at index (" << index[0] << "," << index[1] << ")"
    << std::endl;
  for( unsigned int input = 0;; )
    {
    offset = path->IncrementInput( input );
    if( offset[0] || offset[1] )
      {
      index = path->EvaluateToIndex( input );

      std::cout << "Step[" << input - 1 << "] is (" << offset[0] << ","
        << offset[1] << ")";
      std::cout << "\t to index (" << index[0] << "," << index[1] << ")"
        << std::endl;
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
