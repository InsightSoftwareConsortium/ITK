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
#include "itkPolyLineParametricPath.h"
#include "itkChainCodePath2D.h"
#include "itkPathToChainCodePathFilter.h"
#include "itkTestingMacros.h"

int itkPathToChainCodePathFilterTest( int, char* [] )
{
  const unsigned int Dimension = 2;
  typedef itk::PolyLineParametricPath< Dimension >  InPathType;
  typedef itk::ChainCodePath2D                      ChainPathType;

  typedef InPathType::VertexType               VertexType;

  typedef itk::PathToChainCodePathFilter< InPathType, ChainPathType > FilterType;

  bool passed = true;

  // Setup the path
  std::cout << "Making a triangle Path with v0 at (30,30) -> (30,33) -> (33,33)" << std::endl;
  VertexType v;
  InPathType::Pointer inPath = InPathType::New();
  ChainPathType::Pointer chainPath;

  v.Fill( 30 );
  inPath->AddVertex( v );
  v[0] = 30;
  v[1] = 33;
  inPath->AddVertex( v );
  v.Fill( 33 );
  inPath->AddVertex( v );

  // Set up the filter
  FilterType::Pointer filter = FilterType::New();
  EXERCISE_BASIC_OBJECT_METHODS( filter, PathToChainCodePathFilter,
    PathToPathFilter );

  bool maximallyConnected = false;
  TEST_SET_GET_BOOLEAN( filter, MaximallyConnected, maximallyConnected );

  filter->SetInput( inPath );

  chainPath = filter->GetOutput();

  chainPath->Update();

  std::cout << "PathToChainCodePathFilter: open test path is "
      << chainPath->NumberOfSteps() << " steps:\n \""
      << chainPath->GetChainCodeAsString() << "\"." << std::endl;
  if( chainPath->NumberOfSteps() != 6 )
    {
    passed = false;
    }

  // Close the triangle
  v.Fill( 30 );
  inPath->AddVertex( v );
  chainPath->Update();

  std::cout << "PathToChainCodePathFilter: closed test path is "
      << chainPath->NumberOfSteps() << " steps:\n \""
      << chainPath->GetChainCodeAsString() << "\"." << std::endl;
  if( chainPath->NumberOfSteps() != 9 )
    {
    passed = false;
    }

  maximallyConnected = true;
  TEST_SET_GET_BOOLEAN( filter, MaximallyConnected, maximallyConnected );

  filter->Update();

  std::cout << "PathToChainCodePathFilter: maximally connected test path is "
      << chainPath->NumberOfSteps() << " steps:\n \""
      << chainPath->GetChainCodeAsString() << "\"." << std::endl;
  if( chainPath->NumberOfSteps() != 12 )
    {
    passed = false;
    }

  if (passed)
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
