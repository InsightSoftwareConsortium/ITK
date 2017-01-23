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
#include "itkChainCodePath.h"
#include "itkFourierSeriesPath.h"
#include "itkPathToChainCodePathFilter.h"
#include "itkChainCodeToFourierSeriesPathFilter.h"
#include "itkTestingMacros.h"

int itkChainCodeToFourierSeriesPathFilterTest( int, char*[] )
{
  const unsigned int Dimension = 2;
  typedef itk::PolyLineParametricPath< Dimension >  PolyLinePathType;
  typedef itk::ChainCodePath< Dimension >           ChainPathType;
  typedef itk::FourierSeriesPath< Dimension >       FSPathType;

  typedef PolyLinePathType::VertexType              VertexType;

  typedef itk::PathToChainCodePathFilter< PolyLinePathType, ChainPathType >
    PathToChainCodePathFilterType;
  typedef itk::ChainCodeToFourierSeriesPathFilter< ChainPathType, FSPathType >
    ChainCodeToFSPathFilterType;

  bool passed = true;


  // Setup the path
  std::cout << "Making a triangle Path with v0 at (30,30) -> (30,33) -> (33,33)" << std::endl;
  VertexType v;

  PolyLinePathType::Pointer inputPath = PolyLinePathType::New();

  EXERCISE_BASIC_OBJECT_METHODS( inputPath, PolyLineParametricPath,
    ParametricPath );

  v.Fill( 30 );
  inputPath->AddVertex( v );
  v[0] = 30;
  v[1] = 33;
  inputPath->AddVertex( v );
  v.Fill( 33 );
  inputPath->AddVertex( v );
  v.Fill( 30 );
  inputPath->AddVertex( v );

  // Set up the first filter
  PathToChainCodePathFilterType::Pointer pathToChainCodePathFilter =
    PathToChainCodePathFilterType::New();
  pathToChainCodePathFilter->SetInput( inputPath );

  ChainPathType::Pointer chainPath = pathToChainCodePathFilter->GetOutput();

  // Set up the second filter
  ChainCodeToFSPathFilterType::Pointer chainCodeToFSPathFilter =
    ChainCodeToFSPathFilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( chainCodeToFSPathFilter, ChainCodeToFourierSeriesPathFilter,
    PathToPathFilter );

  chainCodeToFSPathFilter->SetInput( pathToChainCodePathFilter->GetOutput() );

  FSPathType::Pointer outputPath = chainCodeToFSPathFilter->GetOutput();

  chainCodeToFSPathFilter->Update();

  std::cout << "PathToChainCodePathFilter: open test path is "
      << chainPath->NumberOfSteps() << " steps" << std::endl;
  if( chainPath->NumberOfSteps() != 9 )
    {
    passed = false;
    }
  std::cout << "ChainCodeToFourierSeriesPathFilter: smoothed path is from ["
      << outputPath->Evaluate( 0.0 ) << "] to [" << outputPath->Evaluate( 1.0 )
      << "] with a center at [" << outputPath->Evaluate( 0.5 ) << "]." << std::endl;
  // Floating point can be inprecise, so convert to rounded int for comparison:
  if( int( 0.5 + 1000 * ( outputPath->Evaluate( 1.0 ) )[0] ) !=
      int( 0.5 + 1000 * ( outputPath->Evaluate( 0.0 ) )[0] ) ||
      int( 0.5 + 1000 * ( outputPath->Evaluate( 1.0 ) )[1] ) !=
      int( 0.5 + 1000 * ( outputPath->Evaluate( 0.0 ) )[1] ) ||
      int( 0.5 + ( outputPath->Evaluate( 0.5 ) )[0] ) < 31 ||
      int( 0.5 + ( outputPath->Evaluate( 0.5 ) )[0] ) > 32 ||
      int( 0.5 + ( outputPath->Evaluate( 0.5 ) )[1] ) != 33 )
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
