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

#include "itkHilbertPath.h"

int itkHilbertPathTest( int, char*[] )
{

  // Test dimension = 2
  for( unsigned int order = 0; order < 5; order++ )
    {
    typedef itk::HilbertPath<unsigned int, 2> PathType;
    PathType::Pointer path = PathType::New();
    path->SetHilbertOrder( 2 );
    path->Initialize();

    typedef PathType::IndexType IndexType;

    for( unsigned int d = 6; d < 10 + 0 *path->NumberOfSteps(); d++ )
      {
      IndexType index = path->TransformPathIndexToMultiDimensionalIndex( d );

      if( d != path->EvaluateInverse( index ) )
        {
        std::cerr << "Incorrect match-up for path index (" << d
          << ") and multi-dimensional index (" << index << ")"
          << " dimension = 2 " << std::endl;
        return EXIT_FAILURE;
        }
      }
    }

  // Test dimension = 3
  for( unsigned int order = 0; order < 5; order++ )
    {
    typedef itk::HilbertPath<unsigned int, 3> PathType;
    PathType::Pointer path = PathType::New();
    path->SetHilbertOrder( order );
    path->Initialize();

    typedef PathType::IndexType IndexType;

    for( unsigned int d = 0; d < path->NumberOfSteps(); d++ )
      {
      IndexType index = path->Evaluate( d );
      if( d != path->EvaluateInverse( index ) )
        {
        std::cerr << "Incorrect match-up for path index (" << d
          << ") and multi-dimensional index (" << index << ")"
          << " dimension = 3 " << std::endl;
        return EXIT_FAILURE;
        }
      }
    }

  // Test dimension = 4
  for( unsigned int order = 0; order < 5; order++ )
    {
    typedef itk::HilbertPath<unsigned int, 4> PathType;
    PathType::Pointer path = PathType::New();
    path->SetHilbertOrder( order );
    path->Initialize();

    typedef PathType::IndexType IndexType;

    for( unsigned int d = 0; d < path->NumberOfSteps(); d++ )
      {
      IndexType index = path->Evaluate( d );
      if( d != path->EvaluateInverse( index ) )
        {
        std::cerr << "Incorrect match-up for path index (" << d
          << ") and multi-dimensional index (" << index << ")"
          << " dimension = 4 " << std::endl;
        return EXIT_FAILURE;
        }
      }
    }
  return EXIT_SUCCESS;
}
