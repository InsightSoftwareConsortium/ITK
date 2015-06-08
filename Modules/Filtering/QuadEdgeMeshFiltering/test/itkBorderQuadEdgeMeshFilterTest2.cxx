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

#include "itkRegularSphereMeshSource.h"
#include "itkBorderQuadEdgeMeshFilter.h"

#include "itkTestingMacros.h"

// This test demonstrates that BorderQuadEdgeMeshFilter throws an exception if
// the input has no boundary.

int itkBorderQuadEdgeMeshFilterTest2( int argc, char* argv[] )
{
  // ** ERROR MESSAGE AND HELP ** //
  if( argc != 2 )
    {
    std::cout <<"Requires 1 arguments: " << std::endl;
    std::cout <<"- Border Pick" << std::endl;
    std::cout <<"   * 0: LONGEST" << std::endl;
    std::cout <<"   * 1: LARGEST" << std::endl;

    return EXIT_FAILURE;
    }


  // ** TYPEDEF **
  typedef double Coord;

  typedef itk::QuadEdgeMesh< Coord, 3 >                       MeshType;
  typedef itk::RegularSphereMeshSource< MeshType >            SourceType;
  typedef itk::BorderQuadEdgeMeshFilter< MeshType, MeshType > BorderTransformType;

  SourceType::Pointer source = SourceType::New();

  // ** CHOSE< COMPUTE AND SET BORDER TRANSFORM **
  BorderTransformType::Pointer border_transform = BorderTransformType::New( );
  border_transform->SetInput( source->GetOutput()  );
  border_transform->SetTransformType( BorderTransformType::SQUARE_BORDER_TRANSFORM );

  switch( atoi( argv[1] ) )
    {
    case 0:
        border_transform->SetBorderPick( BorderTransformType::LONGEST );
        break;
    case 1:
        border_transform->SetBorderPick( BorderTransformType::LARGEST );
        break;
    default: // handle .... user ....
        std::cerr << "0 for LONGEST BORDER or 1 for LARGEST BORDER" << std::endl;
        return EXIT_FAILURE;
    }

  TRY_EXPECT_EXCEPTION( border_transform->Update() );

  // GET OUT OF HERE AND GET (YET ANOTHER) COFFEE
  return EXIT_SUCCESS;
}
