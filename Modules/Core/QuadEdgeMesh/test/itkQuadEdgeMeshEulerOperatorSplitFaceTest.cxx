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

#include "itkQuadEdgeMeshEulerOperatorSplitFacetFunction.h"
#include "itkQuadEdgeMeshEulerOperatorJoinFacetFunction.h"
#include "itkQuadEdgeMeshEulerOperatorsTestHelper.h"

int itkQuadEdgeMeshEulerOperatorSplitFaceTest( int, char * [] )
{

  typedef itk::QuadEdgeMesh< double, 3 >                      MeshType;
  typedef MeshType::Pointer                                   MeshPointer;
  typedef MeshType::QEType                                    QEType;

  typedef itk::QuadEdgeMeshEulerOperatorSplitFacetFunction< MeshType, QEType>
    SplitFacet;
  typedef itk::QuadEdgeMeshEulerOperatorJoinFacetFunction< MeshType, QEType >
    JoinFacet;

  /////////////////////////////////////////
  //
  //         Split Facet
  //
  /////////////////////////////////////////

  // Split the facet again in order to restore the original situation:
  std::cout << "Checking SplitFacet." << std::endl;

  SplitFacet::Pointer splitFacet = SplitFacet::New( );
  std::cout << "     " << "Test No Mesh Input";
  if( splitFacet->Evaluate( (QEType*)1, (QEType*)2 ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "OK" << std::endl;

  (void)splitFacet->GetNameOfClass();

  MeshPointer mesh = MeshType::New();
  CreateSquareTriangularMesh<MeshType>( mesh );

  splitFacet->SetInput( mesh );

  std::cout << "     " << "Test No QE Input";
  if( splitFacet->Evaluate( (QEType*)ITK_NULLPTR, (QEType*)ITK_NULLPTR ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "OK" << std::endl;

  std::cout << "     " << "Test two QE Input not sharing the same left";
  if( splitFacet->Evaluate( mesh->FindEdge( 10, 16 ),
                            mesh->FindEdge( 13, 19 ) ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "OK" << std::endl;

  std::cout << "     " << "Test twice same non-null QE Input";
  if( splitFacet->Evaluate( (QEType*)1, (QEType*)1 ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "OK" << std::endl;

  std::cout << "     " << "Test two consecutive QE Input";
  if( splitFacet->Evaluate( mesh->FindEdge( 10, 16 ),
                            mesh->FindEdge( 10, 16 )->GetLnext( ) ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "OK" << std::endl;

  JoinFacet::Pointer joinFacet = JoinFacet::New();
  QEType* DeletedEdge = mesh->FindEdge( 12, 7 );
  QEType* G = DeletedEdge->GetSym( )->GetLprev( );
  QEType* H = joinFacet->Evaluate( DeletedEdge );

  if( !splitFacet->Evaluate( H, G ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "     " << "Split a face (possible)";
  // The number of edges and faces must be respectively identical to
  // the original number edges and faces:
  if( ! AssertTopologicalInvariants< MeshType >
          ( mesh, 25, 56, 32, 1, 0 ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  if ( mesh->GetPoint( 12 ).GetValence( ) != 6 )
    {
    std::cout << "FAILED [wrong valence of "
              << mesh->GetPoint( 12 ).GetValence( )
              << " ]." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "OK" << std::endl;
  std::cout << "Checking SplitFacet. OK" << std::endl << std::endl;

  std::cout << "Checking JoinFacet( SplitFacet( edge ) ) invariance.";
  G = mesh->FindEdge( 12, 7 )->GetSym( )->GetLprev( );
  H = joinFacet->Evaluate( mesh->FindEdge( 12, 7 ) );
  if( !H )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  if( !joinFacet->Evaluate( splitFacet->Evaluate( H, G ) ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  if( ! AssertTopologicalInvariants< MeshType >
          ( mesh, 25, 55, 31, 1, 0 ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  if ( mesh->GetPoint( 12 ).GetValence( ) != 5 )
    {
    std::cout << "FAILED [wrong valence of "
              << mesh->GetPoint( 12 ).GetValence( )
              << " ]." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << ".OK" << std::endl << std::endl;

  return EXIT_SUCCESS;
}
