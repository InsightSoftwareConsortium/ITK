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

#include "itkQuadEdgeMeshEulerOperatorFlipEdgeFunction.h"
#include "itkQuadEdgeMeshEulerOperatorsTestHelper.h"

int itkQuadEdgeMeshEulerOperatorFlipTest( int , char * [] )
{

  typedef itk::QuadEdgeMesh< double, 3 >                      MeshType;
  typedef MeshType::Pointer                                   MeshPointer;
  typedef MeshType::QEType                                    QEType;

  typedef itk::QuadEdgeMeshEulerOperatorFlipEdgeFunction< MeshType, QEType>
    FlipEdge;

  MeshPointer  mesh = MeshType::New();
  CreateSquareTriangularMesh<MeshType>( mesh );
  FlipEdge::Pointer flipEdge = FlipEdge::New( );
  std::cout << flipEdge << std::endl;

#ifndef NDEBUG
  if( flipEdge->Evaluate( (QEType*)1 ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "OK" << std::endl;
#endif

  std::cout << flipEdge->GetNameOfClass() << std::endl;

  flipEdge->SetInput( mesh );

#ifndef NDEBUG
  std::cout << "     " << "Test QE Input not internal";
  QEType* dummy = new QEType;
  if( flipEdge->Evaluate( dummy ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  delete dummy;
  std::cout << "OK" << std::endl;
  std::cout << "     " << "Test No QE Input";
  if( flipEdge->Evaluate( (QEType*)0 ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "OK" << std::endl;
#endif

  mesh->LightWeightDeleteEdge( mesh->FindEdge( 12, 18 ) );
  mesh->AddFace( mesh->FindEdge( 17 ,12 ) );
  std::cout << "     " << "Flip an edge with a polygonal face (impossible)";
  QEType* tempFlippedEdge = flipEdge->Evaluate( mesh->FindEdge( 12 , 17 ) );
  if( tempFlippedEdge )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }

  CreateSquareTriangularMesh<MeshType>( mesh );
  std::cout << "     " << "Flip an edge (possible)";
  tempFlippedEdge = flipEdge->Evaluate( mesh->FindEdge( 12 , 6 ) );
  if( !tempFlippedEdge )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  // The number of edges and faces must be unchanged:
  if( ! AssertTopologicalInvariants< MeshType >
          ( mesh, 25, 56, 32, 1, 0 ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  if ( mesh->GetPoint( 12 ).GetValence( ) != 5 )
    {
    std::cout << "FAILED [wrong valence of "
               << mesh->GetPoint( 12 ).GetValence( )
               << " for vertex 12 ]." << std::endl;
    return EXIT_FAILURE;
    }
  if ( mesh->GetPoint( 6 ).GetValence( ) != 5 )
    {
    std::cout << "FAILED [wrong valence of "
              << mesh->GetPoint( 6 ).GetValence( )
              << " for vertex 6 ]." << std::endl;
    return EXIT_FAILURE;
    }
  if ( mesh->GetPoint( 11 ).GetValence( ) != 7 )
    {
    std::cout << "FAILED [wrong valence of "
              << mesh->GetPoint( 11 ).GetValence( )
              << " for vertex 11 ]." << std::endl;
    return EXIT_FAILURE;
    }
  if ( mesh->GetPoint( 7 ).GetValence( ) != 7 )
    {
    std::cout << "FAILED [wrong valence of "
              << mesh->GetPoint( 7 ).GetValence( )
              << " for vertex 7 ]." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << ".OK" << std::endl;
   // Checking invariance (i.e. FlipEdge is it's own inverse):
  std::cout << "     " << "Check FlipEdge(FlipEdge()) invariance (possible for triangles).";
  if( !flipEdge->Evaluate( tempFlippedEdge ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  // The number of edges and faces must be unchanged:
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
              << " for vertex 12 ]." << std::endl;
    return EXIT_FAILURE;
    }
  if ( mesh->GetPoint( 6 ).GetValence( ) != 6 )
    {
    std::cout << "FAILED [wrong valence of "
              << mesh->GetPoint( 6 ).GetValence( )
              << " for vertex 6 ]." << std::endl;
    return EXIT_FAILURE;
    }
  if ( mesh->GetPoint( 11 ).GetValence( ) != 6 )
    {
    std::cout << "FAILED [wrong valence of "
              << mesh->GetPoint( 11 ).GetValence( )
              << " for vertex 11 ]." << std::endl;
    return EXIT_FAILURE;
    }
  if ( mesh->GetPoint( 7 ).GetValence( ) != 6 )
    {
    std::cout << "FAILED [wrong valence of "
              << mesh->GetPoint( 7 ).GetValence( )
              << " for vertex 7 ]." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "OK" << std::endl;
  std::cout << "Checking FlipEdge." << "OK" << std::endl << std::endl;

  return EXIT_SUCCESS;
}
