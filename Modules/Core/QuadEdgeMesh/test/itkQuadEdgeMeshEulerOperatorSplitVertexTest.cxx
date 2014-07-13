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

#include "itkQuadEdgeMeshEulerOperatorSplitVertexFunction.h"
#include "itkQuadEdgeMeshEulerOperatorJoinVertexFunction.h"
#include "itkQuadEdgeMeshEulerOperatorsTestHelper.h"

int itkQuadEdgeMeshEulerOperatorSplitVertexTest( int , char * [] )
{

  typedef itk::QuadEdgeMesh< double, 3 >                      MeshType;
  typedef MeshType::Pointer                                   MeshPointer;
  typedef MeshType::QEType                                    QEType;
  typedef MeshType::PointType                                 PointType;

  typedef itk::QuadEdgeMeshEulerOperatorSplitVertexFunction< MeshType, QEType>
    SplitVertex;
  typedef itk::QuadEdgeMeshEulerOperatorJoinVertexFunction< MeshType, QEType>
    JoinVertex;

  /////////////////////////////////////////
  //
  //          Split Vertex
  //
  /////////////////////////////////////////
  std::cout << "Checking SplitVertex." << std::endl;
  MeshPointer mesh = MeshType::New();
  CreateSquareTriangularMesh<MeshType>( mesh );

  SplitVertex::Pointer splitVertex = SplitVertex::New( );
  std::cout << "     " << "Test No Mesh Input";
  if( splitVertex->Evaluate( (QEType*)1, (QEType*)1 ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "OK" << std::endl;

  (void)splitVertex->GetNameOfClass();

  splitVertex->SetInput( mesh );
  std::cout << "     " << "Test No QE Input";
  if( splitVertex->Evaluate( (QEType*)ITK_NULLPTR, (QEType*)ITK_NULLPTR ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "OK" << std::endl;

  std::cout << "     ";
  std::cout << "Split Vertex (Possible).";
  if( !splitVertex->Evaluate( mesh->FindEdge(  5, 11 ),
                              mesh->FindEdge( 17, 11 ) ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  // move the new point, for example along z axis for special effect :-D
  PointType PtTmp = mesh->GetPoint( splitVertex->GetNewPointID( ) );
  PtTmp[2] = 1;
  mesh->SetPoint( splitVertex->GetNewPointID( ), PtTmp );
  // Test
  if( ! AssertTopologicalInvariants< MeshType >
          ( mesh, 26, 57, 32, 1, 0 ) )
    {
    std::cout << "FAILED, wrong topology" << std::endl;
    return EXIT_FAILURE;
    }
  if ( mesh->GetPoint( splitVertex->GetNewPointID( ) ).GetValence( ) != 4 )
    {
    std::cout << "FAILED, wrong valence of "
    << mesh->GetPoint( splitVertex->GetNewPointID( ) ).GetValence( )
    << " for vertex " << splitVertex->GetNewPointID( ) << "." << std::endl;
    return EXIT_FAILURE;
    }
  if ( mesh->GetPoint( 11 ).GetValence( ) != 4 )
    {
    std::cout << "FAILED (for, wrong valence of "
              << mesh->GetPoint( 11 ).GetValence( )
              << " for vertex 11 )." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << ".OK" << std::endl;
  //test antenna
  CreateSquareTriangularMesh<MeshType>( mesh );
  std::cout << "     ";
  std::cout << "Try to split antenna (impossible).";

  splitVertex->SetInput( mesh );
  if( splitVertex->Evaluate( mesh->FindEdge( 12, 17 ),
                             mesh->FindEdge( 12, 17 ) ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << ".OK" << std::endl;
  //test different dest( )
  CreateSquareTriangularMesh<MeshType>( mesh );
  std::cout << "     ";
  std::cout << "Test with args with different Dest( ) (Impossible).";

  splitVertex->SetInput( mesh );
  if( splitVertex->Evaluate( mesh->FindEdge(  5, 11 ),
                              mesh->FindEdge(  5,  6 ) ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  if( ! AssertTopologicalInvariants< MeshType >
          ( mesh, 25, 56, 32, 1, 0 ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  if ( mesh->GetPoint( 5 ).GetValence( ) != 4 )
    {
    std::cout << "FAILED, wrong valence of "
    << mesh->GetPoint( 5 ).GetValence( )
    << " for vertex 5." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << ".OK" << std::endl;
  std::cout << "Checking SplitVertex." << std::endl << std::endl;

  std::cout << "Checking JoinVertex( SplitVertex()) Invariance.";

  CreateSquareTriangularMesh<MeshType>( mesh );

  splitVertex->SetInput( mesh );

  JoinVertex::Pointer joinVertex = JoinVertex::New();
  joinVertex->SetInput( mesh );
  if( !joinVertex->Evaluate( splitVertex->Evaluate( mesh->FindEdge(  5, 11 ),
                                                    mesh->FindEdge( 17, 11 ) )) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  mesh->DeletePoint( joinVertex->GetOldPointID( ) );
  if( ! AssertTopologicalInvariants< MeshType >
          ( mesh, 25, 56, 32, 1, 0 ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << ".OK" << std::endl << std::endl;

  return EXIT_SUCCESS;
}
