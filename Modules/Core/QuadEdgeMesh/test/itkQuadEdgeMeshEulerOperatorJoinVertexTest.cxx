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

#include "itkQuadEdgeMeshEulerOperatorJoinVertexFunction.h"
#include "itkQuadEdgeMeshEulerOperatorsTestHelper.h"

int itkQuadEdgeMeshEulerOperatorJoinVertexTest( int argc, char * argv[] )
{
  if( argc < 2 )
  {
    std::cout <<"*** ERROR ***" <<std::endl;
    std::cout <<"Requires one argument: " <<std::endl;
    std::cout <<" 0-Test with a square mesh with only triangles." <<std::endl;
    std::cout <<" 1-Test with a square mesh with only quads." <<std::endl;
    std::cout <<" 2-Test with a tetraedron mesh with triangles." <<std::endl;
    std::cout <<" 3-Test with a samosa." <<std::endl;
    std::cout <<" 4-Test with an isolated edge." <<std::endl;
    std::cout <<" 5-Test with a square mesh with one centered hole" <<std::endl;
    return EXIT_FAILURE;
  }

  int InputType;
  std::stringstream ssout( argv[1] );
  ssout >>InputType;

  if( ( InputType > 5 ) || ( InputType < 0 ) )
  {
    std::cout <<"*** ERROR ***" <<std::endl;
    std::cout <<"Requires one argument: " <<std::endl;
    std::cout <<" 0-Test with a square mesh with only triangles." <<std::endl;
    std::cout <<" 1-Test with a square mesh with only quads." <<std::endl;
    std::cout <<" 2-Test with a tetraedron mesh with triangles." <<std::endl;
    std::cout <<" 3-Test with a samosa." <<std::endl;
    std::cout <<" 4-Test with an isolated edge." <<std::endl;
    std::cout <<" 5-Test with a square mesh with one centered hole" <<std::endl;
    return EXIT_FAILURE;
  }

  typedef itk::QuadEdgeMesh< double, 3 >                      MeshType;
  typedef MeshType::Pointer                                   MeshPointer;
  typedef MeshType::QEType                                    QEType;
  typedef MeshType::PointIdentifier                           PointIdentifier;
  typedef MeshType::CellType                                  CellType;
  typedef itk::QuadEdgeMeshLineCell< CellType >               LineCellType;

  typedef itk::QuadEdgeMeshEulerOperatorJoinVertexFunction< MeshType, QEType>
    JoinVertexType;

  MeshPointer mesh = MeshType::New();
  PointIdentifier start_id( 12 );

  switch( InputType )
    {
    case 0:
      // The initial configuration and numbering of simpleSquare.vtk:
      //    Vertices: 25 , Edges: 56, Faces: 32, Boundary = 1, Chi = 1
      //
      //   20 --------- 21 --------- 22 --------- 23 --------- 24
      //    |        __/ |        __/ |        __/ |        __/ |
      //    |     __/    |     __/    |     __/    |     __/    |
      //    |  __/       |  __/       |  __/       |  __/       |
      //    | /          | /          | /          | /          |
      //   15 --------- 16 --------- 17 --------- 18 --------- 19
      //    |        __/ |        __/ |        __/ |        __/ |
      //    |     __/    |     __/    |     __/    |     __/    |
      //    |  __/       |  __/       |  __/       |  __/       |
      //    | /          | /          | /          | /          |
      //   10 --------- 11 --------- 12 --------- 13 --------- 14
      //    |        __/ |        __/ |        __/ |        __/ |
      //    |     __/    |     __/    |     __/    |     __/    |
      //    |  __/       |  __/       |  __/       |  __/       |
      //    | /          | /          | /          | /          |
      //    5 ---------- 6 ---------- 7 ---------- 8 ---------  9
      //    |        __/ |        __/ |        __/ |        __/ |
      //    |     __/    |     __/    |     __/    |     __/    |
      //    |  __/       |  __/       |  __/       |  __/       |
      //    | /          | /          | /          | /          |
      //    0 ---------- 1 ---------- 2  --------- 3 ---------  4
      CreateSquareTriangularMesh< MeshType >( mesh );
      break;
    case 1:
      // The initial configuration and numbering of simpleSquare.vtk:
      //    Vertices: 25 , Edges: 56, Faces: 32, Boundary = 1, Chi = 1
      //
      //   20 --------- 21 --------- 22 --------- 23 --------- 24
      //    |            |            |            |            |
      //    |            |            |            |            |
      //    |            |            |            |            |
      //    |            |            |            |            |
      //   15 --------- 16 --------- 17 --------- 18 --------- 19
      //    |            |            |            |            |
      //    |            |            |            |            |
      //    |            |            |            |            |
      //    |            |            |            |            |
      //   10 --------- 11 --------- 12 --------- 13 --------- 14
      //    |            |            |            |            |
      //    |            |            |            |            |
      //    |            |            |            |            |
      //    |            |            |            |            |
      //    5 ---------- 6 ---------- 7 ---------- 8 ---------  9
      //    |            |            |            |            |
      //    |            |            |            |            |
      //    |            |            |            |            |
      //    |            |            |            |            |
      //    0 ---------- 1 ---------- 2  --------- 3 ---------  4
      CreateSquareQuadMesh< MeshType >( mesh );
      break;
    case 2:
      CreateTetraedronMesh< MeshType >( mesh );
      start_id = 0;
      break;
    case 3:
      CreateSamosa< MeshType >( mesh );
      start_id = 0;
      break;
    case 4:
      //   20 --------- 21 --------- 22 --------- 23 --------- 24
      //    |        __/ |        __/ |        __/ |        __/ |
      //    |     __/    |     __/    |     __/    |     __/    |
      //    |  __/       |  __/       |  __/       |  __/       |
      //    | /          | /          | /          | /          |
      //   15 --------- 16 --------- 17 --------- 18 --------- 19
      //    |        __/ |        __/ |            |        __/ |
      //    |     __/    |     __/    |            |     __/    |
      //    |  __/       |  __/       |            |  __/       |
      //    | /          | /          |            | /          |
      //   10 --------- 11           12           13 --------- 14
      //    |        __/ |                         |        __/ |
      //    |     __/    |                         |     __/    |
      //    |  __/       |                         |  __/       |
      //    | /          |                         | /          |
      //    5 ---------- 6 ---------- 7 ---------- 8 ---------  9
      //    |        __/ |        __/ |        __/ |        __/ |
      //    |     __/    |     __/    |     __/    |     __/    |
      //    |  __/       |  __/       |  __/       |  __/       |
      //    | /          | /          | /          | /          |
      //    0 ---------- 1 ---------- 2  --------- 3 ---------  4
      CreateSquareTriangularMesh< MeshType >( mesh );
      mesh->LightWeightDeleteEdge( mesh->FindEdge( 11, 12 ) );
      mesh->LightWeightDeleteEdge( mesh->FindEdge(  6, 12 ) );
      mesh->LightWeightDeleteEdge( mesh->FindEdge(  7, 12 ) );
      mesh->LightWeightDeleteEdge( mesh->FindEdge(  7, 13 ) );
      mesh->LightWeightDeleteEdge( mesh->FindEdge( 12, 13 ) );
      mesh->LightWeightDeleteEdge( mesh->FindEdge( 12, 18 ) );
      mesh->AddFace( mesh->FindEdge( 7, 8 ) );
      break;
    case 5:
      //   20 --------- 21 --------- 22 --------- 23 --------- 24
      //    |        __/ |        __/ |        __/ |        __/ |
      //    |     __/    |     __/    |     __/    |     __/    |
      //    |  __/       |  __/       |  __/       |  __/       |
      //    | /          | /          | /          | /          |
      //   15 --------- 16 --------- 17 --------- 18 --------- 19
      //    |        __/ |                         |        __/ |
      //    |     __/    |                         |     __/    |
      //    |  __/       |                         |  __/       |
      //    | /          |                         | /          |
      //   10 --------- 11                        13 --------- 14
      //    |        __/ |                         |        __/ |
      //    |     __/    |                         |     __/    |
      //    |  __/       |                         |  __/       |
      //    | /          |                         | /          |
      //    5 ---------- 6 ---------- 7 ---------- 8 ---------  9
      //    |        __/ |        __/ |        __/ |        __/ |
      //    |     __/    |     __/    |     __/    |     __/    |
      //    |  __/       |  __/       |  __/       |  __/       |
      //    | /          | /          | /          | /          |
      //    0 ---------- 1 ---------- 2  --------- 3 ---------  4
      CreateSquareTriangularMesh< MeshType >( mesh );
      mesh->LightWeightDeleteEdge( mesh->FindEdge( 11, 12 ) );
      mesh->LightWeightDeleteEdge( mesh->FindEdge( 11, 17 ) );
      mesh->LightWeightDeleteEdge( mesh->FindEdge(  6, 12 ) );
      mesh->LightWeightDeleteEdge( mesh->FindEdge(  7, 12 ) );
      mesh->LightWeightDeleteEdge( mesh->FindEdge(  7, 13 ) );
      mesh->LightWeightDeleteEdge( mesh->FindEdge( 12, 13 ) );
      mesh->LightWeightDeleteEdge( mesh->FindEdge( 12, 18 ) );
      mesh->LightWeightDeleteEdge( mesh->FindEdge( 12, 17 ) );
      start_id = 17;
      break;
    }

  JoinVertexType::Pointer joinVertex = JoinVertexType::New( );

  std::cout << joinVertex->GetNameOfClass() << std::endl;
  std::cout << joinVertex << std::endl;


#ifndef NDEBUG
  std::cout << "     " << "Test No Mesh Input.";
  if( joinVertex->Evaluate( (QEType*)1 ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "OK" << std::endl;
#endif

  (void)joinVertex->GetNameOfClass();

  joinVertex->SetInput( mesh );

  std::cout << "     " << "Test QE Input and Sym isolated.";
  LineCellType* IsolatedLineCell = new LineCellType;
  if( joinVertex->Evaluate( IsolatedLineCell->GetQEGeom( ) ) )
    {
    std::cout << "FAILED." << std::endl;
    delete IsolatedLineCell;
    return EXIT_FAILURE;
    }
  delete IsolatedLineCell;
  std::cout << "OK" << std::endl;

#ifndef NDEBUG
  std::cout << "     " << "Test No QE Input.";
  if( joinVertex->Evaluate( (QEType*)0 ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "OK" << std::endl;
#endif

  QEType* qe = mesh->FindEdge( start_id );
  int kk( 0 );

  typedef itk::QuadEdgeMeshTopologyChecker< MeshType > CheckerType;
  CheckerType::Pointer check = CheckerType::New();

  while( qe != ITK_NULLPTR )
    {
    joinVertex->SetInput( mesh );
    PointIdentifier id_org = qe->GetOrigin();
    PointIdentifier id_dest = qe->GetDestination();

    QEType* qe_output = joinVertex->Evaluate( qe );
    JoinVertexType::EdgeStatusType status = joinVertex->GetEdgeStatus();

    std::cout <<"*** " <<kk <<" ***" <<std::endl;
    std::cout <<"org: " <<id_org <<" " <<"dest: " <<id_dest <<" " <<status <<std::endl;

    if( ( status == JoinVertexType::EDGE_JOINING_DIFFERENT_BORDERS ) ||
        ( status == JoinVertexType::SAMOSA_CONFIG ) ||
        ( status == JoinVertexType::TETRAHEDRON_CONFIG ) )
      {
      break;
      }

    if( !qe_output )
      {
      std::cout <<"Number of Edges: " <<mesh->GetNumberOfEdges() <<" " <<std::endl;
      std::cout <<"Number of Faces: " <<mesh->GetNumberOfFaces() <<std::endl;
      std::cout <<"FAILED." <<std::endl;
      return EXIT_FAILURE;
      }

    PointIdentifier old_id = joinVertex->GetOldPointID();

    mesh->DeletePoint( old_id );

    check->SetMesh( mesh );

    if( check->ValidateEulerCharacteristic( ) )
      {
      std::cout <<"OK" <<std::endl;
      }
    else
      {
      std::cout <<"EULER FALSE" <<std::endl;
      return EXIT_FAILURE;
      }

    mesh->SqueezePointsIds();
    qe = qe_output;
    kk++;
    }

  /*
  std::cout << "     " << "Test Topological Changes";
  MeshType::Pointer topoTest = MeshType::New( );
  MeshType::PointType pts2[4];
  pts2[ 0][0] = 0.0;  pts2[ 0][1] = 0.0;  pts2[ 0][2] = 0.0;
  pts2[ 1][0] = 1.0;  pts2[ 1][1] = 0.0;  pts2[ 1][2] = 0.0;
  pts2[ 2][0] = 0.0;  pts2[ 2][1] = 1.0;  pts2[ 2][2] = 0.0;
  pts2[ 3][0] = 0.0;  pts2[ 3][1] = 0.0;  pts2[ 3][2] = 1.0;
  for(int ii=0; ii<4; ii++)
    {
    topoTest->SetPoint( ii, pts2[ii] );
    }
  MeshType::PointIdList topoTestpoints;

  topoTestpoints.push_back( 3 );
  topoTestpoints.push_back( 0 );
  topoTestpoints.push_back( 1 );
  topoTest->AddFace( topoTestpoints );
  topoTestpoints.clear( );

  topoTestpoints.push_back( 3 );
  topoTestpoints.push_back( 2 );
  topoTestpoints.push_back( 0 );
  topoTest->AddFace( topoTestpoints );
  topoTestpoints.clear( );

  topoTestpoints.push_back( 3 );
  topoTestpoints.push_back( 1 );
  topoTestpoints.push_back( 2 );
  topoTest->AddFace( topoTestpoints );
  topoTestpoints.clear( );

  topoTestpoints.push_back( 0 );
  topoTestpoints.push_back( 2 );
  topoTestpoints.push_back( 1 );
  topoTest->AddFace( topoTestpoints );
  topoTestpoints.clear( );

  joinVertex->SetInput( topoTest );
  if( joinVertex->Evaluate( topoTest->FindEdge( 0, 1 ) ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "OK" << std::endl;
  joinVertex->SetInput( mesh );

  // First Test the case were the argument is an internal edge (here
  // we consider [12, 11]) whose adjacent faces are both internal (i.e.
  // not adjacent to the boundary). Notice that on topological grounds
  // and in this context applying JoinVertex( [12, 11] ) or
  // JoinVertex( [11, 12] ) is the same thing. But of course the
  // geometrical result is not the same. Here is was we must obtain:
  //
  //   20 --------- 21 --------- 22 --------- 23 --------- 24
  //    |        __/ |        __/ |        __/ |        __/ |
  //    |     __/    |     __/    |     __/    |     __/    |
  //    |  __/       |  __/       |  __/       |  __/       |
  //    | /          | /          | /          | /          |
  //   15 --------- 16 --------- 17 --------- 18 --------- 19
  //    |        __/ |        __/       _____/ |        __/ |
  //    |     __/    |     __/     ____/       |     __/    |
  //    |  __/       |  __/   ____/            |  __/       |
  //    | /          | /_____/                 | /          |
  //   10 --------- 11 ---------------------- 13 --------- 14
  //    |        __/ | \__                 __/ |        __/ |
  //    |     __/    |    \__           __/    |     __/    |
  //    |  __/       |       \__     __/       |  __/       |
  //    | /          |          \   /          | /          |
  //    5 ---------- 6 ---------- 7 ---------- 8 ---------  9
  //    |        __/ |        __/ |        __/ |        __/ |
  //    |     __/    |     __/    |     __/    |     __/    |
  //    |  __/       |  __/       |  __/       |  __/       |
  //    | /          | /          | /          | /          |
  //    0 ---------- 1 ---------- 2  --------- 3 ---------  4
  //
  std::cout << "     ";
  std::cout << "Join v of internal edge with internal Left and Right (possible).";

  if( !joinVertex->Evaluate( mesh->FindEdge( 12, 11 ) ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  mesh->DeletePoint( joinVertex->GetOldPointID( ) );
  if( ! AssertTopologicalInvariants< MeshType >
          ( mesh, 24, 53, 30, 1, 0 ) )
    {
    std::cout << "FAILED (for [ 12, 11 ] )." << std::endl;
    return EXIT_FAILURE;
    }
  if ( mesh->GetPoint( 11 ).GetValence( ) != 8 )
    {
    std::cout << "FAILED (for [ 12, 11 ], wrong valence of "
              << mesh->GetPoint( 11 ).GetValence( )
              << " for vertex 11 )." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "OK" << std::endl;
  // Second Test with an internal edge (here we consider [0, 6]) whose
  // both adjacent faces are themselves adjacent to the boundary of the
  // surface. Here is the result:
  //
  //   20 --------- 21 --------- 22 --------- 23 --------- 24
  //    |        __/ |        __/ |        __/ |        __/ |
  //    |     __/    |     __/    |     __/    |     __/    |
  //    |  __/       |  __/       |  __/       |  __/       |
  //    | /          | /          | /          | /          |
  //   15 --------- 16 --------- 17 --------- 18 --------- 19
  //    |        __/ |        __/ |        __/ |        __/ |
  //    |     __/    |     __/    |     __/    |     __/    |
  //    |  __/       |  __/       |  __/       |  __/       |
  //    | /          | /          | /          | /          |
  //   10 --------- 11 --------- 12 --------- 13 --------- 14
  //    |        __/ |        __/ |        __/ |        __/ |
  //    |     __/    |     __/    |     __/    |     __/    |
  //    |  __/       |  __/       |  __/       |  __/       |
  //    | /          | /          | /          | /          |
  //    5 ---------- 6 ---------- 7 ---------- 8 ---------  9
  //                 |        __/ |        __/ |        __/ |
  //                 |     __/    |     __/    |     __/    |
  //                 |  __/       |  __/       |  __/       |
  //                 | /          | /          | /          |
  //                 1 ---------- 2  --------- 3 ---------  4
  //
  PopulateMesh<MeshType>( mesh );
  std::cout << "     ";
  std::cout << "Join v of internal edge with Boundary Left and Right (possible).";
  joinVertex->SetInput( mesh );
  if( !joinVertex->Evaluate( mesh->FindEdge( 0, 6 ) ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  mesh->DeletePoint( joinVertex->GetOldPointID( ) );
  if( ! AssertTopologicalInvariants< MeshType >
          ( mesh, 24, 53, 30, 1, 0 ) )
    {
    std::cout << "FAILED (for [ 0, 6 ] )." << std::endl;
    return EXIT_FAILURE;
    }
  QEType* sixEdge = mesh->FindEdge( 6, 5 );
  if( ! sixEdge || ! sixEdge->IsAtBorder( ) )
    {
    std::cout << "FAILED (for [ 0, 6 ], vertex 6 not on boundary )."
              << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "OK" << std::endl;
  // We now consider a boundary edge (here we consider [0, 5] but still
  // with a an adjacent face.
  //
  //   20 --------- 21 --------- 22 --------- 23 --------- 24
  //    |        __/ |        __/ |        __/ |        __/ |
  //    |     __/    |     __/    |     __/    |     __/    |
  //    |  __/       |  __/       |  __/       |  __/       |
  //    | /          | /          | /          | /          |
  //   15 --------- 16 --------- 17 --------- 18 --------- 19
  //    |        __/ |        __/ |        __/ |        __/ |
  //    |     __/    |     __/    |     __/    |     __/    |
  //    |  __/       |  __/       |  __/       |  __/       |
  //    | /          | /          | /          | /          |
  //   10 --------- 11 --------- 12 --------- 13 --------- 14
  //    |        __/ |        __/ |        __/ |        __/ |
  //    |     __/    |     __/    |     __/    |     __/    |
  //    |  __/       |  __/       |  __/       |  __/       |
  //    | /          | /          | /          | /          |
  //    5 ---------- 6 ---------- 7 ---------- 8 ---------- 9
  //      \__        |        __/ |        __/ |        __/ |
  //         \__     |     __/    |     __/    |     __/    |
  //            \__  |  __/       |  __/       |  __/       |
  //               \ | /          | /          | /          |
  //                 1 ---------- 2 ---------- 3 ---------- 4
  //
  PopulateMesh<MeshType>( mesh );
  std::cout << "     ";
  std::cout << "Join v of border edge with only Right set (possible).";
  joinVertex->SetInput( mesh );
  if( !joinVertex->Evaluate( mesh->FindEdge( 0, 5 ) ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  mesh->DeletePoint( joinVertex->GetOldPointID( ) );

  if( ! AssertTopologicalInvariants< MeshType >
          ( mesh, 24, 54, 31, 1, 0 ) )
    {
    std::cout << "FAILED (for [ 0, 5 ] )." << std::endl;
    return EXIT_FAILURE;
    }
   std::cout << "OK" << std::endl;
  // Just to make sure, we now consider a boundary edge with the surface
  // on it's left (as opposed to the previous one which had the surface
  // on it's right) i.e. we consider [2, 3]:
  //
  //   20 --------- 21 --------- 22 --------- 23 --------- 24
  //    |        __/ |        __/ |        __/ |        __/ |
  //    |     __/    |     __/    |     __/    |     __/    |
  //    |  __/       |  __/       |  __/       |  __/       |
  //    | /          | /          | /          | /          |
  //   15 --------- 16 --------- 17 --------- 18 --------- 19
  //    |        __/ |        __/ |        __/ |        __/ |
  //    |     __/    |     __/    |     __/    |     __/    |
  //    |  __/       |  __/       |  __/       |  __/       |
  //    | /          | /          | /          | /          |
  //   10 --------- 11 --------- 12 --------- 13 --------- 14
  //    |        __/ |        __/ |        __/ |        __/ |
  //    |     __/    |     __/    |     __/    |     __/    |
  //    |  __/       |  __/       |  __/       |  __/       |
  //    | /          | /          | /          | /          |
  //    5 ---------- 6 ---------- 7 ---------- 8 ---------  9
  //    |        __/ |        __/   \__        |        __/ |
  //    |     __/    |     __/         \__     |     __/    |
  //    |  __/       |  __/               \__  |  __/       |
  //    | /          | /                     \ | /          |
  //    0 ---------- 1 ----------------------- 3 ---------  4
  //

  PopulateMesh<MeshType>( mesh );

  std::cout << "     ";
  std::cout << "Join v of border edge with only Left set (possible).";
  joinVertex->SetInput( mesh );
  if( !joinVertex->Evaluate( mesh->FindEdge( 2, 3 ) ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  mesh->DeletePoint( joinVertex->GetOldPointID( ) );

  if( ! AssertTopologicalInvariants< MeshType >
          ( mesh, 24, 54, 31, 1, 0 ) )
    {
    std::cout << "FAILED (for [ 2, 3 ] )." << std::endl;
    return EXIT_FAILURE;
    }
  if ( mesh->GetPoint( 3 ).GetValence( ) != 5 )
    {
    std::cout << "FAILED (for [ 2, 3 ], wrong valence of "
               <<  mesh->GetPoint( 3 ).GetValence( )
               << " for vertex 3 )." << std::endl;
    return EXIT_FAILURE;
    }
   std::cout << "OK" << std::endl;
  // Now try with a wire edge (a pathological edge which has no face
  // neither on it's left nor on it's right).
  // Create this situation by manually deleting two faces sharing the
  // same edge which will thus become a wire edge.
  //
  //   20 --------- 21 --------- 22 --------- 23 --------- 24
  //    |        __/ |        __/ |        __/ |        __/ |
  //    |     __/    |     __/    |     __/    |     __/    |
  //    |  __/       |  __/       |  __/       |  __/       |
  //    | /          | /          | /          | /          |
  //   15 --------- 16 --------- 17 --------- 18 --------- 19
  //    |        __/ |        __/   \__        |        __/ |
  //    |     __/    |     __/         \__     |     __/    |
  //    |  __/       |  __/               \__  |  __/       |
  //    | /          | /                     \ | /          |
  //   10 --------- 11 ---------------------- 13 --------- 14
  //    |        __/ |                ______// |        __/ |
  //    |     __/    |         ______/   ___/  |     __/    |
  //    |  __/       |  ______/      ___/      |  __/       |
  //    | /          | /            /          | /          |
  //    5 ---------- 6 ---------- 7 ---------- 8 ---------  9
  //    |        __/ |        __/ |        __/ |        __/ |
  //    |     __/    |     __/    |     __/    |     __/    |
  //    |  __/       |  __/       |  __/       |  __/       |
  //    | /          | /          | /          | /          |
  //    0 ---------- 1 ---------- 2  --------- 3 ---------  4
  //
  PopulateMesh<MeshType>( mesh );
  std::cout << "     ";
  std::cout << "Join v of wired edge (possible).";

  ///NOTE temporary
  std::cout <<std::endl;
  std::cout <<"NOTE: this Test is not performed for the time being.";
  std::cout <<"Discussion with Alex regarding this Test" <<std::endl;

  // Instead of adjacent triangular faces we now consider bigger
  // faces. Here is the initial situation:
  //
  //   20 --------- 21 --------- 22 --------- 23 --------- 24
  //    |        __/ |        __/ |        __/ |        __/ |
  //    |     __/    |     __/    |     __/    |     __/    |
  //    |  __/       |  __/       |  __/       |  __/       |
  //    | /          | /          | /          | /          |
  //   15 --------- 16 --------- 17 --------- 18 --------- 19
  //    |        __/ |            |            |        __/ |
  //    |     __/    |            |            |     __/    |
  //    |  __/       |            |            |  __/       |
  //    | /          |            |            | /          |
  //   10 --------- 11           12           13 --------- 14
  //    |        __/ |        __/ |            |        __/ |
  //    |     __/    |     __/    |            |     __/    |
  //    |  __/       |  __/       |            |  __/       |
  //    | /          | /          |            | /          |
  //    5 ---------- 6 ---------- 7 ---------- 8 ---------  9
  //    |        __/ |        __/ |        __/ |        __/ |
  //    |     __/    |     __/    |     __/    |     __/    |
  //    |  __/       |  __/       |  __/       |  __/       |
  //    | /          | /          | /          | /          |
  //    0 ---------- 1 ---------- 2  --------- 3 ---------  4
  //
  PopulateMesh<MeshType>( mesh );
  std::cout << "     ";
  std::cout << "Join v of internal edge with polygonal Faces (possible).";
  joinVertex->SetInput( mesh );
  mesh->LightWeightDeleteEdge( mesh->FindEdgeCell( 11, 17 ) );
  mesh->LightWeightDeleteEdge( mesh->FindEdgeCell( 11, 12 ) );
  mesh->LightWeightDeleteEdge( mesh->FindEdgeCell( 12, 18 ) );
  mesh->LightWeightDeleteEdge( mesh->FindEdgeCell( 12, 13 ) );
  mesh->LightWeightDeleteEdge( mesh->FindEdgeCell(  7, 13 ) );
  mesh->AddFace( mesh->FindEdge(  8, 13 ) );
  mesh->AddFace( mesh->FindEdge( 12, 17 ) );
  if( !joinVertex->Evaluate( mesh->FindEdge( 12, 17 ) ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  mesh->DeletePoint( joinVertex->GetOldPointID( ) );

  // And here is what we should obtain:
  //
  //   20 --------- 21 --------- 22 --------- 23 --------- 24
  //    |        __/ |        __/ |        __/ |        __/ |
  //    |     __/    |     __/    |     __/    |     __/    |
  //    |  __/       |  __/       |  __/       |  __/       |
  //    | /          | /          | /          | /          |
  //   15 --------- 16 --------- 17 --------- 18 --------- 19
  //    |        __/ |          / |            |        __/ |
  //    |     __/    |         /  |            |     __/    |
  //    |  __/       |        /   |            |  __/       |
  //    | /          |       /    |            | /          |
  //   10 --------- 11     _/     |           13 --------- 14
  //    |        __/ |    /       |            |        __/ |
  //    |     __/    |   /        |            |     __/    |
  //    |  __/       |  /         |            |  __/       |
  //    | /          | /          |            | /          |
  //    5 ---------- 6 ---------- 7 ---------- 8 ---------  9
  //    |        __/ |        __/ |        __/ |        __/ |
  //    |     __/    |     __/    |     __/    |     __/    |
  //    |  __/       |  __/       |  __/       |  __/       |
  //    | /          | /          | /          | /          |
  //    0 ---------- 1 ---------- 2  --------- 3 ---------  4
  //
  if( ! AssertTopologicalInvariants< MeshType >
          ( mesh, 24, 50, 27, 1, 0 ) )
    {
    std::cout << "FAILED (for [ 12, 13 ] )." << std::endl;
    return EXIT_FAILURE;
    }
  // Push it one step further: we should obtain
  //
  //   20 --------- 21 --------- 22 --------- 23 --------- 24
  //    |        __/ |        __/ |        __/ |        __/ |
  //    |     __/    |     __/    |     __/    |     __/    |
  //    |  __/       |  __/       |  __/       |  __/       |
  //    | /          | /          | /          | /          |
  //   15 --------- 16 --------- 17 --------- 18 --------- 19
  //    |        __/ |          //| \          |        __/ |
  //    |     __/    |         // |  \         |     __/    |
  //    |  __/       |        / | |   \        |  __/       |
  //    | /          |       / /  |    \       | /          |
  //   10 --------- 11     _/ |   |     \_    13 --------- 14
  //    |        __/ |    /  /    |       \    |        __/ |
  //    |     __/    |   /  |     |        \   |     __/    |
  //    |  __/       |  /   |     |         \  |  __/       |
  //    | /          | /    |     |          \ | /          |
  //    5 ---------- 6     /      |            8 ---------  9
  //    |        __/ |    |       |        __/ |        __/ |
  //    |     __/    |   /        |     __/    |     __/    |
  //    |  __/       |  |         |  __/       |  __/       |
  //    | /          | /          | /          | /          |
  //    0 ---------- 1 ---------- 2  --------- 3 ---------  4
  //
  mesh->LightWeightDeleteEdge( mesh->FindEdge(  6, 17 ) );
  mesh->AddFace( mesh->FindEdge(  7, 17 ) );
  if( !joinVertex->Evaluate( mesh->FindEdge( 7, 17 ) ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  mesh->DeletePoint( joinVertex->GetOldPointID( ) );

  if( ! AssertTopologicalInvariants< MeshType >
          ( mesh, 23, 48, 26, 1, 0 ) )
    {
    std::cout << "FAILED (for [ 12, 13 ] )." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << ".OK" << std::endl;
  // Consider a last pathological Test of an edge with an isolated
  // end potientaly immersed in a face. Here is the initial situation:
  //
  //   20 --------- 21 --------- 22 --------- 23 --------- 24
  //    |        __/ |        __/ |        __/ |        __/ |
  //    |     __/    |     __/    |     __/    |     __/    |
  //    |  __/       |  __/       |  __/       |  __/       |
  //    | /          | /          | /          | /          |
  //   15 --------- 16 --------- 17 --------- 18 --------- 19
  //    |        __/ |        __/ |            |        __/ |
  //    |     __/    |     __/    |            |     __/    |
  //    |  __/       |  __/       |            |  __/       |
  //    | /          | /          |            | /          |
  //   10 --------- 11           12           13 --------- 14
  //    |        __/ |                         |        __/ |
  //    |     __/    |                         |     __/    |
  //    |  __/       |                         |  __/       |
  //    | /          |                         | /          |
  //    5 ---------- 6 ---------- 7 ---------- 8 ---------  9
  //    |        __/ |        __/ |        __/ |        __/ |
  //    |     __/    |     __/    |     __/    |     __/    |
  //    |  __/       |  __/       |  __/       |  __/       |
  //    | /          | /          | /          | /          |
  //    0 ---------- 1 ---------- 2  --------- 3 ---------  4
  //
  //    Vertices: 25 , Edges: 56, Faces: 32, Boundary = 1, Chi = 1
  PopulateMesh<MeshType>( mesh );

  std::cout << "     ";
  std::cout << "Join v of antenna - version 1 (possible).";
  joinVertex->SetInput( mesh );
  mesh->LightWeightDeleteEdge( mesh->FindEdge( 11, 12 ) );
  mesh->LightWeightDeleteEdge( mesh->FindEdge(  6, 12 ) );
  mesh->LightWeightDeleteEdge( mesh->FindEdge(  7, 12 ) );
  mesh->LightWeightDeleteEdge( mesh->FindEdge(  7, 13 ) );
  mesh->LightWeightDeleteEdge( mesh->FindEdge( 12, 13 ) );
  mesh->LightWeightDeleteEdge( mesh->FindEdge( 12, 18 ) );
  mesh->AddFace( mesh->FindEdge( 7, 8 ) );
  if( !joinVertex->Evaluate( mesh->FindEdge( 12, 17 ) ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  mesh->DeletePoint( joinVertex->GetOldPointID( ) );
  if( ! AssertTopologicalInvariants< MeshType >
          ( mesh, 24, 49, 26, 1, 0 ) )
    {
    std::cout << "FAILED (for antenna - version 1)." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "OK" << std::endl;

  PopulateMesh<MeshType>( mesh );

  std::cout << "     ";
  std::cout << "Join v of antenna - version 2 (possible).";
  joinVertex->SetInput( mesh );
  mesh->LightWeightDeleteEdge( mesh->FindEdge( 11, 12 ) );
  mesh->LightWeightDeleteEdge( mesh->FindEdge(  6, 12 ) );
  mesh->LightWeightDeleteEdge( mesh->FindEdge(  7, 12 ) );
  mesh->LightWeightDeleteEdge( mesh->FindEdge(  7, 13 ) );
  mesh->LightWeightDeleteEdge( mesh->FindEdge( 12, 13 ) );
  mesh->LightWeightDeleteEdge( mesh->FindEdge( 12, 18 ) );
  mesh->AddFace( mesh->FindEdge( 7, 8 ) );
  if( !joinVertex->Evaluate( mesh->FindEdge( 12, 17 )->GetSym( ) ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  mesh->DeletePoint( joinVertex->GetOldPointID( ) );
  if( ! AssertTopologicalInvariants< MeshType >
          ( mesh, 24, 49, 26, 1, 0 ) )
    {
    std::cout << "FAILED (for antenna - version 2)." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "OK" << std::endl; */

  std::cout << "Checking JoinVertex." << "OK" << std::endl << std::endl;

  return EXIT_SUCCESS;
}
