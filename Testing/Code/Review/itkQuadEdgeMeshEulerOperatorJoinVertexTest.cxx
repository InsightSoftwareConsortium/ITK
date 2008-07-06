#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <string>

#include "itkQuadEdgeMesh.h"
#include "itkQuadEdgeMeshLineCell.h"
#include "itkQuadEdgeMeshPolygonCell.h"

#include "itkQuadEdgeMeshFunctionBase.h"
#include "itkQuadEdgeMeshEulerOperatorJoinVertexFunction.h"
#include "itkQuadEdgeMeshEulerOperatorsTestHelper.h"

int itkQuadEdgeMeshEulerOperatorJoinVertexTest(int argc, char* argv[] )
{
  (void)argc;
  (void)argv;
  
  typedef itk::QuadEdgeMesh< double, 3 >                      MeshType;
  typedef MeshType::Pointer                                   MeshPointer;
  typedef MeshType::QEType                                    QEType;
  typedef MeshType::PointIdentifier                           PointIdentifier;
  typedef MeshType::PointType                                 PointType;
  typedef MeshType::CellType                                  CellType;
  typedef itk::QuadEdgeMeshLineCell< CellType >               LineCellType;

  typedef itk::QuadEdgeMeshEulerOperatorJoinVertexFunction< MeshType, QEType>
    JoinVertex;
  /////////////////////////////////////////
  //
  //          Join Vertex
  //
  /////////////////////////////////////////
  std::cout << "Checking JoinVertex." << std::endl;
  MeshPointer mesh = MeshType::New();
  PopulateMesh<MeshType>( mesh );
 
  JoinVertex::Pointer joinVertex = JoinVertex::New( );

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
    return EXIT_FAILURE;
    }
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

  std::cout << "     " << "Test Topological Changes";
  MeshType::Pointer topotest = MeshType::New( );
  MeshType::PointType pts2[4];
  pts2[ 0][0] = 0.0;  pts2[ 0][1] = 0.0;  pts2[ 0][2] = 0.0;
  pts2[ 1][0] = 1.0;  pts2[ 1][1] = 0.0;  pts2[ 1][2] = 0.0;
  pts2[ 2][0] = 0.0;  pts2[ 2][1] = 1.0;  pts2[ 2][2] = 0.0;
  pts2[ 3][0] = 0.0;  pts2[ 3][1] = 0.0;  pts2[ 3][2] = 1.0;
  for(int ii=0; ii<4; ii++)
    {
    topotest->SetPoint( ii, pts2[ii] );
    }
  MeshType::PointIdList topotestpoints;

  topotestpoints.push_back( 3 );
  topotestpoints.push_back( 0 );
  topotestpoints.push_back( 1 );
  topotest->AddFace( topotestpoints );
  topotestpoints.clear( );

  topotestpoints.push_back( 3 );
  topotestpoints.push_back( 2 );
  topotestpoints.push_back( 0 );
  topotest->AddFace( topotestpoints );
  topotestpoints.clear( );

  topotestpoints.push_back( 3 );
  topotestpoints.push_back( 1 );
  topotestpoints.push_back( 2 );
  topotest->AddFace( topotestpoints );
  topotestpoints.clear( );

  topotestpoints.push_back( 0 );
  topotestpoints.push_back( 2 );
  topotestpoints.push_back( 1 );
  topotest->AddFace( topotestpoints );
  topotestpoints.clear( );

  joinVertex->SetInput( topotest );
  if( joinVertex->Evaluate( topotest->FindEdge( 0, 1 ) ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "OK" << std::endl;
  joinVertex->SetInput( mesh );

  // First test the case were the argument is an internal edge (here
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
  // Second test with an internal edge (here we consider [0, 6]) whose
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
  joinVertex->SetInput( mesh );
  mesh->DeleteFace( mesh->FindEdge( 12, 13 )->GetLeft( ) );
  mesh->DeleteFace( mesh->FindEdge( 12, 13 )->GetRight( ) );
  if( !joinVertex->Evaluate( mesh->FindEdge( 12, 13 ) ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  mesh->DeletePoint( joinVertex->GetOldPointID( ) );
  if( ! AssertTopologicalInvariants< MeshType >
          ( mesh, 24, 53, 30, 1, 0 ) )
    {
    std::cout << "FAILED (for [ 12, 13 ] )." << std::endl;
    return EXIT_FAILURE;
    }
  if ( mesh->GetPoint( 13 ).GetValence( ) != 8 )
    {
    std::cout << "FAILED (for [ 12, 13 ], wrong valence of "
              <<  mesh->GetPoint( 13 ).GetValence( ) 
              << " for vertex 13 )." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "OK" << std::endl;
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
  // Consider a last pathological test of an edge with an isolated
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
  std::cout << "OK" << std::endl;

  std::cout << "Checking JoinVertex." << "OK" << std::endl << std::endl;

  return EXIT_SUCCESS;  
}
