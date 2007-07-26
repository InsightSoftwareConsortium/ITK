#include <string>

#include "itkQuadEdgeMesh.h"
#include "itkQuadEdgeMeshPolygonCell.h"

#include "itkQuadEdgeMeshEulerOperatorJoinFacetFunction.h"
#include "itkQuadEdgeMeshEulerOperatorSplitFacetFunction.h"
#include "itkQuadEdgeMeshEulerOperatorFlipEdgeFunction.h"
#include "itkQuadEdgeMeshEulerOperatorJoinVertexFunction.h"
#include "itkQuadEdgeMeshEulerOperatorSplitVertexFunction.h"
#include "itkQuadEdgeMeshEulerOperatorSplitEdgeFunction.h"
#include "itkQuadEdgeMeshEulerOperatorCreateCenterVertexFunction.h"
#include "itkQuadEdgeMeshEulerOperatorDeleteCenterVertexFunction.h"

#include "itkQuadEdgeMeshTopologyChecker.h"

typedef unsigned long IdentifierType;

template<class TMesh >
bool AssertTopologicalInvariants( TMesh *mesh, 
                                  IdentifierType NumVertices,
                                  IdentifierType NumFaces,
                                  IdentifierType NumEdges,
                                  IdentifierType NumBorders,
                                  IdentifierType Genus)
{
  typedef itk::QuadEdgeMeshTopologyChecker< TMesh > CheckerType;
  CheckerType *check = new CheckerType;
  check->SetMesh( mesh );
  check->SetExpectedNumberOfPoints( NumVertices );
  check->SetExpectedNumberOfEdges( NumFaces );
  check->SetExpectedNumberOfFaces( NumEdges );
  check->SetExpectedNumberOfBoundaries( NumBorders );
  check->SetExpectedGenus( Genus );
  return( check->ValidateEulerCharacteristic( ) );
}

template< class TMesh >
void PopulateMesh( typename TMesh::Pointer mesh )
{
  typedef typename TMesh                MeshType;
  typedef typename MeshType::CellType   CellType;

  typedef itk::QuadEdgeMeshPolygonCell< CellType > QEPolygonCellType;
  
  if( mesh->GetNumberOfPoints( ) )
    {
    mesh->Clear( );
    mesh->ClearFreePointAndCellIndexesLists();
    }

  /////////////////////////////////////////////////////////////
  int expectedNumPts = 25;
  int expectedNumCells = 32;
  int simpleSquareCells[96] =
  {  0,  1,  6,
     0,  6,  5,
     1,  2,  7,
     1,  7,  6,
     2,  3,  8,
     2,  8,  7,
     3,  4,  9,
     3,  9,  8,
     5,  6, 11,
     5, 11, 10,
     6,  7, 12,
     6, 12, 11,
     7,  8, 13,
     7, 13, 12,
     8,  9, 14,
     8, 14, 13,
    10, 11, 16,
    10, 16, 15,
    11, 12, 17,
    11, 17, 16,
    12, 13, 18,
    12, 18, 17,
    13, 14, 19,
    13, 19, 18,
    15, 16, 21,
    15, 21, 20,
    16, 17, 22,
    16, 22, 21,
    17, 18, 23,
    17, 23, 22,
    18, 19, 24,
    18, 24, 23 };

  MeshType::PointType pts[25];

  pts[ 0][0] = 0.0;  pts[ 0][1] = 0.0;  pts[ 0][2] = 0.0;
  pts[ 1][0] = 1.0;  pts[ 1][1] = 0.0;  pts[ 1][2] = 0.0;
  pts[ 2][0] = 2.0;  pts[ 2][1] = 0.0;  pts[ 2][2] = 0.0;
  pts[ 3][0] = 3.0;  pts[ 3][1] = 0.0;  pts[ 3][2] = 0.0;
  pts[ 4][0] = 4.0;  pts[ 4][1] = 0.0;  pts[ 4][2] = 0.0;
  pts[ 5][0] = 0.0;  pts[ 5][1] = 1.0;  pts[ 5][2] = 0.0;
  pts[ 6][0] = 1.0;  pts[ 6][1] = 1.0;  pts[ 6][2] = 0.0;
  pts[ 7][0] = 2.0;  pts[ 7][1] = 1.0;  pts[ 7][2] = 0.0;
  pts[ 8][0] = 3.0;  pts[ 8][1] = 1.0;  pts[ 8][2] = 0.0;
  pts[ 9][0] = 4.0;  pts[ 9][1] = 1.0;  pts[ 9][2] = 0.0;
  pts[10][0] = 0.0;  pts[10][1] = 2.0;  pts[10][2] = 0.0;
  pts[11][0] = 1.0;  pts[11][1] = 2.0;  pts[11][2] = 0.0;
  pts[12][0] = 2.0;  pts[12][1] = 2.0;  pts[12][2] = 0.0;
  pts[13][0] = 3.0;  pts[13][1] = 2.0;  pts[13][2] = 0.0;
  pts[14][0] = 4.0;  pts[14][1] = 2.0;  pts[14][2] = 0.0;
  pts[15][0] = 0.0;  pts[15][1] = 3.0;  pts[15][2] = 0.0;
  pts[16][0] = 1.0;  pts[16][1] = 3.0;  pts[16][2] = 0.0;
  pts[17][0] = 2.0;  pts[17][1] = 3.0;  pts[17][2] = 0.0;
  pts[18][0] = 3.0;  pts[18][1] = 3.0;  pts[18][2] = 0.0;
  pts[19][0] = 4.0;  pts[19][1] = 3.0;  pts[19][2] = 0.0;
  pts[20][0] = 0.0;  pts[20][1] = 4.0;  pts[20][2] = 0.0;
  pts[21][0] = 1.0;  pts[21][1] = 4.0;  pts[21][2] = 0.0;
  pts[22][0] = 2.0;  pts[22][1] = 4.0;  pts[22][2] = 0.0;
  pts[23][0] = 3.0;  pts[23][1] = 4.0;  pts[23][2] = 0.0;
  pts[24][0] = 4.0;  pts[24][1] = 4.0;  pts[24][2] = 0.0;

  for(int i=0; i<expectedNumPts; i++)
    {
    mesh->SetPoint( i, pts[i] );
    }
 
  CellType::CellAutoPointer cellpointer;
  QEPolygonCellType *poly;

  for(int i=0; i<expectedNumCells; i++)
    {
    poly = new QEPolygonCellType( 3 );
    cellpointer.TakeOwnership( poly );
    cellpointer->SetPointId( 0, simpleSquareCells[3*i] );
    cellpointer->SetPointId( 1, simpleSquareCells[3*i+1] );
    cellpointer->SetPointId( 2, simpleSquareCells[3*i+2] );
    mesh->SetCell( i, cellpointer );
    }
}


int itkQuadEdgeMeshEulerOperatorsTest(int argc, char * argv[])
{
  typedef itk::QuadEdgeMesh< double, 3 >                      MeshType;
  typedef MeshType::Pointer                                   MeshPointer;
  typedef MeshType::QEType                                    QEType;
  typedef MeshType::PointIdentifier                           PointIdentifier;
  typedef MeshType::PointType                                 PointType;
  
  typedef itk::QuadEdgeMeshEulerOperatorJoinFacetFunction< MeshType, QEType>
    JoinFacet;
  typedef itk::QuadEdgeMeshEulerOperatorSplitFacetFunction< MeshType, QEType>
    SplitFacet;
  typedef itk::QuadEdgeMeshEulerOperatorFlipEdgeFunction< MeshType, QEType>
    FlipEdge;
  typedef itk::QuadEdgeMeshEulerOperatorJoinVertexFunction< MeshType, QEType>
    JoinVertex;
  typedef itk::QuadEdgeMeshEulerOperatorSplitEdgeFunction< MeshType, QEType>
    SplitEdge;
  typedef itk::QuadEdgeMeshEulerOperatorSplitVertexFunction< MeshType, QEType>
    SplitVertex;
  typedef itk::QuadEdgeMeshEulerOperatorCreateCenterVertexFunction< MeshType, QEType>
    CreateCenterVertex;
  typedef itk::QuadEdgeMeshEulerOperatorDeleteCenterVertexFunction< MeshType, QEType>
    DeleteCenterVertex;

  MeshPointer  mesh = MeshType::New();
  PopulateMesh<MeshType>( mesh );

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
   //

   /////////////////////////////////////////
   //
   //          Join Facet
   //
   /////////////////////////////////////////

   std::cout << "Checking JointFacet." << std::endl;
   JoinFacet::Pointer joinFacet = JoinFacet::New( );
   joinFacet->SetInput( mesh );

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
   //   10 --------- 11 --------- 12 ---<-H--- 13 --------- 14
   //    |        __/ |        __/          __/ |        __/ |
   //    |     __/    |     __/ new      __/    |     __/    |
   //    |  __/       |  __/     face __/       |  __/       |
   //    | /          | /            /          | /          |
   //    5 ---------- 6 ----G->--- 7 ---------- 8 ---------  9
   //    |        __/ |        __/ |        __/ |        __/ |
   //    |     __/    |     __/    |     __/    |     __/    |
   //    |  __/       |  __/       |  __/       |  __/       |
   //    | /          | /          | /          | /          |
   //    0 ---------- 1 ---------- 2  --------- 3 ---------  4
   //
   
   std::cout << "     " << "Test Edge deletion (possible)";
   // Find an internal edge and collapse it
   QEType* DeletedEdge = mesh->FindEdge( 12, 7 );
   // Store G and H for testing the inverse with SplitFace:
   QEType* G = DeletedEdge->GetSym( )->GetLprev( );
   QEType* H = joinFacet->Evaluate( DeletedEdge );
   if( !H )
     {
     std::cout << "FAILED." << std::endl;
     return 1;
     }
   
   // Since the edge was internal we lost an edge and an face:
   if( ! AssertTopologicalInvariants< MeshType >
           ( mesh, 25, 55, 31, 1, 0 ) )
     {
     std::cout << "FAILED." << std::endl;
     return 1;
     }
   if ( mesh->GetPoint( 12 ).GetValence( ) != 5 )
     {
     std::cout << "FAILED [wrong valence of "
               << mesh->GetPoint( 12 ).GetValence( )
               << " ]." << std::endl;
     return 1;
     }
   std::cout << "OK" << std::endl;
   std::cout << "Checking JointFacet." << "OK" << std::endl << std::endl;

   /////////////////////////////////////////
   //
   //         Split Facet
   //
   /////////////////////////////////////////

   // Split the facet again in order to restore the original situation:
   std::cout << "Checking SplitFacet." << std::endl;
   SplitFacet::Pointer splitFacet = SplitFacet::New( );
   splitFacet->SetInput( mesh );
   if( !splitFacet->Evaluate( H, G ) )
     {
     std::cout << "FAILED." << std::endl;
     return 1;
     }
 
   std::cout << "     " << "Split a face (possible)";
   // The number of edges and faces must be respectively identical to
   // the original number edges and faces:
   if( ! AssertTopologicalInvariants< MeshType >
           ( mesh, 25, 56, 32, 1, 0 ) )
     {
     std::cout << "FAILED." << std::endl;
     return 1;
     }
   if ( mesh->GetPoint( 12 ).GetValence( ) != 6 )
     {
     std::cout << "FAILED [wrong valence of "
               << mesh->GetPoint( 12 ).GetValence( )
               << " ]." << std::endl;
     return 1;
     }
   std::cout << "OK" << std::endl;
   std::cout << "Checking SplitFacet. OK" << std::endl << std::endl;
   
   std::cout << "Checking JoinFacet( SplitFacet( edge ) ) invariance.";
   G = mesh->FindEdge( 12, 7 )->GetSym( )->GetLprev( );
   H = joinFacet->Evaluate( mesh->FindEdge( 12, 7 ) );
   if( !H )
     {
     std::cout << "FAILED." << std::endl;
     return 1;
     }
   if( !joinFacet->Evaluate( splitFacet->Evaluate( H, G ) ) )
     {
     std::cout << "FAILED." << std::endl;
     return 1;
     }
   if( ! AssertTopologicalInvariants< MeshType >
           ( mesh, 25, 55, 31, 1, 0 ) )
     {
     std::cout << "FAILED." << std::endl;
     return 1;
     }
   if ( mesh->GetPoint( 12 ).GetValence( ) != 5 )
     {
     std::cout << "FAILED [wrong valence of "
               << mesh->GetPoint( 12 ).GetValence( )
               << " ]." << std::endl;
     return 1;
     }
   std::cout << ".OK" << std::endl << std::endl;
   
   ///////////////////////////////////////// 
   //
   //              Flip Edge
   //
   /////////////////////////////////////////
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
   //    |        __/ | \__        |        __/ |        __/ |
   //    |     __/    |    \__     |     __/    |     __/    |
   //    |  __/       |       \__  |  __/       |  __/       |
   //    | /          |          \ | /          | /          |
   //    5 ---------- 6 ---------- 7 ---------- 8 ---------  9
   //    |        __/ |        __/ |        __/ |        __/ |
   //    |     __/    |     __/    |     __/    |     __/    |
   //    |  __/       |  __/       |  __/       |  __/       |
   //    | /          | /          | /          | /          |
   //    0 ---------- 1 ---------- 2  --------- 3 ---------  4
   //

   PopulateMesh<MeshType>( mesh );

   std::cout << "Checking FlipEdge." << std::endl;
   std::cout << "     " << "Flip an edge (possible)";
   
   FlipEdge::Pointer flipEdge = FlipEdge::New( );
   flipEdge->SetInput( mesh );
   QEType* tempFlippedEdge = flipEdge->Evaluate( mesh->FindEdge( 12 , 6 ) ); 
   if( !tempFlippedEdge )
     {
     std::cout << "FAILED." << std::endl;
     return 1;
     }
   // The number of edges and faces must be unchanged:
   if( ! AssertTopologicalInvariants< MeshType >
           ( mesh, 25, 56, 32, 1, 0 ) )
     {
     std::cout << "FAILED." << std::endl;
     return 1;
     }
   if ( mesh->GetPoint( 12 ).GetValence( ) != 5 )
     {
     std::cout << "FAILED [wrong valence of "
                << mesh->GetPoint( 12 ).GetValence( )
                << " for vertex 12 ]." << std::endl;
     return 1;
     }
   if ( mesh->GetPoint( 6 ).GetValence( ) != 5 )
     {
     std::cout << "FAILED [wrong valence of "
               << mesh->GetPoint( 6 ).GetValence( )
               << " for vertex 6 ]." << std::endl;
     return 1;
     }
   if ( mesh->GetPoint( 11 ).GetValence( ) != 7 )
     {
     std::cout << "FAILED [wrong valence of "
               << mesh->GetPoint( 11 ).GetValence( )
               << " for vertex 11 ]." << std::endl;
     return 1;
     }
   if ( mesh->GetPoint( 7 ).GetValence( ) != 7 )
     {
     std::cout << "FAILED [wrong valence of "
               << mesh->GetPoint( 7 ).GetValence( )
               << " for vertex 7 ]." << std::endl;
     return 1;
     }
   std::cout << ".OK" << std::endl;

   // Checking invariance (i.e. FlipEdge is it's own inverse):
   std::cout << "     " << "Check FlipEdge(FlipEdge()) invariance (possible for triangles).";
   if( !flipEdge->Evaluate( tempFlippedEdge ) )
     {
     std::cout << "FAILED." << std::endl;
     return 1;
     }
   // The number of edges and faces must be unchanged:
   if( ! AssertTopologicalInvariants< MeshType >
           ( mesh, 25, 56, 32, 1, 0 ) )
     {
     std::cout << "FAILED." << std::endl;
     return 1;
     }
   if ( mesh->GetPoint( 12 ).GetValence( ) != 6 )
     {
     std::cout << "FAILED [wrong valence of "
               << mesh->GetPoint( 12 ).GetValence( )
               << " for vertex 12 ]." << std::endl;
     return 1;
     }
   if ( mesh->GetPoint( 6 ).GetValence( ) != 6 )
     {
     std::cout << "FAILED [wrong valence of "
               << mesh->GetPoint( 6 ).GetValence( )
               << " for vertex 6 ]." << std::endl;
     return 1;
     }
   if ( mesh->GetPoint( 11 ).GetValence( ) != 6 )
     {
     std::cout << "FAILED [wrong valence of "
               << mesh->GetPoint( 11 ).GetValence( )
               << " for vertex 11 ]." << std::endl;
     return 1;
     }
   if ( mesh->GetPoint( 7 ).GetValence( ) != 6 )
     {
     std::cout << "FAILED [wrong valence of "
               << mesh->GetPoint( 7 ).GetValence( )
               << " for vertex 7 ]." << std::endl;
     return 1;
     }

   std::cout << "OK" << std::endl;
   std::cout << "Checking FlipEdge." << "OK" << std::endl << std::endl;
   

   /////////////////////////////////////////
   //
   //          Join Vertex
   //
   /////////////////////////////////////////
   std::cout << "Checking JoinVertex." << std::endl;

   PopulateMesh<MeshType>( mesh );
   
   JoinVertex::Pointer joinVertex = JoinVertex::New( );
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
     return 1;
     }
   mesh->DeletePoint( joinVertex->GetOldPointID( ) );
   if( ! AssertTopologicalInvariants< MeshType >
           ( mesh, 24, 53, 30, 1, 0 ) )
     {
     std::cout << "FAILED (for [ 12, 11 ] )." << std::endl;
     return 1;
     }
   if ( mesh->GetPoint( 11 ).GetValence( ) != 8 )
     {
     std::cout << "FAILED (for [ 12, 11 ], wrong valence of "
               << mesh->GetPoint( 11 ).GetValence( )
               << " for vertex 11 )." << std::endl;
     return 1;
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
     return 1;
     }
   mesh->DeletePoint( joinVertex->GetOldPointID( ) ); 
   if( ! AssertTopologicalInvariants< MeshType >
           ( mesh, 24, 53, 30, 1, 0 ) )
     {
     std::cout << "FAILED (for [ 0, 6 ] )." << std::endl;
     return 1;
     }
   QEType* sixEdge = mesh->FindEdge( 6, 5 );
   if( ! sixEdge || ! sixEdge->IsAtBorder( ) )
     {
     std::cout << "FAILED (for [ 0, 6 ], vertex 6 not on boundary )."
               << std::endl;
     return 1;
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
     return 1;
     }
   mesh->DeletePoint( joinVertex->GetOldPointID( ) );
      
   if( ! AssertTopologicalInvariants< MeshType >
           ( mesh, 24, 54, 31, 1, 0 ) )
     {
     std::cout << "FAILED (for [ 0, 5 ] )." << std::endl;
     return 1;
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
     return 1;
     }
   mesh->DeletePoint( joinVertex->GetOldPointID( ) );
   
   if( ! AssertTopologicalInvariants< MeshType >
           ( mesh, 24, 54, 31, 1, 0 ) )
     {
     std::cout << "FAILED (for [ 2, 3 ] )." << std::endl;
     return 1;
     }
   if ( mesh->GetPoint( 3 ).GetValence( ) != 5 )
     {
     std::cout << "FAILED (for [ 2, 3 ], wrong valence of "
                <<  mesh->GetPoint( 3 ).GetValence( ) 
                << " for vertex 3 )." << std::endl;
     return 1;
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
     return 1;
     }
   mesh->DeletePoint( joinVertex->GetOldPointID( ) );
   if( ! AssertTopologicalInvariants< MeshType >
           ( mesh, 24, 53, 30, 1, 0 ) )
     {
     std::cout << "FAILED (for [ 12, 13 ] )." << std::endl;
     return 1;
     }
   if ( mesh->GetPoint( 13 ).GetValence( ) != 8 )
     {
     std::cout << "FAILED (for [ 12, 13 ], wrong valence of "
               <<  mesh->GetPoint( 13 ).GetValence( ) 
               << " for vertex 13 )." << std::endl;
     return 1;
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
     return 1;
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
     return 1;
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
     return 1;
     }
   mesh->DeletePoint( joinVertex->GetOldPointID( ) );
   
   if( ! AssertTopologicalInvariants< MeshType >
           ( mesh, 23, 48, 26, 1, 0 ) )
     {
     std::cout << "FAILED (for [ 12, 13 ] )." << std::endl;
     return 1;
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
   std::cout << "Join v of antenna (possible).";

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
     return 1;
     }
   mesh->DeletePoint( joinVertex->GetOldPointID( ) );
   if( ! AssertTopologicalInvariants< MeshType >
           ( mesh, 24, 49, 26, 1, 0 ) )
     {
     std::cout << "FAILED (for antenna )." << std::endl;
     return 1;
     }
   std::cout << "OK" << std::endl;

   std::cout << "Checking JoinVertex." << "OK" << std::endl << std::endl;

   /////////////////////////////////////////
   //
   //          Split Vertex
   //
   /////////////////////////////////////////
   std::cout << "Checking SplitVertex." << std::endl;

   PopulateMesh<MeshType>( mesh );
  
   std::cout << "     ";
   std::cout << "Split Vertex (Possible).";

   SplitVertex::Pointer splitVertex = SplitVertex::New( );
   splitVertex->SetInput( mesh );
   if( !splitVertex->Evaluate( mesh->FindEdge(  5, 11 ), 
                               mesh->FindEdge( 17, 11 ) ) )
     {
     std::cout << "FAILED." << std::endl;
     return 1;
     }
   // move the new point, for example along z axis for special effect :-D
   PointType PtTmp = mesh->GetPoint( splitVertex->GetNewPointID( ) );
   PtTmp[2] = 1;
   mesh->SetPoint( splitVertex->GetNewPointID( ), PtTmp );

   // Test
   if( ! AssertTopologicalInvariants< MeshType >
           ( mesh, 26, 57, 32, 1, 0 ) )
     {
     std::cout << "FAILED." << std::endl;
     return 1;
     }
   if ( mesh->GetPoint( splitVertex->GetNewPointID( ) ).GetValence( ) != 4 )
     {
     std::cout << "FAILED, wrong valence of "
     << mesh->GetPoint( splitVertex->GetNewPointID( ) ).GetValence( )
     << " for vertex " << splitVertex->GetNewPointID( ) << "." << std::endl;
     return 1;
     }
   if ( mesh->GetPoint( 11 ).GetValence( ) != 4 )
     {
     std::cout << "FAILED (for, wrong valence of "
               << mesh->GetPoint( 11 ).GetValence( )
               << " for vertex 11 )." << std::endl;
     return 1;
     }
   std::cout << ".OK" << std::endl;

   //test antenna
   PopulateMesh<MeshType>( mesh );

   std::cout << "     ";
   std::cout << "Try to split antenna (impossible).";
   
   splitVertex->SetInput( mesh );
   if( splitVertex->Evaluate( mesh->FindEdge( 12, 17 ),
                              mesh->FindEdge( 12, 17 ) ) )
     {
     std::cout << "FAILED." << std::endl;
     return 1;
     }
   std::cout << ".OK" << std::endl;

   //test different dest( )
   PopulateMesh<MeshType>( mesh );

   std::cout << "     ";
   std::cout << "Test with args with different Dest( ) (Impossible).";
      
   splitVertex->SetInput( mesh );
   if( splitVertex->Evaluate( mesh->FindEdge(  5, 11 ), 
                               mesh->FindEdge(  5,  6 ) ) )
     {
     std::cout << "FAILED." << std::endl;
     return 1;
     }
   if( ! AssertTopologicalInvariants< MeshType >
           ( mesh, 25, 56, 32, 1, 0 ) )
     {
     std::cout << "FAILED." << std::endl;
     return 1;
     }
   if ( mesh->GetPoint( 5 ).GetValence( ) != 4 )
     {
     std::cout << "FAILED, wrong valence of "
     << mesh->GetPoint( 5 ).GetValence( )
     << " for vertex 5." << std::endl;
     return 1;
     }
   std::cout << ".OK" << std::endl;

   std::cout << "Checking SplitVertex." << std::endl << std::endl;
   
   std::cout << "Checking JoinVertex( SplitVertex()) Invariance.";
   
   PopulateMesh<MeshType>( mesh );

   splitVertex->SetInput( mesh );
   joinVertex->SetInput( mesh );
   if( !joinVertex->Evaluate( splitVertex->Evaluate( mesh->FindEdge(  5, 11 ),
                                                     mesh->FindEdge( 17, 11 ) )) )
     {
     std::cout << "FAILED." << std::endl;
     return 1;
     }
   mesh->DeletePoint( joinVertex->GetOldPointID( ) );
   if( ! AssertTopologicalInvariants< MeshType >
           ( mesh, 25, 56, 32, 1, 0 ) )
     {
     std::cout << "FAILED." << std::endl;
     return 1;
     }
   std::cout << ".OK" << std::endl << std::endl;

   /////////////////////////////////////////
   //
   //          Split Edge
   //
   /////////////////////////////////////////
   std::cout << "Checking SplitEdge." << std::endl;
   PopulateMesh<MeshType>( mesh );
   
   std::cout << "     ";
   std::cout << "Split an internal edge (possible).";

   SplitEdge::Pointer splitEdge = SplitEdge::New( );
   splitEdge->SetInput( mesh );
   if( !splitEdge->Evaluate( mesh->FindEdge(  6, 12 ) ) )
     {
     std::cout << "FAILED." << std::endl;
     return 1;
     }
   if( ! AssertTopologicalInvariants< MeshType >
           ( mesh, 26, 57, 32, 1, 0 ) )
     {
     std::cout << "FAILED." << std::endl;
     return 1;
     }
   std::cout << ".OK" << std::endl;

   std::cout << "Checking SplitEdge." << "OK" << std::endl << std::endl;

   /////////////////////////////////////////
   //
   //          Create Center Vertex
   //
   /////////////////////////////////////////
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
   //    |        __/ |        __/ |        __//|        __/ |
   //    |     __/    |     __/    |     __/  / |     __/    |
   //    |  __/       |  __/       |  __/ __x/  |  __/       |
   //    | /          | /          | /___/   \_ | /          |
   //    0 ---------- 1 ---------- 2  --------- 3 ---------  4
   
   std::cout << "Checking CreateCenterVertex." << std::endl;
   
   PopulateMesh<MeshType>( mesh );
   
   std::cout << "     ";
   std::cout << "Create a center vertex of a triangle (possible).";
      
   CreateCenterVertex::Pointer createCenterVertex = CreateCenterVertex::New( );
   createCenterVertex->SetInput( mesh );
   if( !createCenterVertex->Evaluate( mesh->FindEdge( 8, 2 ) ) )
     {
     std::cout << "FAILED." << std::endl;
     return 1;
     }
   if( ! AssertTopologicalInvariants< MeshType >
           ( mesh, 26, 59, 34, 1, 0 ) )
     {
     std::cout << "FAILED." << std::endl;
     return 1;
     }
   if ( mesh->GetPoint( createCenterVertex->GetNewPointID( ) ).GetValence( ) != 3 )
     {
     std::cout << "FAILED, wrong valence of "
        << mesh->GetPoint( createCenterVertex->GetNewPointID( ) ).GetValence( )
        << " for vertex "
        << createCenterVertex->GetNewPointID( )
        << "." << std::endl;
     return 1;
     }
   std::cout << ".OK" << std::endl;

   // test with a bigger polygon
   //
   //   20 --------- 21 --------- 22 --------- 23 --------- 24
   //    |        __/ |        __/ |        __/ |        __/ |
   //    |     __/    |     __/    |     __/    |     __/    |
   //    |  __/       |  __/       |  __/       |  __/       |
   //    | /          | /          | /          | /          |
   //   15 --------- 16 --------- 17 --------- 18 --------- 19
   //    |        __/ | \__        |        __/ |        __/ |
   //    |     __/    |    \__     |     __/    |     __/    |
   //    |  __/       |       \__  |  __/       |  __/       |
   //    | /          |          \ | /          | /          |
   //   10 --------- 11---------- xx --------- 13 --------- 14
   //    |        __/ |        __/ | \__        |        __/ |
   //    |     __/    |     __/    |    \__     |     __/    |
   //    |  __/       |  __/       |       \__  |  __/       |
   //    | /          | /          |          \ | /          |
   //    5 ---------- 6 ---------- 7 ---------- 8 ---------  9
   //    |        __/ |        __/ |        __/ |        __/ |
   //    |     __/    |     __/    |     __/    |     __/    |
   //    |  __/       |  __/       |  __/       |  __/       |
   //    | /          | /          | /          | /          |
   //    0 ---------- 1 ---------- 2  --------- 3 ---------  4
   
   PopulateMesh<MeshType>( mesh );
   
   std::cout << "     ";
   std::cout << "Create a center vertex of a Polygon (possible).";
   
   // this create the 16-17-18-13-8-7-6-11 polygon.
   mesh->LightWeightDeleteEdge( mesh->FindEdge( 11, 12 ) );
   mesh->LightWeightDeleteEdge( mesh->FindEdge(  6, 12 ) );
   mesh->LightWeightDeleteEdge( mesh->FindEdge(  7, 12 ) );
   mesh->LightWeightDeleteEdge( mesh->FindEdge(  7, 13 ) );
   mesh->LightWeightDeleteEdge( mesh->FindEdge( 12, 13 ) );
   mesh->LightWeightDeleteEdge( mesh->FindEdge( 12, 18 ) );
   mesh->LightWeightDeleteEdge( mesh->FindEdge( 12, 17 ) );
   mesh->LightWeightDeleteEdge( mesh->FindEdge( 11, 17 ) );
   mesh->DeletePoint( 12 );
   mesh->AddFace( mesh->FindEdge( 6, 7 ) );
   if( !createCenterVertex->Evaluate( mesh->FindEdge( 6,  7 ) ) )
     {
     std::cout << "FAILED." << std::endl;
     return 1;
     }   
   if( ! AssertTopologicalInvariants< MeshType >
           ( mesh, 25, 56, 32, 1, 0 ) )
     {
     std::cout << "FAILED." << std::endl;
     return 1;
     }
   if ( mesh->GetPoint( createCenterVertex->GetNewPointID( ) ).GetValence( ) != 8 )
     {
     std::cout << "FAILED, wrong valence of "
        << mesh->GetPoint( createCenterVertex->GetNewPointID( ) ).GetValence( )
        << " for vertex "
        << createCenterVertex->GetNewPointID( )
        << "." << std::endl;
     return 1;
     }
   std::cout << ".OK" << std::endl;

   std::cout << "Checking CreateCenterVertex." << "OK" << std::endl << std::endl;

   /////////////////////////////////////////
   //
   //          Delete Center Vertex
   //
   /////////////////////////////////////////

   // first test with the center vertex.
   // we take 6-->12 as incoming parameter.
   //
   //    Vertices: 24 , Edges: 50, Faces: 27, Boundary = 1, Chi = 1
   //
   //   20 --------- 21 --------- 22 --------- 23 --------- 24
   //    |        __/ |        __/ |        __/ |        __/ |
   //    |     __/    |     __/    |     __/    |     __/    |
   //    |  __/       |  __/       |  __/       |  __/       |
   //    | /          | /          | /          | /          |
   //   15 --------- 16 --------- 17 --------- 18 --------- 19
   //    |        __/ |        __/              |        __/ |
   //    |     __/    |     __/                 |     __/    |
   //    |  __/       |  __/                    |  __/       |
   //    | /          | /                       | /          |
   //   10 --------- 11                        13 --------- 14
   //    |        __/ |                     __/ |        __/ |
   //    |     __/    |                  __/    |     __/    |
   //    |  __/       |               __/       |  __/       |
   //    | /          |              /          | /          |
   //    5 ---------- 6 ---------- 7 ---------- 8 ---------  9
   //    |        __/ |        __/ |        __/ |        __/ |
   //    |     __/    |     __/    |     __/    |     __/    |
   //    |  __/       |  __/       |  __/       |  __/       |
   //    | /          | /          | /          | /          |
   //    0 ---------- 1 ---------- 2  --------- 3 ---------  4
   std::cout << "Checking DeleteCenterVertex." << std::endl;

   PopulateMesh<MeshType>( mesh );

   std::cout << "     ";
   std::cout << "Delete center vertex with internal 1-ring (possible).";

   DeleteCenterVertex::Pointer deleteCenterVertex = DeleteCenterVertex::New( );
   deleteCenterVertex->SetInput( mesh );
   if( !deleteCenterVertex->Evaluate( mesh->FindEdge( 6, 12 ) ) )
     {
     std::cout << "FAILED." << std::endl;
     return 1;
     }
   mesh->DeletePoint( deleteCenterVertex->GetOldPointID( ) );
   if( ! AssertTopologicalInvariants< MeshType >
           ( mesh, 24, 50, 27, 1, 0 ) )
     {
     std::cout << "FAILED." << std::endl;
     return 1;
     }
   std::cout << ".OK" << std::endl;

   // The initial configuration and numbering of simpleSquare.vtk:
   //    Vertices: 25 , Edges: 56, Faces: 32, Boundary = 1, Chi = 1
   //
   //   20 --------- 21 --------- 22 --------- 23 --------- 24
   //    |        __/ |        __/ |        __/              |
   //    |     __/    |     __/    |     __/                 |
   //    |  __/       |  __/       |  __/                    |
   //    | /          | /          | /                       |
   //   15 --------- 16 --------- 17                        19
   //    |        __/ |        __/ |                     __/ |
   //    |     __/    |     __/    |                  __/    |
   //    |  __/       |  __/       |               __/       |
   //    | /          | /          |              /          |
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
   //

   PopulateMesh<MeshType>( mesh );

   std::cout << "     ";
   std::cout << "Delete center vertex with border 1-ring (possible).";

   deleteCenterVertex->SetInput( mesh );
   if( !deleteCenterVertex->Evaluate( mesh->FindEdge( 17, 18 ) ) )
     {
     std::cout << "FAILED." << std::endl;
     return 1;
     }
   mesh->DeletePoint( deleteCenterVertex->GetOldPointID( ) );
   if( ! AssertTopologicalInvariants< MeshType >
           ( mesh, 24, 50, 27, 1, 0 ) )
     {
     std::cout << "FAILED." << std::endl;
     return 1;
     }
   std::cout << ".OK" << std::endl;

   // test that border points can not be deleted.
   PopulateMesh<MeshType>( mesh );

   std::cout << "     ";
   std::cout << "Check deleting a border vertex (impossible).";

   deleteCenterVertex->SetInput( mesh );
   if( deleteCenterVertex->Evaluate( mesh->FindEdge( 23, 24 ) ) )
     {
     std::cout << "FAILED." << std::endl;
     return 1;
     }
   if( ! AssertTopologicalInvariants< MeshType >
           ( mesh, 25, 56, 32, 1, 0 ) )
     {
     std::cout << "FAILED." << std::endl;
     return 1;
     }
   std::cout << ".OK" << std::endl;
   
   // test that points including an hole in the 1-ring
   // can not be deleted.
   PopulateMesh<MeshType>( mesh );

   std::cout << "     ";
   std::cout << "Check deleting a vertex around an hole (impossible).";

   mesh->LightWeightDeleteEdge( mesh->FindEdge(  6, 12 ) );
   deleteCenterVertex->SetInput( mesh );
   if( deleteCenterVertex->Evaluate( mesh->FindEdge(  6,  7 ) ) )
     {
     std::cout << "FAILED." << std::endl;
     return 1;
     }
   if( !AssertTopologicalInvariants< MeshType >
           ( mesh, 25, 55, 30, 2, 0 ) )
     {
     std::cout << "FAILED." << std::endl;
     return 1;
     }
   std::cout << ".OK" << std::endl;

   std::cout << "     ";
   std::cout << "Check deleting a vertex in a tetrahedron (impossible).";
   //do something
   std::cout << "TODO.OK" << std::endl;

   std::cout << "Checking DeleteCenterVertex." << "OK" << std::endl << std::endl;

   std::cout << "Checking DeleteCenterVertex( CreateCenterVertex()) Invariance.";
   
   PopulateMesh<MeshType>( mesh );
   
   if( !deleteCenterVertex->Evaluate( createCenterVertex->Evaluate( mesh->FindEdge( 0, 1 ) ) ) )
     {
     std::cout << "FAILED." << std::endl;
     return 1;
     }
   mesh->DeletePoint( deleteCenterVertex->GetOldPointID( ) );
   if( ! AssertTopologicalInvariants< MeshType >
           ( mesh, 25, 56, 32, 1, 0 ) )
     {
     std::cout << "FAILED." << std::endl;
     return 1;
     }
   std::cout << ".OK" << std::endl;

   return 0;

}
