#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <string>

#include "itkQuadEdgeMesh.h"
#include "itkQuadEdgeMeshLineCell.h"
#include "itkQuadEdgeMeshPolygonCell.h"

#include "itkQuadEdgeMeshFunctionBase.h"
#include "itkQuadEdgeMeshEulerOperatorJoinFacetFunction.h"
#include "itkQuadEdgeMeshEulerOperatorsTestHelper.h"

int itkQuadEdgeMeshEulerOperatorJoinFacetTest(int argc, char* argv[] )
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

  typedef itk::QuadEdgeMeshEulerOperatorJoinFacetFunction< MeshType, QEType>
    JoinFacet;
    
  // EULER OPERATOR TESTS
  QEType * dummy;
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
  std::cout << "     " << "Test No Mesh Input";
  if( joinFacet->Evaluate( (QEType*)1 ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "OK" << std::endl;
  
  (void)joinFacet->GetNameOfClass(); 

  joinFacet->SetInput( mesh );
  
  std::cout << "     " << "Test QE Input not internal";
  dummy = new QEType;
  if( joinFacet->Evaluate( dummy ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  delete dummy;
  std::cout << "OK" << std::endl;
  
  std::cout << "     " << "Test No QE Input";
  if( joinFacet->Evaluate( (QEType*)0 ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "OK" << std::endl;

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
  if( !joinFacet->Evaluate( mesh->FindEdge( 12, 7 ) ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
   
  // Since the edge was internal we lost an edge and an face:
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
  std::cout << "OK" << std::endl;
  std::cout << "Checking JointFacet." << "OK" << std::endl << std::endl;

  return EXIT_SUCCESS;

}
