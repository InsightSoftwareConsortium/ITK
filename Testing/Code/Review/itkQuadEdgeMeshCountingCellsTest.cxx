#include "itkQuadEdgeMesh.h"
#include "itkQuadEdgeMeshLineCell.h"
#include "itkQuadEdgeMeshPolygonCell.h"

template< class TMesh >
void PopulateMesh( typename TMesh::Pointer mesh )
{
  typedef TMesh                         MeshType;
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

  typename MeshType::PointType pts[25];

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
 
  typename CellType::CellAutoPointer cellpointer;
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

int itkQuadEdgeMeshCountingCellsTest(int argc, char * argv[])
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

  std::cout << "ITK API"           << std::endl;
  std::cout << "GetNumberOfCells   - Get Number of cells (all cells, from container)";
  std::cout << std::endl;
  std::cout << "GetNumberOfPoints  - Get Number of points (all points, from container)";
  std::cout << std::endl;
  std::cout << "ITK QE API"        << std::endl;
  std::cout << "ComputeNumberOfEdges - Only edges  - one container traversal"  << std::endl;
  std::cout << "ComputeNumberOfFaces - Only faces  - one container traversal"  << std::endl;
  std::cout << "ComputeNumberOfPoints- Only USED points - one container traversal" << std::endl;
  std::cout << "GetNumberOfFaces     - Only faces  - member variable" << std::endl;
  std::cout << "GetNumberOfEdges     - Only Edges  - member variable" << std::endl; 

  std::cout << std::endl;
  std::cout << "START TEST" << std::endl;
  std::cout << "ITK API"            << std::endl;
  std::cout << "GetNumberOfCells: " << mesh->GetNumberOfCells() << std::endl;
  std::cout << "GetNumberOfPoints: "<< mesh->GetNumberOfPoints()<< std::endl;
  
  std::cout << "ITK QE API"        << std::endl;
  std::cout << "ComputeNumberOfEdges: " << mesh->ComputeNumberOfEdges() << std::endl;
  std::cout << "ComputeNumberOfFaces: " << mesh->ComputeNumberOfFaces() << std::endl;
  std::cout << "ComputeNumberOfPoints: "<< mesh->ComputeNumberOfPoints()<< std::endl;
  std::cout << "GetNumberOfFaces: "     << mesh->GetNumberOfFaces()     << std::endl;
  std::cout << "GetNumberOfEdges: "     << mesh->GetNumberOfEdges()     << std::endl; 

  return EXIT_SUCCESS;
}

