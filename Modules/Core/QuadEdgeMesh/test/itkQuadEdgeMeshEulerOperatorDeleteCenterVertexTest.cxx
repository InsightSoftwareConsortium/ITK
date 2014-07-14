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

#include "itkQuadEdgeMeshEulerOperatorDeleteCenterVertexFunction.h"
#include "itkQuadEdgeMeshEulerOperatorCreateCenterVertexFunction.h"
#include "itkQuadEdgeMeshEulerOperatorsTestHelper.h"

int itkQuadEdgeMeshEulerOperatorDeleteCenterVertexTest(int argc, char* argv[] )
{
  (void)argc;
  (void)argv;

  typedef itk::QuadEdgeMesh< double, 3 >                      MeshType;
  typedef MeshType::Pointer                                   MeshPointer;
  typedef MeshType::QEType                                    QEType;
  typedef MeshType::PointType                                 PointType;
  typedef MeshType::CellType                                  CellType;

  typedef itk::QuadEdgeMeshEulerOperatorDeleteCenterVertexFunction< MeshType,
    QEType> DeleteCenterVertex;

  typedef itk::QuadEdgeMeshEulerOperatorCreateCenterVertexFunction< MeshType,
    QEType> CreateCenterVertex;

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

  MeshPointer mesh = MeshType::New();
  CreateSquareTriangularMesh<MeshType>( mesh );

  DeleteCenterVertex::Pointer deleteCenterVertex = DeleteCenterVertex::New( );
  std::cout << "     " << "Test No Mesh Input";
  if( deleteCenterVertex->Evaluate( (QEType*)1 ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "OK" << std::endl;

  (void)deleteCenterVertex->GetNameOfClass();

  deleteCenterVertex->SetInput( mesh );
  std::cout << "     " << "Test No QE Input";
  if( deleteCenterVertex->Evaluate( (QEType*)ITK_NULLPTR ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "OK" << std::endl;

  deleteCenterVertex->SetInput( mesh );
  std::cout << "     " << "Test one-ring not full (impossible)";
  if( deleteCenterVertex->Evaluate( mesh->FindEdge( 15, 21 ) ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "OK" << std::endl;

    {
    MeshPointer  specialmesh = MeshType::New();
    PointType pts3[4];
    pts3[ 0][0] = 0.0;  pts3[ 0][1] = 0.0;  pts3[ 0][2] = 0.0;
    pts3[ 1][0] = 1.0;  pts3[ 1][1] = 0.0;  pts3[ 1][2] = 0.0;
    pts3[ 2][0] = 0.0;  pts3[ 2][1] = 1.0;  pts3[ 2][2] = 0.0;
    pts3[ 3][0] = 0.0;  pts3[ 3][1] = 0.0;  pts3[ 3][2] = 1.0;
    for(int i=0; i<4; i++)
      {
      specialmesh->SetPoint( i, pts3[i] );
      }
    int specialCells[12] =
    {  0,  1,  2,
       0,  2,  3,
       3,  1,  0,
       1,  3,  2 };

    CellType::CellAutoPointer cellpointer;
    typedef itk::QuadEdgeMeshPolygonCell< CellType > QEPolygonCellType;
    QEPolygonCellType *poly;
    for(int i=0; i<4; i++)
      {
      poly = new QEPolygonCellType( 3 );
      cellpointer.TakeOwnership( poly );
      cellpointer->SetPointId( 0, specialCells[3*i] );
      cellpointer->SetPointId( 1, specialCells[3*i+1] );
      cellpointer->SetPointId( 2, specialCells[3*i+2] );
      specialmesh->SetCell( i, cellpointer );
      }
    deleteCenterVertex->SetInput( specialmesh );
    std::cout << "     ";
    std::cout << "Delete a vertex of a non-collapsable mesh (impossible).";
    if( deleteCenterVertex->Evaluate( specialmesh->FindEdge( 0, 1 ) ) )
      {
      std::cout << "FAILED." << std::endl;
      return EXIT_FAILURE;
      }
    }

  deleteCenterVertex->SetInput( mesh );
  std::cout << "     ";
  std::cout << "Delete center vertex with internal 1-ring (possible).";
  if( !deleteCenterVertex->Evaluate( mesh->FindEdge( 6, 12 ) ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  mesh->DeletePoint( deleteCenterVertex->GetOldPointID( ) );
  if( ! AssertTopologicalInvariants< MeshType >
          ( mesh, 24, 50, 27, 1, 0 ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
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
  CreateSquareTriangularMesh<MeshType>( mesh );
  std::cout << "     ";
  std::cout << "Delete center vertex with border 1-ring (possible).";
  deleteCenterVertex->SetInput( mesh );
  if( !deleteCenterVertex->Evaluate( mesh->FindEdge( 17, 18 ) ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  mesh->DeletePoint( deleteCenterVertex->GetOldPointID( ) );
  if( ! AssertTopologicalInvariants< MeshType >
          ( mesh, 24, 50, 27, 1, 0 ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << ".OK" << std::endl;
  // test that border points can not be deleted.
  CreateSquareTriangularMesh<MeshType>( mesh );
  std::cout << "     ";
  std::cout << "Check deleting a border vertex (impossible).";

  deleteCenterVertex->SetInput( mesh );
  if( deleteCenterVertex->Evaluate( mesh->FindEdge( 23, 24 ) ) )
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
  std::cout << ".OK" << std::endl;

  // test that points including an hole in the 1-ring
  // can not be deleted.
  CreateSquareTriangularMesh<MeshType>( mesh );
  std::cout << "     ";
  std::cout << "Check deleting a vertex around an hole (impossible).";
  mesh->LightWeightDeleteEdge( mesh->FindEdge(  6, 12 ) );
  deleteCenterVertex->SetInput( mesh );
  if( deleteCenterVertex->Evaluate( mesh->FindEdge(  6,  7 ) ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  if( !AssertTopologicalInvariants< MeshType >
          ( mesh, 25, 55, 30, 2, 0 ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << ".OK" << std::endl;

  std::cout << "Checking DeleteCenterVertex." << "OK" << std::endl << std::endl;

  std::cout << "Checking DeleteCenterVertex( CreateCenterVertex()) Invariance.";

  CreateCenterVertex::Pointer createCenterVertex = CreateCenterVertex::New();
  createCenterVertex->SetInput( mesh );

  CreateSquareTriangularMesh<MeshType>( mesh );
  if( !deleteCenterVertex->Evaluate(
    createCenterVertex->Evaluate( mesh->FindEdge( 0, 1 ) ) ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  mesh->DeletePoint( deleteCenterVertex->GetOldPointID( ) );
  if( ! AssertTopologicalInvariants< MeshType >
          ( mesh, 25, 56, 32, 1, 0 ) )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << ".OK" << std::endl;
  return EXIT_SUCCESS;
}
