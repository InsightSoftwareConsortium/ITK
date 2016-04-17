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

/**
 * This test verifies that a \ref itk::QuadEdgeMesh::FrontIterator walkthrough on
 * a Mesh correctly visits all the points. Therefore, we count the points
 * and assert that the result is identical to the one obtained with
 * \ref itk::QuadEdgeMesh::ComputeNumberOfPoints .
 * This can only work when the instance of the Mesh we are considering has
 * a single component (possibly multiply connected). In order to verify
 * this assumption we use \ref MeshExtractComponentFilter which is based
 * on a \ref itk::QuadEdgeMesh::FrontDualIterator. Hence we shall have tested
 * both primal and dual version of the front iterator.
 */

#include "itkQuadEdgeMesh.h"

int itkQuadEdgeMeshFrontIteratorTest( int , char *[] )
{
  typedef itk::QuadEdgeMesh< double, 3 >      MeshType;
  typedef MeshType::Pointer                   MeshPointer;

  typedef MeshType::PointIdentifier   PointIdentifier;
  typedef std::set< PointIdentifier > PointIdSet;

  typedef MeshType::CellType                        CellType;
  typedef itk::QuadEdgeMeshPolygonCell< CellType >  QEPolygonCellType;

  typedef MeshType::FrontIterator FrontIterator;
  typedef FrontIterator::QEType   QEType;

  int expectedNumPts = 25;
  int expectedNumCells = 32;

  std::cout << "expectedNumPts= " << expectedNumPts << std::endl;
  std::cout << "expectedNumCells= " << expectedNumCells << std::endl;

  /////////////////////////////////////////////////////////////
  int simpleSquareCells[96] =
  { 0,  1,  6,
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

  // Configuration of simpleSquare mesh:
  // #Vertices= 25 , #Edges= 56, #Faces= 32, #Boundary= 1, Chi= 1
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

  MeshPointer  mesh = MeshType::New();

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

  int numPts = mesh->GetNumberOfPoints();
  std::cout << "numPts= " << numPts << std::endl;

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

  int numCells = mesh->GetNumberOfCells();
  std::cout << "numCells= " << numCells << std::endl;

  // Use a FrontIterator (Primal) to visit the points.
  PointIdSet visitedSet;

  for( FrontIterator it = mesh->BeginFront();
      it != mesh->EndFront(); it++ )
    {
    QEType* edge = it.Value();

    PointIdentifier origin  = edge->GetOrigin();
    PointIdentifier destination = edge->GetDestination();

    if( !visitedSet.count( origin ) )
      {
      visitedSet.insert( origin );
      }
    if( !visitedSet.count( destination ) )
      {
      visitedSet.insert( destination );
      }
    }

  // Compare with Mesh container iteration version
  size_t numberOfPoints = visitedSet.size();
  int computedNumberOfPoints = mesh->ComputeNumberOfPoints();

  std::cout << "numberOfPoints " << numberOfPoints << std::endl;
  std::cout << "computedNumberOfPoints= " << computedNumberOfPoints
    << std::endl;

  if( computedNumberOfPoints == itk::Math::CastWithRangeCheck<int>( numberOfPoints ) )
    {
    std::cout << "Passed" << std::endl;
    }
  else
    {
    std::cout << "Failed" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
