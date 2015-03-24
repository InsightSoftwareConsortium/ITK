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
#ifndef itkQuadEdgeMeshEulerOperatorsTestHelper_h
#define itkQuadEdgeMeshEulerOperatorsTestHelper_h

#include "itkQuadEdgeMeshTopologyChecker.h"
#include "itkQuadEdgeMeshPolygonCell.h"

typedef unsigned long IdentifierType;

template<typename TMesh >
bool AssertTopologicalInvariants( TMesh *mesh,
                                  IdentifierType NumVertices,
                                  IdentifierType NumFaces,
                                  IdentifierType NumEdges,
                                  IdentifierType NumBorders,
                                  IdentifierType Genus)
{
  typedef itk::QuadEdgeMeshTopologyChecker< TMesh > CheckerType;
  typename CheckerType::Pointer check = CheckerType::New();
  check->SetMesh( mesh );
  check->SetExpectedNumberOfPoints( NumVertices );
  check->SetExpectedNumberOfEdges( NumFaces );
  check->SetExpectedNumberOfFaces( NumEdges );
  check->SetExpectedNumberOfBoundaries( NumBorders );
  check->SetExpectedGenus( Genus );
  return( check->ValidateEulerCharacteristic( ) );
}

//----------------------------------------------------------------------------
template< typename TMesh >
std::vector< typename TMesh::PointType >
GeneratePointCoordinates( const unsigned int& iN )
{
  typedef typename TMesh::PointType        PointType;
  typedef typename PointType::CoordRepType CoordRepType;
  std::vector< PointType > oPt( iN * iN );

  for( unsigned int i = 0; i < iN; i++ )
  {
    for( unsigned int j = 0; j < iN; j++ )
    {
      oPt[ i * iN + j ][0] = static_cast< CoordRepType >( j );
      oPt[ i * iN + j ][1] = static_cast< CoordRepType >( i );
      oPt[ i * iN + j ][2] = static_cast< CoordRepType >( 0. );
    }
  }

  return oPt;
}

//----------------------------------------------------------------------------
template< typename TMesh >
void CreateSquareQuadMesh( typename TMesh::Pointer mesh )
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
  int expectedNumCells = 16;
  int simpleSquareCells[64] =
  {  0,  1,  6, 5,
     1,  2,  7, 6,
     2,  3,  8, 7,
     3,  4,  9, 8,
     5,  6, 11, 10,
     6,  7, 12, 11,
     7,  8, 13, 12,
     8,  9, 14, 13,
    10, 11, 16, 15,
    11, 12, 17, 16,
    12, 13, 18, 17,
    13, 14, 19, 18,
    15, 16, 21, 20,
    16, 17, 22, 21,
    17, 18, 23, 22,
    18, 19, 24, 23 };

  typedef typename MeshType::PointType PointType;

  std::vector< PointType > pts = GeneratePointCoordinates< MeshType >( 5 );

  for(int i=0; i<expectedNumPts; i++)
    {
    mesh->SetPoint( i, pts[i] );
    }

  typename CellType::CellAutoPointer cellpointer;
  QEPolygonCellType *poly;

  for(int i=0; i<expectedNumCells; i++)
    {
    poly = new QEPolygonCellType( 4 );
    cellpointer.TakeOwnership( poly );
    cellpointer->SetPointId( 0, simpleSquareCells[4*i] );
    cellpointer->SetPointId( 1, simpleSquareCells[4*i+1] );
    cellpointer->SetPointId( 2, simpleSquareCells[4*i+2] );
    cellpointer->SetPointId( 3, simpleSquareCells[4*i+3] );
    mesh->SetCell( i, cellpointer );
    }
}

//----------------------------------------------------------------------------
template< typename TMesh >
void CreateSquareTriangularMesh( typename TMesh::Pointer mesh )
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

  typedef typename TMesh::PointType PointType;
  std::vector< PointType > pts = GeneratePointCoordinates< TMesh >( 5 );

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

//----------------------------------------------------------------------------
template< typename TMesh >
void CreateTetraedronMesh( typename TMesh::Pointer mesh )
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
  int expectedNumPts = 4;
  int expectedNumCells = 4;
  int simpleSquareCells[12] =
  {  0,  1,  2,
     1,  0,  3,
     1,  3,  2,
     2,  3,  0 };

  typedef typename TMesh::PointType PointType;
  std::vector< PointType > pts( 4 );
  int i(0);
  pts[i][0] = 0.; pts[i][1] = 1.; pts[i++][2] = 0.;
  pts[i][0] = 0.; pts[i][1] = -1.; pts[i++][2] = 0.;
  pts[i][0] = -1.; pts[i][1] = 0.; pts[i++][2] = 0.;
  pts[i][0] = 0.; pts[i][1] = 0.; pts[i++][2] = 1.;

  for( i=0; i<expectedNumPts; i++)
    {
    mesh->SetPoint( i, pts[i] );
    }

  typename CellType::CellAutoPointer cellpointer;
  QEPolygonCellType *poly;

  for( i=0; i<expectedNumCells; i++)
    {
    poly = new QEPolygonCellType( 3 );
    cellpointer.TakeOwnership( poly );
    cellpointer->SetPointId( 0, simpleSquareCells[3*i] );
    cellpointer->SetPointId( 1, simpleSquareCells[3*i+1] );
    cellpointer->SetPointId( 2, simpleSquareCells[3*i+2] );
    mesh->SetCell( i, cellpointer );
    }
}


//----------------------------------------------------------------------------
template< typename TMesh >
void CreateSamosa( typename TMesh::Pointer mesh )
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
  int expectedNumPts = 3;
  int expectedNumCells = 2;
  int simpleSquareCells[6] =
  {  0,  1,  2,
     1,  0,  2 };

  typedef typename TMesh::PointType PointType;
  std::vector< PointType > pts( 3 );
  int i(0);
  pts[i][0] = 0.; pts[i][1] = 1.; pts[i++][2] = 0.;
  pts[i][0] = 0.; pts[i][1] = -1.; pts[i++][2] = 0.;
  pts[i][0] = -1.; pts[i][1] = 0.; pts[i++][2] = 0.;

  for( i=0; i<expectedNumPts; i++)
    {
    mesh->SetPoint( i, pts[i] );
    }

  typename CellType::CellAutoPointer cellpointer;
  QEPolygonCellType *poly;

  for( i=0; i<expectedNumCells; i++)
    {
    poly = new QEPolygonCellType( 3 );
    cellpointer.TakeOwnership( poly );
    cellpointer->SetPointId( 0, simpleSquareCells[3*i] );
    cellpointer->SetPointId( 1, simpleSquareCells[3*i+1] );
    cellpointer->SetPointId( 2, simpleSquareCells[3*i+2] );
    mesh->SetCell( i, cellpointer );
    }
}


#endif
