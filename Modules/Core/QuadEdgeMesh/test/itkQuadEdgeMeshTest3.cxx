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

#include "itkQuadEdgeMesh.h"

int itkQuadEdgeMeshTest3( int , char* [] )
{
  typedef double                                    PixelType;
  typedef itk::QuadEdgeMesh< PixelType, 3 >         MeshType;
  typedef MeshType::CellType                        CellType;
  typedef itk::QuadEdgeMeshLineCell< CellType >     LineType;
  typedef itk::QuadEdgeMeshPolygonCell< CellType >  QEPolygonCellType;

  MeshType::Pointer mesh = MeshType::New();

  MeshType::PointType point0;
  MeshType::PointType point1;
  MeshType::PointType point2;
  MeshType::PointType point3;

  point0[ 0 ] = -1; point0[ 1 ] = -1; point0[ 2 ] = -1;
  point1[ 0 ] =  1; point1[ 1 ] =  1; point1[ 2 ] = -1;
  point2[ 0 ] =  1; point2[ 1 ] = -1; point2[ 2 ] =  1;
  point3[ 0 ] = -1; point3[ 1 ] =  1; point3[ 2 ] =  1;

  mesh->SetPoint( 0, point0 );
  mesh->SetPoint( 1, point1 );
  mesh->SetPoint( 2, point2 );
  mesh->SetPoint( 3, point3 );

  CellType::CellAutoPointer cellpointer;
  QEPolygonCellType* poly;
  LineType* edge;

  poly = new QEPolygonCellType( 3 );
  cellpointer.TakeOwnership( poly );
  cellpointer->SetPointId( 0, 0 );
  cellpointer->SetPointId( 1, 1 );
  cellpointer->SetPointId( 2, 2 );
  mesh->SetCell( 0, cellpointer );

  poly = new QEPolygonCellType( 3 );
  cellpointer.TakeOwnership( poly );
  cellpointer->SetPointId( 0, 0 );
  cellpointer->SetPointId( 1, 2 );
  cellpointer->SetPointId( 2, 3 );
  mesh->SetCell( 1, cellpointer );

  poly = new QEPolygonCellType( 3 );
  cellpointer.TakeOwnership( poly );
  cellpointer->SetPointId( 0, 0 );
  cellpointer->SetPointId( 1, 3 );
  cellpointer->SetPointId( 2, 1 );
  mesh->SetCell( 2, cellpointer );

  poly = new QEPolygonCellType( 3 );
  // FIXME ALEX: write REAL test code here for coverage.
  poly->GetNameOfClass( );
  poly->GetEdgeRingEntry( )->GetRight( );
  poly->GetEdgeRingEntry( )->GetLeft( );
  poly->GetEdgeRingEntry( )->GetIdent( );
  poly->GetType( );
  poly->GetDimension( );
  // up to here.
  cellpointer.TakeOwnership( poly );
  cellpointer->SetPointId( 0, 3 );
  cellpointer->SetPointId( 1, 2 );
  cellpointer->SetPointId( 2, 1 );
  mesh->SetCell( 3, cellpointer );

  edge = new LineType;
  cellpointer.TakeOwnership( edge );
  cellpointer->SetPointId( 0, 0 );
  cellpointer->SetPointId( 1, 1 );
  mesh->SetCell( 4, cellpointer );

  edge = new LineType;
  cellpointer.TakeOwnership( edge );
  cellpointer->SetPointId( 0, 1 );
  cellpointer->SetPointId( 1, 2 );
  mesh->SetCell( 5, cellpointer );

  edge = new LineType;
  cellpointer.TakeOwnership( edge );
  cellpointer->SetPointId( 0, 2 );
  cellpointer->SetPointId( 1, 0 );
  mesh->SetCell( 6, cellpointer );

  edge = new LineType;
  cellpointer.TakeOwnership( edge );
  cellpointer->SetPointId( 0, 1 );
  cellpointer->SetPointId( 1, 3 );
  mesh->SetCell( 7, cellpointer );

  edge = new LineType;
  cellpointer.TakeOwnership( edge );
  cellpointer->SetPointId( 0, 3 );
  cellpointer->SetPointId( 1, 2 );
  mesh->SetCell( 8, cellpointer );

  edge = new LineType;
  edge->GetNameOfClass( );
  edge->GetType( );
  edge->GetDimension( );
  cellpointer.TakeOwnership( edge );
  cellpointer->SetPointId( 0, 3 );
  cellpointer->SetPointId( 1, 0 );
  mesh->SetCell( 9, cellpointer );

  std::cout << "numPoints = " << mesh->GetNumberOfPoints() << std::endl;
  std::cout << "numCells  = " << mesh->GetNumberOfCells() << std::endl;

  typedef MeshType::PointsContainer::ConstIterator  PointIterator;
  PointIterator pointIterator = mesh->GetPoints()->Begin();
  PointIterator pointEnd      = mesh->GetPoints()->End();

  while( pointIterator != pointEnd )
    {
    std::cout << pointIterator.Value() << std::endl;
    pointIterator++;
    }

  typedef MeshType::CellsContainer::ConstIterator CellIterator;

  CellIterator cellIterator = mesh->GetCells()->Begin();
  CellIterator cellEnd      = mesh->GetCells()->End();

  while( cellIterator != cellEnd )
    {
    CellType* cell = cellIterator.Value();
    std::cout << cell->GetNumberOfPoints() << std::endl;
    ++cellIterator;
    }

  cellIterator = mesh->GetCells()->Begin();
  cellEnd      = mesh->GetCells()->End();

  while( cellIterator != cellEnd )
    {
    CellType* cell = cellIterator.Value();

    std::cout << "cell with " << cell->GetNumberOfPoints();
    std::cout << " points   : ";

    typedef CellType::PointIdIterator PointIdIterator;

    PointIdIterator pointIditer = cell->PointIdsBegin();
    PointIdIterator pointIdend  = cell->PointIdsEnd();

    if( pointIditer != pointIdend )
      {
      std::cout << *pointIditer;
      ++pointIditer;
      }

    while( pointIditer != pointIdend )
      {
      std::cout << " -> " << *pointIditer;
      ++pointIditer;
      }

    std::cout << std::endl;
    ++cellIterator;
    }

  //try to clear the populated mesh
  mesh->Clear( );

  std::cout << "Mesh3Test passed" << std::endl;
  return EXIT_SUCCESS;
}
