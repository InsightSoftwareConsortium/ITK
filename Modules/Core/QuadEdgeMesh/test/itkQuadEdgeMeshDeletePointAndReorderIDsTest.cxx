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
#include "itkMath.h"

int itkQuadEdgeMeshDeletePointAndReorderIDsTest( int , char* [] )
{

  typedef double PixelType;
  const unsigned int Dimension = 3;
  typedef itk::QuadEdgeMesh< PixelType, Dimension > MeshType;
  typedef MeshType::CellType                        CellType;
  typedef itk::QuadEdgeMeshPolygonCell< CellType >  QEPolygonCellType;

  MeshType::Pointer  mesh = MeshType::New();
  MeshType::PointType pts[ 5 ];
  MeshType::PixelType ptData = 0.;

  pts[ 0 ][ 0 ] = -1.0; pts[ 0 ][ 1 ] = -1.0; pts[ 0 ][ 2 ] = 0.0;
  pts[ 1 ][ 0 ] =  1.0; pts[ 1 ][ 1 ] = -1.0; pts[ 1 ][ 2 ] = 0.0;
  pts[ 2 ][ 0 ] =  1.0; pts[ 2 ][ 1 ] =  1.0; pts[ 2 ][ 2 ] = 0.0;
  pts[ 3 ][ 0 ] = -1.0; pts[ 3 ][ 1 ] =  1.0; pts[ 3 ][ 2 ] = 0.0;
  pts[ 4 ][ 0 ] =  0.0; pts[ 4 ][ 1 ] =  0.0; pts[ 4 ][ 2 ] = 1.0;

  for( unsigned int e = 0; e < 5; e++ )
    {
    mesh->SetPoint( e, pts[ e ] );
    mesh->SetPointData( e, e );
    }

  // create a tetahedra and one isolated point: id = 0
  int specialCells[12] =
  {  4,  1,  2,
     4,  2,  3,
     3,  1,  4,
     1,  3,  2 };

  CellType::CellAutoPointer cellpointer;
  QEPolygonCellType *poly;
  for(int i=0; i<4; i++)
    {
    poly = new QEPolygonCellType( 3 );
    cellpointer.TakeOwnership( poly );
    cellpointer->SetPointId( 0, specialCells[3*i] );
    cellpointer->SetPointId( 1, specialCells[3*i+1] );
    cellpointer->SetPointId( 2, specialCells[3*i+2] );
    mesh->SetCell( i, cellpointer );
    }

  // Point exists along with pointData
  if(  ! mesh->GetPoints()->IndexExists( 0 )
    || ! mesh->GetPointData()->IndexExists( 0 ) )
    {
    return EXIT_FAILURE;
    }

  // delete point and check that point and pointdata have been removed
  // form corresponding containers
  mesh->DeletePoint( 0 );
  if(  mesh->GetPoints()->IndexExists( 0 )
    || mesh->GetPointData()->IndexExists( 0 ) )
    {
    return EXIT_FAILURE;
    }

  // Squeeze point IDs. ID=0 should reappear but have a different value
  // actually it should have the value of the previously last point
  mesh->SqueezePointsIds( );
  if(  ! mesh->GetPoints()->IndexExists( 0 )
    || ! mesh->GetPointData()->IndexExists( 0 ) )
    {
    // should check coordinates

    // check data
    mesh->GetPointData( 0, &ptData );
    if( itk::Math::NotAlmostEquals( ptData, 4 ) )
    {
    return EXIT_FAILURE;
    }
  }

  return EXIT_SUCCESS;
}
