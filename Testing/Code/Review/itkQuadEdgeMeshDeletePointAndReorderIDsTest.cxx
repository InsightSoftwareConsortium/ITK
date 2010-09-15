/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshDeletePointAndReorderIDsTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkQuadEdgeMesh.h"
#include "itkQuadEdgeMeshPolygonCell.h"

int itkQuadEdgeMeshDeletePointAndReorderIDsTest( int , char* [] )
{

  typedef double PixelType;
  const unsigned int Dimension = 3;
  typedef itk::QuadEdgeMesh< PixelType, Dimension > MeshType;
  typedef MeshType::CellTraits                      CellTraits;
  typedef CellTraits::QuadEdgeType                  QEType;
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
    if( ptData != 4 )
    {
    return EXIT_FAILURE;
    }
  }

  return EXIT_SUCCESS;
}
