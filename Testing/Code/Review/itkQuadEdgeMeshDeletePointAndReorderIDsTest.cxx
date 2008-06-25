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
#include "itkVTKPolyDataWriter.h"

int itkQuadEdgeMeshDeletePointAndReorderIDsTest( int , char* [] )
{

  typedef double PixelType;
  const unsigned int Dimension = 3;
  typedef itk::QuadEdgeMesh< PixelType, Dimension > MeshType;
  typedef MeshType::CellTraits                      CellTraits;
  typedef CellTraits::QuadEdgeType                  QEType;
  typedef MeshType::CellType                        CellType;
  typedef itk::QuadEdgeMeshPolygonCell< CellType >  QEPolygonCellType;
  typedef itk::VTKPolyDataWriter< MeshType >        WriterType;

  MeshType::Pointer  mesh = MeshType::New();
  MeshType::PointType pts[ 5 ];
 
  WriterType::Pointer writer = WriterType::New( );
 
  pts[ 0 ][ 0 ] = -1.0; pts[ 0 ][ 1 ] = -1.0; pts[ 0 ][ 2 ] = 0.0;
  pts[ 1 ][ 0 ] =  1.0; pts[ 1 ][ 1 ] = -1.0; pts[ 1 ][ 2 ] = 0.0;
  pts[ 2 ][ 0 ] =  1.0; pts[ 2 ][ 1 ] =  1.0; pts[ 2 ][ 2 ] = 0.0;
  pts[ 3 ][ 0 ] = -1.0; pts[ 3 ][ 1 ] =  1.0; pts[ 3 ][ 2 ] = 0.0;
  pts[ 4 ][ 0 ] =  0.0; pts[ 4 ][ 1 ] =  0.0; pts[ 4 ][ 2 ] = 1.0;

  mesh->SetPoint( 0, pts[ 0 ] );
  mesh->SetPoint( 1, pts[ 1 ] );
  mesh->SetPoint( 2, pts[ 2 ] );
  mesh->SetPoint( 3, pts[ 3 ] );
  mesh->SetPoint( 4, pts[ 4 ] );


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

  writer->SetInput( mesh );
  writer->SetFileName( "BeforeDeletePoint.vtk" );
  writer->Update( );

  mesh->DeletePoint( 0 );
  writer->SetFileName( "AfterDeletePoint.vtk" );
  writer->Update( );
 
  mesh->SqueezePointsIds( );  
  writer->SetFileName( "AfterSqueeze.vtk" );
  writer->Update( );

  return EXIT_SUCCESS;
}
