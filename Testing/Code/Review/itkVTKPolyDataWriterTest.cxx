/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVTKPolyDataWriterTest.cxx
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

#include "itkTriangleCell.h"
#include "itkLineCell.h"
#include "itkVTKPolyDataWriter.h"

int itkVTKPolyDataWriterTest(int argc, char* argv[])
{
  if( argc != 2 )
    {
    std::cerr << "Usage: itkVTKPolyDataWriter outputFileName" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int PointDimension = 3;

  typedef unsigned short    PixelType;
  typedef float             PointType;

  typedef itk::Mesh< PointType, PointDimension >  MeshType;

  typedef MeshType::CellTraits                        CellTraits;
  typedef itk::CellInterface< PointType, CellTraits > CellInterfaceType;
  typedef itk::TriangleCell< CellInterfaceType >      TriangleCellType;
  typedef itk::LineCell< CellInterfaceType >          LineCellType;

  typedef itk::VTKPolyDataWriter<MeshType>   WriterType;

  MeshType::Pointer mesh = MeshType::New();

  const unsigned int numberOfPoints = 4;
  const unsigned int numberOfCells  = 9;

  float rawPoints[12] = {
    0.0, 0.0, 0.0,
    1.0, 1.0, 0.0,
    0.0, 1.0, 1.0,
    1.0, 0.0, 1.0 };

  unsigned long rawCells[24] = {
    0, 2, 1,
    0, 1, 3,
    0, 3, 2,
    1, 2, 3,
    0, 1,
    0, 2,
    0, 3,
    1, 2,
    1, 3,
    2, 3 };

  mesh->GetPoints()->Reserve( numberOfPoints );
  mesh->GetCells()->Reserve( numberOfCells );

  MeshType::PointType point;

  for(unsigned int i=0; i<numberOfPoints; i++)
    {
    point[0] = rawPoints[3*i];
    point[1] = rawPoints[3*i+1];
    point[2] = rawPoints[3*i+2];
    mesh->SetPoint( i, point );
    }

  unsigned long pointIds[3];

  MeshType::CellAutoPointer cell;
  TriangleCellType * triangle;
  LineCellType *     line;

  for(unsigned int i=0; i<4; i++)
    {
    pointIds[0] = rawCells[3*i];
    pointIds[1] = rawCells[3*i+1];
    pointIds[2] = rawCells[3*i+2];

    triangle = new TriangleCellType;
    triangle->SetPointIds( pointIds );
    cell.TakeOwnership( triangle );
    mesh->SetCell( i, cell );
    }
  for(unsigned int i=4; i<10; i++)
    {
    pointIds[0] = rawCells[12+2*(i-4)];
    pointIds[1] = rawCells[12+2*(i-4)+1];

    line = new LineCellType;
    line->SetPointIds( pointIds );
    cell.TakeOwnership( line );
    mesh->SetCell( i, cell );
    }

  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( mesh );
  writer->SetFileName( argv[1] );
  writer->Write();

  std::cout << __LINE__ << " PrintSelf\n" << writer;

  return EXIT_SUCCESS;
}
