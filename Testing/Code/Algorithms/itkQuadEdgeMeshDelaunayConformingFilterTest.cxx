/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshDelaunayConformingFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkVTKPolyDataReader.h"
#include "itkVTKPolyDataWriter.h"
#include "itkQuadEdgeMesh.h"

// NEW
#include "itkQuadEdgeMeshDelaunayConformingFilter.h"

int itkQuadEdgeMeshDelaunayConformingFilterTest( int argc, char* argv[] )
{
  // ** ERROR MESSAGE AND HELP ** //
  if( argc < 3 )
    {
    std::cout <<"Requires 2 argument: " <<std::endl;
    std::cout <<"1-Input file name " <<std::endl;
    std::cout <<"2-Output file name " <<std::endl;
    return EXIT_FAILURE;
    }

  // ** TYPEDEF **
  typedef double Coord;

  typedef itk::QuadEdgeMesh< Coord, 3 >                      MeshType;
  typedef itk::VTKPolyDataReader< MeshType >                 ReaderType;
  typedef itk::VTKPolyDataWriter< MeshType >                 WriterType;

  // ** READ THE FILE IN **
  ReaderType::Pointer reader = ReaderType::New( );
  reader->SetFileName( argv[1] );
  try
    {
    reader->Update( );
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception thrown while reading the input file " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  MeshType::Pointer mesh = reader->GetOutput( );

  typedef itk::QuadEdgeMeshDelaunayConformingFilter< MeshType, MeshType >
  DelaunayConformFilterType;
  DelaunayConformFilterType::Pointer filter = DelaunayConformFilterType::New( );
  filter->SetInput( mesh );
  filter->Update( );

  // ** WRITE OUTPUT **
  WriterType::Pointer writer = WriterType::New( );
  writer->SetInput( filter->GetOutput( ) );
  writer->SetFileName( argv[2] );
  writer->Update( );

  std::cout <<"Input: " <<argv[1] <<std::endl;
  std::cout <<"Output: " <<argv[2] <<std::endl;
  std::cout <<"Number of Edge flipped performed: "
    <<filter->GetNumberOfEdgeFlips( ) <<std::endl;

  return EXIT_SUCCESS;
}
