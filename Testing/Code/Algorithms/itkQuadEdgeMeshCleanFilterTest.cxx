/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshCleanFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkQuadEdgeMesh.h"
#include "itkVTKPolyDataReader.h"
#include "itkVTKPolyDataWriter.h"

#include <sstream>

#include "itkQuadEdgeMeshCleanFilter.h"

int itkQuadEdgeMeshCleanFilterTest( int argc, char* argv[] )
{
  // ** ERROR MESSAGE AND HELP ** //
  if( argc < 3 )
    {
    std::cout <<"Requires 3 argument: " <<std::endl;
    std::cout <<"1-Input file name " <<std::endl;
    std::cout <<"2-Relative Tolerance " <<std::endl;
    std::cout <<"3-Output file name " <<std::endl;
    return EXIT_FAILURE;
    }

  // ** TYPEDEF **
  typedef double        Coord;
  const unsigned int    Dimension = 3;

  typedef itk::QuadEdgeMesh< Coord, Dimension >  MeshType;
  typedef itk::VTKPolyDataReader< MeshType >     ReaderType;
  typedef itk::VTKPolyDataWriter< MeshType >     WriterType;

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

  Coord tol;
  std::stringstream ssout( argv[2] );
  ssout >>tol;

  typedef itk::QuadEdgeMeshCleanFilter< MeshType, MeshType > CleanFilterType;
  CleanFilterType::Pointer filter = CleanFilterType::New();
  filter->SetInput( mesh );
  filter->SetRelativeTolerance( tol );
  filter->Update();

  // ** WRITE OUTPUT **
  WriterType::Pointer writer = WriterType::New( );
  writer->SetInput( filter->GetOutput( ) );
  writer->SetFileName( argv[3] );
  writer->Update( );

  return EXIT_SUCCESS;

}
