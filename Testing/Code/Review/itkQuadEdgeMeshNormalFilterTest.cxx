/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshNormalFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkVector.h"
#include "itkQuadEdgeMesh.h"
#include "itkVTKPolyDataReader.h"

#include "itkQuadEdgeMeshExtendedTraits.h"
#include "itkQuadEdgeMeshNormalFilter.h"

int itkQuadEdgeMeshNormalFilterTest( int argc, char* argv[] )
{
  if( argc < 2 )
    {
    std::cout <<"This program requires at least 1 argument" <<std::endl;
    std::cout <<"1- Input filename" <<std::endl;
    return EXIT_FAILURE;
    }
  
  const unsigned int    Dimension = 3;
  typedef double        CoordType;

  typedef itk::QuadEdgeMesh< CoordType, Dimension > InputMeshType;

  typedef itk::Vector< CoordType, Dimension > VectorType;

  typedef itk::QuadEdgeMeshExtendedTraits <
    VectorType,
    Dimension,
    2,
    CoordType,
    CoordType,
    VectorType,
    bool,
    bool > Traits;

  typedef itk::QuadEdgeMesh < VectorType, Dimension, Traits > OutputMeshType;

  typedef itk::VTKPolyDataReader< InputMeshType > ReaderType;

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

  InputMeshType::Pointer mesh = reader->GetOutput( );

  typedef itk::QuadEdgeMeshNormalFilter< InputMeshType, OutputMeshType > NormalFilterType;

  NormalFilterType::Pointer normals = NormalFilterType::New( );
  normals->SetInput( mesh );
  normals->Update( );

  OutputMeshType::Pointer output = normals->GetOutput( );

  //
  //   FIXME
  //
  //     OutputMeshType::PointDataContainerPointer pointdata =
  //       output->GetPointData( );
  // 
  //     std::cout <<"*********************************" <<endl;
  //     std::cout <<"Vertex Normal" <<endl;
  //     for( OutputMeshType::PointDataContainerIterator
  //           d_it = pointdata->Begin( );
  //          d_it != pointdata->End( );
  //          d_it++ )
  //       {
  //       std::cout <<d_it->Index( ) <<"  " <<d_it->Value( ) <<endl;
  //       }
  // 
  //     std::cout <<endl;
  //     std::cout <<"*********************************" <<endl;
  //     std::cout <<"Face Normal" <<endl;
  // 
  //     OutputMeshType::CellDataContainerPointer celldata =
  //       output->GetCellData( );
  // 
  // 
  //     for( OutputMeshType::CellDataContainerIterator
  //           n_it = celldata->Begin( );
  //          n_it != celldata->End( );
  //          n_it++ )
  //       {
  //       std::cout <<n_it->Index( ) <<"  " <<n_it->Value( ) <<endl;
  //       }

  return EXIT_SUCCESS;
}
