/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshMaxCurvatureTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include <itkQuadEdgeMesh.h>
#include <itkVTKPolyDataReader.h>

#include "itkQuadEdgeMeshExtendedTraits.h"
#include "itkQuadEdgeMeshDiscreteMaxCurvatureEstimator.h"
#include "itkQuadEdgeMeshScalarDataVTKPolyDataWriter.h"

int itkQuadEdgeMeshMaxCurvatureTest( int argc, char* argv[] )
{
  if( argc < 2 )
    {
    std::cout <<"*** GaussianCurvature ***" <<std::endl;
    std::cout <<"This example requires at least one argument:" <<std::endl;
    std::cout <<" 1- FileName" <<std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int Dimension = 3;
  typedef double CoordType;

  typedef itk::QuadEdgeMeshExtendedTraits <
    CoordType,
    Dimension,
    2,
    CoordType,
    CoordType,
    CoordType,
    bool,
    bool > Traits;

  typedef itk::QuadEdgeMesh< CoordType, Dimension, Traits > MeshType;
  typedef itk::QuadEdgeMeshDiscreteMaxCurvatureEstimator<
    MeshType, MeshType > CurvatureFilterType;

  typedef itk::VTKPolyDataReader< MeshType > ReaderType;

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

  MeshType::Pointer mesh = reader->GetOutput();

  CurvatureFilterType::Pointer max_curvature = CurvatureFilterType::New();
  max_curvature->SetInput( mesh );
  max_curvature->Update();

  MeshType::Pointer output = max_curvature->GetOutput();

  typedef itk::QuadEdgeMeshScalarDataVTKPolyDataWriter< MeshType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( output );
  writer->SetFileName( "max_curvature.vtk" );
  writer->Update();

  return EXIT_SUCCESS;
}
