/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConformalFlatteningQuadEdgeMeshFilterTest.cxx
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

#include "itkVTKPolyDataReader.h"
#include "itkVTKPolyDataWriter.h"
#include "itkConformalFlatteningMeshFilter.h"

#include "itkQuadEdgeMesh.h"
#include <stdlib.h>


int itkConformalFlatteningQuadEdgeMeshFilterTest(int argc, char *argv[])
{
  if( argc != 6 )
    {
    std::cerr << "Usage: itkConformalFlatteningMeshFilterTest "
     << "vtkInputFilename vtkOutputFilename "
     << "polarCellId scale mapToSphere[0:1]" << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::QuadEdgeMesh< double, 3 > MeshType;
 
  typedef itk::ConformalFlatteningMeshFilter<MeshType, MeshType>  FilterType;

  typedef itk::VTKPolyDataReader<MeshType>  ReaderType;
  typedef itk::VTKPolyDataWriter<MeshType>  WriterType;

  typedef MeshType::CellIdentifier  CellIdentifier;

  //
  // Read mesh file
  //

  std::cout << "Read " << argv[1] << std::endl;

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  try
    {
    reader->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  MeshType::Pointer mesh = reader->GetOutput();

  //
  // Test itkConformalFlatteningMeshFilter
  //

  FilterType::Pointer filter = FilterType::New();

  // Connect the input
  filter->SetInput( mesh );

  CellIdentifier  polarCellId = atoi( argv[3] );
  filter->SetPolarCellIdentifier( polarCellId );

  int mapToSphere = atoi( argv[5] );

  if( mapToSphere == 1 )
    {
    filter->MapToSphere();
    }
  else
    {
    filter->MapToPlane();
    }

  double scale = atof( argv[4] );

  filter->SetScale( scale );

  // Execute the filter

  std::cout << "Execute the filter" << std::endl;

  try
    {
    filter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  // Get the Smart Pointer to the Filter Output
  MeshType::Pointer newMesh = filter->GetOutput();

  //
  // Write to file
  //

  std::cout << "Write " << argv[2] << std::endl;

  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( newMesh );
  writer->SetFileName( argv[2] );

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
