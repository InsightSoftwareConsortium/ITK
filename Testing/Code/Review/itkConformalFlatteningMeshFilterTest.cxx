/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConformalFlatteningMeshFilterTest.cxx
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
#include <stdlib.h>

int itkConformalFlatteningMeshFilterTest(int argc, char *argv[])
{
  if( argc < 4 )
    {
     std::cerr << "Usage: "<< argv[0] \
               << "vtkInputFilename vtkOutputFilename mapToSphere[0:1] [polarCellId]\n";

    return EXIT_FAILURE;
    }

  typedef itk::Mesh< double, 3 > MeshType;
 
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

  CellIdentifier  polarCellId = 0; // default set to the first cell

  if( argc > 4 )
    {
    polarCellId = atoi( argv[4] );
    }

  filter->SetPolarCellIdentifier( polarCellId );

  int mapToSphere = atoi( argv[3] );

  if( mapToSphere == 1 )
    {
    filter->MapToSphere();
    }
  else
    {
    filter->MapToPlane();
    }

  //  double scale = atof( argv[4] );
  //  filter->SetScale( scale );

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
