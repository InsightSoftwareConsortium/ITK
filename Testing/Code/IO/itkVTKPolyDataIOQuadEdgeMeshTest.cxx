/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVTKPolyDataIOQuadEdgeMeshTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif


#include "itkQuadEdgeMesh.h"
#include "itkVTKPolyDataReader.h"
#include "itkVTKPolyDataWriter.h"
#include "itkDefaultStaticMeshTraits.h"

#include <iostream>

int itkVTKPolyDataIOQuadEdgeMeshTest(int argc, char* argv[] )
{

  typedef itk::QuadEdgeMesh<float, 3>         MeshType;
  typedef itk::VTKPolyDataReader< MeshType >  ReaderType;
  typedef itk::VTKPolyDataWriter< MeshType >  WriterType;

  ReaderType::Pointer  polyDataReader = ReaderType::New();
  WriterType::Pointer  polyDataWriter = WriterType::New();

  if( argc != 3 )
    {
    std::cerr << "Usage: itkVTKPolyDataReaderTest inputFilename outputFilename"
              << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    polyDataReader->SetFileName(argv[1]);
    polyDataWriter->SetFileName(argv[2]);
    }

  std::cout << "polyDataReader:" << std::endl;
  std::cout << polyDataReader << std::endl;
  try
    {
    polyDataReader->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error during Update() " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  MeshType::Pointer mesh = polyDataReader->GetOutput();

  polyDataWriter->SetInput( mesh );

  std::cout << "polyDataWriter:" << std::endl;
  std::cout << polyDataWriter << std::endl;
  try
    {
    polyDataWriter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error during Update() " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  // Should make a diff

  std::cout << "Test passed"<< std::endl;
  return EXIT_SUCCESS;
}
