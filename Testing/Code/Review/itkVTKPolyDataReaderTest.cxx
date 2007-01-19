/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVTKPolyDataReaderTest.cxx
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


#include "itkMesh.h"
#include "itkVTKPolyDataReader.h"
#include "itkDefaultStaticMeshTraits.h"

#include <iostream>

int itkVTKPolyDataReaderTest(int argc, char* argv[] )
{
  if( argc != 2 )
    {
    std::cerr << "Usage: itkVTKPolyDataReaderTest inputFilename"
      << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::Mesh<float, 3>                 MeshType;
  typedef itk::VTKPolyDataReader< MeshType >  ReaderType;

  ReaderType::Pointer  polyDataReader = ReaderType::New();

  typedef ReaderType::PointType   PointType;
  typedef ReaderType::VectorType  VectorType;

  polyDataReader->SetFileName(argv[1]);

  try
    {
    polyDataReader->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error during Update() " << std::endl;
    std::cerr << excp << std::endl;
    }

  std::cout << "polyDataReader: " << polyDataReader;

  MeshType::Pointer mesh = polyDataReader->GetOutput();

  PointType  pt;

  std::cout << "Testing itk::VTKPolyDataReader" << std::endl;

  for(unsigned int i=0; i<mesh->GetNumberOfPoints(); i++)
    {
    mesh->GetPoint(i, &pt);
    std::cout << "Point[" << i << "]: " << pt << std::endl;
    }

  std::cout << "Test passed"<< std::endl;
  return EXIT_SUCCESS;
}

