/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkQuadEdgeMesh.h"
#include "itkVTKPolyDataReader.h"
#include "itkVTKPolyDataWriter.h"

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
