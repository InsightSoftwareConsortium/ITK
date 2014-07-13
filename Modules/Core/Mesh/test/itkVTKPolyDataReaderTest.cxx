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

#include "itkVTKPolyDataReader.h"

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

  polyDataReader->SetFileName(argv[1]);

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

  std::cout << "polyDataReader:" << std::endl;
  std::cout << polyDataReader << std::endl;

  MeshType::Pointer mesh = polyDataReader->GetOutput();

  PointType  point;

  std::cout << "Testing itk::VTKPolyDataReader" << std::endl;

  unsigned int numberOfPoints = mesh->GetNumberOfPoints();
  unsigned int numberOfCells  = mesh->GetNumberOfCells();

  std::cout << "numberOfPoints= " << numberOfPoints << std::endl;
  std::cout << "numberOfCells= " << numberOfCells << std::endl;

  if( !numberOfPoints )
    {
    std::cerr << "ERROR: numberOfPoints= " << numberOfPoints << std::endl;
    return EXIT_FAILURE;
    }

  if( !numberOfCells )
    {
    std::cerr << "ERROR: numberOfCells= " << numberOfCells << std::endl;
    return EXIT_FAILURE;
    }

  for(unsigned int i=0; i<numberOfPoints; i++)
    {
    mesh->GetPoint(i, &point);
    }

  std::cout << "Test passed"<< std::endl;
  return EXIT_SUCCESS;
}
