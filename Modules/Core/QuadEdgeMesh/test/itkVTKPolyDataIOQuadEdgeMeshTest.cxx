/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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
#include "itkTestingMacros.h"

#include <iostream>

int
itkVTKPolyDataIOQuadEdgeMeshTest(int argc, char * argv[])
{
  if (argc != 3)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " inputFilename outputFilename" << std::endl;
    return EXIT_FAILURE;
  }

  using MeshType = itk::QuadEdgeMesh<float, 3>;
  using ReaderType = itk::VTKPolyDataReader<MeshType>;
  using WriterType = itk::VTKPolyDataWriter<MeshType>;

  auto polyDataReader = ReaderType::New();
  auto polyDataWriter = WriterType::New();

  polyDataReader->SetFileName(argv[1]);
  polyDataWriter->SetFileName(argv[2]);

  std::cout << "polyDataReader:" << std::endl;
  std::cout << polyDataReader << std::endl;

  ITK_TRY_EXPECT_NO_EXCEPTION(polyDataReader->Update());


  MeshType::Pointer mesh = polyDataReader->GetOutput();

  polyDataWriter->SetInput(mesh);

  std::cout << "polyDataWriter:" << std::endl;
  std::cout << polyDataWriter << std::endl;

  ITK_TRY_EXPECT_NO_EXCEPTION(polyDataWriter->Update());


  // Should make a diff

  std::cout << "Test passed" << std::endl;
  return EXIT_SUCCESS;
}
