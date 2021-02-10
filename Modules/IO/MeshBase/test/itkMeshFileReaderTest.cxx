/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#include "itkMeshFileReader.h"
#include "itkQuadEdgeMesh.h"
#include "itkTestingMacros.h"


int
itkMeshFileReaderTest(int argc, char * argv[])
{
  if (argc < 2)
  {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " inputFileName" << std::endl;
    return EXIT_FAILURE;
  }

  using coord = double;
  constexpr unsigned int Dimension = 3;

  using MeshType = itk::QuadEdgeMesh<coord, Dimension>;

  using ReaderType = itk::MeshFileReader<MeshType>;
  ReaderType::Pointer reader = ReaderType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(reader, MeshFileReader, MeshSource);


  // Test exceptions
  std::string inputFileName = "";
  reader->SetFileName(inputFileName);

  inputFileName = argv[1];
  reader->SetFileName(inputFileName);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
