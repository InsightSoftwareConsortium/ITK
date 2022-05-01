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

#include "itkMeshFileTestHelper.h"
#include "itkTestingMacros.h"

int
itkMeshFileReadWriteVectorAttributeTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " inputFileName outputFileName [isBinary]"
              << std::endl;
    return EXIT_FAILURE;
  }

  bool isBinary = (argc > 3);

  constexpr unsigned int Dimension = 3;
  using PixelType = itk::CovariantVector<float, Dimension>;

  using MeshType = itk::Mesh<PixelType, Dimension>;
  using QEMeshType = itk::QuadEdgeMesh<PixelType, Dimension>;

  int result = EXIT_SUCCESS;

  if (test<MeshType>(argv[1], argv[2], isBinary))
  {
    std::cerr << "Failure for itk::Mesh" << std::endl;
    result = EXIT_FAILURE;
  }

  if (test<QEMeshType>(argv[1], argv[2], isBinary))
  {
    std::cerr << "Failure for itk::QuadEdgeMesh" << std::endl;
    result = EXIT_FAILURE;
  }

  return result;
}
