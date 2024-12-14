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

#include "itkQuadEdgeMeshEulerOperatorSplitEdgeFunction.h"
#include "itkQuadEdgeMeshEulerOperatorsTestHelper.h"
#include "itkTestingMacros.h"

int
itkQuadEdgeMeshEulerOperatorSplitEdgeTest(int, char *[])
{

  using MeshType = itk::QuadEdgeMesh<double, 3>;
  using MeshPointer = MeshType::Pointer;
  using QEType = MeshType::QEType;

  using SplitEdge = itk::QuadEdgeMeshEulerOperatorSplitEdgeFunction<MeshType, QEType>;

  /////////////////////////////////////////
  //
  //          Split Edge
  //
  /////////////////////////////////////////
  std::cout << "Checking SplitEdge." << '\n';
  const MeshPointer mesh = MeshType::New();
  CreateSquareTriangularMesh<MeshType>(mesh);

  auto splitEdge = SplitEdge::New();
  std::cout << "     "
            << "Test No Mesh Input";
  if (splitEdge->Evaluate((QEType *)nullptr))
  {
    std::cout << "FAILED." << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "OK" << '\n';

  ITK_TEST_EXPECT_EQUAL(std::string_view("QuadEdgeMeshEulerOperatorSplitEdgeFunction"),
                        std::string_view(splitEdge->GetNameOfClass()));

  splitEdge->SetInput(mesh);
  std::cout << "     "
            << "Test No QE Input";
  if (splitEdge->Evaluate((QEType *)nullptr))
  {
    std::cout << "FAILED." << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "OK" << '\n';

  std::cout << "     ";
  std::cout << "Split an internal edge (possible).";
  if (!splitEdge->Evaluate(mesh->FindEdge(6, 12)))
  {
    std::cout << "FAILED." << '\n';
    return EXIT_FAILURE;
  }
  if (!AssertTopologicalInvariants<MeshType>(mesh, 26, 57, 32, 1, 0))
  {
    std::cout << "FAILED." << '\n';
    return EXIT_FAILURE;
  }
  std::cout << ".OK" << '\n';

  std::cout << "Checking SplitEdge."
            << "OK" << '\n'
            << '\n';
  return EXIT_SUCCESS;
}
