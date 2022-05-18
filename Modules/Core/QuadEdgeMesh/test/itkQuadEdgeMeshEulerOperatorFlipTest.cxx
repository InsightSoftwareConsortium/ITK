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

#include "itkQuadEdgeMeshEulerOperatorFlipEdgeFunction.h"
#include "itkQuadEdgeMeshEulerOperatorsTestHelper.h"

int
itkQuadEdgeMeshEulerOperatorFlipTest(int, char *[])
{

  using MeshType = itk::QuadEdgeMesh<double, 3>;
  using MeshPointer = MeshType::Pointer;
  using QEType = MeshType::QEType;

  using FlipEdge = itk::QuadEdgeMeshEulerOperatorFlipEdgeFunction<MeshType, QEType>;

  MeshPointer mesh = MeshType::New();
  CreateSquareTriangularMesh<MeshType>(mesh);
  auto flipEdge = FlipEdge::New();
  std::cout << flipEdge << std::endl;

#ifndef NDEBUG
  if (flipEdge->Evaluate((QEType *)1))
  {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "OK" << std::endl;
#endif

  std::cout << flipEdge->GetNameOfClass() << std::endl;

  flipEdge->SetInput(mesh);

#ifndef NDEBUG
  std::cout << "     "
            << "Test QE Input not internal";
  QEType * dummy = new QEType;
  if (flipEdge->Evaluate(dummy))
  {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
  }
  delete dummy;
  std::cout << "OK" << std::endl;
  std::cout << "     "
            << "Test No QE Input";
  if (flipEdge->Evaluate((QEType *)nullptr))
  {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "OK" << std::endl;
#endif

  mesh->LightWeightDeleteEdge(mesh->FindEdge(12, 18));
  mesh->AddFace(mesh->FindEdge(17, 12));
  std::cout << "     "
            << "Flip an edge with a polygonal face (impossible)";
  QEType * tempFlippedEdge = flipEdge->Evaluate(mesh->FindEdge(12, 17));
  if (tempFlippedEdge)
  {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
  }

  CreateSquareTriangularMesh<MeshType>(mesh);
  std::cout << "     "
            << "Flip an edge (possible)";
  tempFlippedEdge = flipEdge->Evaluate(mesh->FindEdge(12, 6));
  if (!tempFlippedEdge)
  {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
  }
  // The number of edges and faces must be unchanged:
  if (!AssertTopologicalInvariants<MeshType>(mesh, 25, 56, 32, 1, 0))
  {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
  }
  if (mesh->GetPoint(12).GetValence() != 5)
  {
    std::cout << "FAILED [wrong valence of " << mesh->GetPoint(12).GetValence() << " for vertex 12 ]." << std::endl;
    return EXIT_FAILURE;
  }
  if (mesh->GetPoint(6).GetValence() != 5)
  {
    std::cout << "FAILED [wrong valence of " << mesh->GetPoint(6).GetValence() << " for vertex 6 ]." << std::endl;
    return EXIT_FAILURE;
  }
  if (mesh->GetPoint(11).GetValence() != 7)
  {
    std::cout << "FAILED [wrong valence of " << mesh->GetPoint(11).GetValence() << " for vertex 11 ]." << std::endl;
    return EXIT_FAILURE;
  }
  if (mesh->GetPoint(7).GetValence() != 7)
  {
    std::cout << "FAILED [wrong valence of " << mesh->GetPoint(7).GetValence() << " for vertex 7 ]." << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << ".OK" << std::endl;
  // Checking invariance (i.e. FlipEdge is its own inverse):
  std::cout << "     "
            << "Check FlipEdge(FlipEdge()) invariance (possible for triangles).";
  if (!flipEdge->Evaluate(tempFlippedEdge))
  {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
  }
  // The number of edges and faces must be unchanged:
  if (!AssertTopologicalInvariants<MeshType>(mesh, 25, 56, 32, 1, 0))
  {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
  }
  if (mesh->GetPoint(12).GetValence() != 6)
  {
    std::cout << "FAILED [wrong valence of " << mesh->GetPoint(12).GetValence() << " for vertex 12 ]." << std::endl;
    return EXIT_FAILURE;
  }
  if (mesh->GetPoint(6).GetValence() != 6)
  {
    std::cout << "FAILED [wrong valence of " << mesh->GetPoint(6).GetValence() << " for vertex 6 ]." << std::endl;
    return EXIT_FAILURE;
  }
  if (mesh->GetPoint(11).GetValence() != 6)
  {
    std::cout << "FAILED [wrong valence of " << mesh->GetPoint(11).GetValence() << " for vertex 11 ]." << std::endl;
    return EXIT_FAILURE;
  }
  if (mesh->GetPoint(7).GetValence() != 6)
  {
    std::cout << "FAILED [wrong valence of " << mesh->GetPoint(7).GetValence() << " for vertex 7 ]." << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "OK" << std::endl;
  std::cout << "Checking FlipEdge."
            << "OK" << std::endl
            << std::endl;

  // Test streaming enumeration for QuadEdgeMeshEulerOperatorFlipEdgeFunctionEnums::EdgeStatusType elements
  const std::set<itk::QuadEdgeMeshEulerOperatorFlipEdgeFunctionEnums::EdgeStatusType> allEdgeStatusType{
    itk::QuadEdgeMeshEulerOperatorFlipEdgeFunctionEnums::EdgeStatusType::STANDARD_CONFIG,
    itk::QuadEdgeMeshEulerOperatorFlipEdgeFunctionEnums::EdgeStatusType::EDGE_NULL,
    itk::QuadEdgeMeshEulerOperatorFlipEdgeFunctionEnums::EdgeStatusType::MESH_NULL,
    itk::QuadEdgeMeshEulerOperatorFlipEdgeFunctionEnums::EdgeStatusType::NON_INTERNAL_EDGE,
    itk::QuadEdgeMeshEulerOperatorFlipEdgeFunctionEnums::EdgeStatusType::NON_TRIANGULAR_RIGHT_FACE,
    itk::QuadEdgeMeshEulerOperatorFlipEdgeFunctionEnums::EdgeStatusType::NON_TRIANGULAR_LEFT_FACE,
    itk::QuadEdgeMeshEulerOperatorFlipEdgeFunctionEnums::EdgeStatusType::EXISTING_OPPOSITE_EDGE
  };
  for (const auto & ee : allEdgeStatusType)
  {
    std::cout << "STREAMED ENUM VALUE QuadEdgeMeshEulerOperatorFlipEdgeFunctionEnums::EdgeStatusType: " << ee
              << std::endl;
  }
  return EXIT_SUCCESS;
}
