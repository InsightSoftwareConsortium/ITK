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
#include "itkQuadEdgeMeshEulerOperatorsTestHelper.h"

int
itkQuadEdgeMeshCountingCellsTest(int, char *[])
{

  using MeshType = itk::QuadEdgeMesh<double, 3>;
  using MeshPointer = MeshType::Pointer;

  const MeshPointer mesh = MeshType::New();
  CreateSquareTriangularMesh<MeshType>(mesh);

  // The initial configuration and numbering of simpleSquare.vtk:
  //    Vertices: 25 , Edges: 56, Faces: 32, Boundary = 1, Chi = 1
  //
  //   20 --------- 21 --------- 22 --------- 23 --------- 24
  //    |        __/ |        __/ |        __/ |        __/ |
  //    |     __/    |     __/    |     __/    |     __/    |
  //    |  __/       |  __/       |  __/       |  __/       |
  //    | /          | /          | /          | /          |
  //   15 --------- 16 --------- 17 --------- 18 --------- 19
  //    |        __/ |        __/ |        __/ |        __/ |
  //    |     __/    |     __/    |     __/    |     __/    |
  //    |  __/       |  __/       |  __/       |  __/       |
  //    | /          | /          | /          | /          |
  //   10 --------- 11 --------- 12 --------- 13 --------- 14
  //    |        __/ |        __/ |        __/ |        __/ |
  //    |     __/    |     __/    |     __/    |     __/    |
  //    |  __/       |  __/       |  __/       |  __/       |
  //    | /          | /          | /          | /          |
  //    5 ---------- 6 ---------- 7 ---------- 8 ---------  9
  //    |        __/ |        __/ |        __/ |        __/ |
  //    |     __/    |     __/    |     __/    |     __/    |
  //    |  __/       |  __/       |  __/       |  __/       |
  //    | /          | /          | /          | /          |
  //    0 ---------- 1 ---------- 2  --------- 3 ---------  4
  //

  std::cout << "ITK API" << '\n';
  std::cout << "GetNumberOfCells   - Get Number of cells (all cells, from container)";
  std::cout << '\n';
  std::cout << "GetNumberOfPoints  - Get Number of points (all points, from container)";
  std::cout << '\n';
  std::cout << "ITK QE API" << '\n';
  std::cout << "ComputeNumberOfEdges - Only edges  - one container traversal" << '\n';
  std::cout << "ComputeNumberOfFaces - Only faces  - one container traversal" << '\n';
  std::cout << "ComputeNumberOfPoints- Only USED points - one container traversal" << '\n';
  std::cout << "GetNumberOfFaces     - Only faces  - member variable" << '\n';
  std::cout << "GetNumberOfEdges     - Only Edges  - member variable" << '\n';

  std::cout << '\n';
  std::cout << "START TEST" << '\n';
  std::cout << "ITK API" << '\n';
  std::cout << "GetNumberOfCells: " << mesh->GetNumberOfCells() << '\n';
  std::cout << "GetNumberOfPoints: " << mesh->GetNumberOfPoints() << '\n';

  std::cout << "ITK QE API" << '\n';
  std::cout << "ComputeNumberOfEdges: " << mesh->ComputeNumberOfEdges() << '\n';
  std::cout << "ComputeNumberOfFaces: " << mesh->ComputeNumberOfFaces() << '\n';
  std::cout << "ComputeNumberOfPoints: " << mesh->ComputeNumberOfPoints() << '\n';
  std::cout << "GetNumberOfFaces: " << mesh->GetNumberOfFaces() << '\n';
  std::cout << "GetNumberOfEdges: " << mesh->GetNumberOfEdges() << '\n';

  return EXIT_SUCCESS;
}
