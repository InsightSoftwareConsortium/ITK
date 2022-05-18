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

int
itkQuadEdgeMeshTest1(int, char *[])
{
  std::cout << "Testing points..." << std::endl;

  using PixelType = double;
  constexpr unsigned int Dimension = 3;
  using MeshType = itk::QuadEdgeMesh<PixelType, Dimension>;
  using CellTraits = MeshType::CellTraits;
  using QEType = CellTraits::QuadEdgeType;
  using CellType = MeshType::CellType;
  using QEPolygonCellType = itk::QuadEdgeMeshPolygonCell<CellType>;
  using QELineCellType = itk::QuadEdgeMeshLineCell<CellType>;

  auto mesh = MeshType::New();

  mesh->GetCellBoundaryFeatureNeighbors(0, 0, 0, nullptr);
  mesh->GetCellNeighbors(0, nullptr);

  // test ComputeNumberOfPoints( ) failsafe
  {
    if (mesh->ComputeNumberOfPoints())
    {
      return EXIT_FAILURE;
    }
  }

  MeshType::PointType pts[5];

  pts[0][0] = -1.0;
  pts[0][1] = -1.0;
  pts[0][2] = 0.0;
  pts[1][0] = 1.0;
  pts[1][1] = -1.0;
  pts[1][2] = 0.0;
  pts[2][0] = 1.0;
  pts[2][1] = 1.0;
  pts[2][2] = 0.0;
  pts[3][0] = -1.0;
  pts[3][1] = 1.0;
  pts[3][2] = 0.0;
  pts[4][0] = 0.0;
  pts[4][1] = 0.0;
  pts[4][2] = 1.0;

  mesh->SetPoint(0, pts[0]);
  mesh->SetPoint(1, pts[1]);
  mesh->SetPoint(2, pts[2]);
  mesh->SetPoint(3, pts[3]);
  mesh->SetPoint(4, pts[4]);

  if (mesh->GetNumberOfPoints() != 5)
  {
    std::cout << "Not all points added." << std::endl;
    return EXIT_FAILURE;
  }

  using PointsIterator = MeshType::PointsContainer::Iterator;
  PointsIterator pointIterator = mesh->GetPoints()->Begin();
  PointsIterator end = mesh->GetPoints()->End();

  int nPoints = 0;
  while (pointIterator != end)
  {
    MeshType::PointType p = pointIterator.Value();

    if (p != pts[nPoints])
    {
      std::cout << "Point N. " << nPoints << " differs." << std::endl;
      return EXIT_FAILURE;
    }

    pointIterator++;
    nPoints++;
  }

  if (nPoints != 5)
  {
    std::cout << "Iteration didn't visit all points." << std::endl;
    return EXIT_FAILURE;
  }

  // Test AddEdge failsafe
  {
    if (mesh->AddEdge(1, 1))
    {
      std::cout << "Should not be able to define an edge"
                << " with twice the same Point identifier." << std::endl;
      return EXIT_FAILURE;
    }
    if (mesh->AddEdge(1, 6))
    {
      std::cout << "Should not be able to define an edge"
                << " with a non existing point Id." << std::endl;
      return EXIT_FAILURE;
    }

    // create a tetahedra and one isolated point: id = 4
    int specialCells[12] = { 0, 1, 2, 0, 2, 3, 3, 1, 0, 1, 3, 2 };

    CellType::CellAutoPointer cellpointer;
    QEPolygonCellType *       poly;
    for (int i = 0; i < 4; ++i)
    {
      poly = new QEPolygonCellType(3);
      cellpointer.TakeOwnership(poly);
      cellpointer->SetPointId(0, specialCells[3 * i]);
      cellpointer->SetPointId(1, specialCells[3 * i + 1]);
      cellpointer->SetPointId(2, specialCells[3 * i + 2]);
      mesh->SetCell(i, cellpointer);
    }

    // test origin internal
    if (mesh->AddEdge(4, 1))
    {
      std::cout << "Should not be able to define an edge"
                << " with an internal origin." << std::endl;
      return EXIT_FAILURE;
    }

    // test destination internal
    if (mesh->AddEdge(1, 4))
    {
      std::cout << "Should not be able to define an edge"
                << " with an internal destination." << std::endl;
      return EXIT_FAILURE;
    }
  }

  // test DeletePoint failsafes
  {
    mesh->DeletePoint(1); // that will not delete the point
    // check it.
  }

  // test DeleteEdge failsafe
  {
    // deleteEdge( pid, pid )
    mesh->DeleteEdge(1, 4); // that will not delete the edge
    // check it.

    // deleteEdge( QEPrimal * )
    // check with a disconnected edge

    // LightWeightDeleteEdge
    auto * qeLineCell = new QELineCellType;
    mesh->LightWeightDeleteEdge((QEType *)nullptr);
    mesh->LightWeightDeleteEdge(qeLineCell->GetQEGeom());
    mesh->LightWeightDeleteEdge(qeLineCell);
  }

  // test delete face failsafe
  {
    mesh->DeleteFace(1299877); // should fail, Id too high
  }

  // Test GetEdge failsafe
  {
    if (!mesh->GetEdge(0))
    {
      std::cout << "Should be able to get this edge" << std::endl;
      return EXIT_FAILURE;
    }

    if (mesh->GetEdge(129987))
    {
      std::cout << "Should not be able to get an edge"
                << " with a non existing ID." << std::endl;
      return EXIT_FAILURE;
    }
  }

  // test AddFace failsafe
  {
    if (mesh->AddFaceTriangle(0, 1, 129987))
    {
      std::cout << "Should not be able to add a face"
                << " with a non existing ID." << std::endl;
      return EXIT_FAILURE;
    }
  }

  std::cout << "Test passed" << std::endl;
  return EXIT_SUCCESS;
}
