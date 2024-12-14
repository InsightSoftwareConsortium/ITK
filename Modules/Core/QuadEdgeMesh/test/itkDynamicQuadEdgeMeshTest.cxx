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

#include <iostream>

/**
 * Some type alias to make things easier.
 */


using MeshType = itk::QuadEdgeMesh<double, 3>;


using CellTraits = MeshType::CellTraits;


/**
 * The type of point stored in the mesh. Because mesh was instantiated
 * with defaults (itkDefaultDynamicMeshTraits), the point dimension is 3 and
 * the coordinate representation is float.
 */
using PointType = MeshType::PointType;
using VectorType = PointType::VectorType;

using MeshPointer = MeshType::Pointer;
using MeshConstPointer = MeshType::ConstPointer;

using PointType = MeshType::PointType;

using PointsContainer = MeshType::PointsContainer;
using PointDataContainer = MeshType::PointDataContainer;

using PointsIterator = PointsContainer::Iterator;
using CellsIterator = PointDataContainer::Iterator;

int
itkDynamicQuadEdgeMeshTest(int, char *[])
{

  /**
   * Create the mesh through its object factory.
   */
  const MeshType::Pointer mesh(MeshType::New());

  VectorType displacement;
  displacement[0] = 2;
  displacement[1] = 5;
  displacement[2] = 0;

  const PointType pointA{};

  const PointType pointB = pointA + displacement;
  const PointType pointC = pointB + displacement;
  const PointType pointD = pointC + displacement;

  const PointsContainer::Pointer pointsContainter = mesh->GetPoints();

  pointsContainter->SetElement(0, pointA);
  pointsContainter->SetElement(1, pointB);
  pointsContainter->SetElement(2, pointC);
  pointsContainter->SetElement(3, pointD);

  std::cout << "Number of Points = " << mesh->GetNumberOfPoints() << '\n';

  PointsIterator       point = pointsContainter->Begin();
  const PointsIterator endpoint = pointsContainter->End();

  while (point != endpoint)
  {
    std::cout << point.Index() << " = " << point.Value() << '\n';
    point++;
  }

  return EXIT_SUCCESS;
}
