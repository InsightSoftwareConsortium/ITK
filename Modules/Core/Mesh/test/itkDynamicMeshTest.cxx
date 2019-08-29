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

#include "itkMesh.h"
#include "itkDefaultDynamicMeshTraits.h"

#include <iostream>

/**
 * Some type alias to make things easier.
 */

/**
 * Define a mesh type that stores a PixelType of "int".  Use the defaults
 * for the other template parameters.
 */
using MeshTraits = itk::DefaultDynamicMeshTraits<int, 2, 2, float, float>;

using MeshType = itk::Mesh<MeshTraits::PixelType, MeshTraits::PointDimension, MeshTraits>;


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
itkDynamicMeshTest(int, char *[])
{

  /**
   * Create the mesh through its object factory.
   */
  MeshType::Pointer mesh(MeshType::New());

  PointType pointA;
  PointType pointB;
  PointType pointC;
  PointType pointD;

  VectorType displacement;

  displacement[0] = 2;
  displacement[1] = 5;

  pointA.Fill(0.0);
  pointB = pointA + displacement;
  pointC = pointB + displacement;
  pointD = pointC + displacement;

  PointsContainer::Pointer pointsContainter = mesh->GetPoints();

  pointsContainter->SetElement(0, pointA);
  pointsContainter->SetElement(1, pointB);
  pointsContainter->SetElement(2, pointC);
  pointsContainter->SetElement(3, pointD);


  std::cout << "Number of Points = " << mesh->GetNumberOfPoints() << std::endl;

  PointsIterator point = pointsContainter->Begin();
  PointsIterator endpoint = pointsContainter->End();

  while (point != endpoint)
  {
    std::cout << point.Index() << " = " << point.Value() << std::endl;
    point++;
  }


  return EXIT_SUCCESS;
}
