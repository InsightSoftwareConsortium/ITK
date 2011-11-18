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

#include "itkQuadEdgeMesh.h"

#include <iostream>

/**
 * Some typedefs to make things easier.
 */


typedef itk::QuadEdgeMesh< double, 3 >  MeshType;


typedef MeshType::CellTraits  CellTraits;


/**
 * The type of point stored in the mesh. Because mesh was instantiated
 * with defaults (itkDefaultDynamicMeshTraits), the point dimension is 3 and
 * the coordinate representation is float.
 */
typedef MeshType::PointType             PointType;
typedef PointType::VectorType           VectorType;

typedef MeshType::Pointer               MeshPointer;
typedef MeshType::ConstPointer          MeshConstPointer;

typedef MeshType::PointType             PointType;

typedef MeshType::PointsContainer       PointsContainer;
typedef MeshType::PointDataContainer    PointDataContainer;

typedef PointsContainer::Iterator       PointsIterator;
typedef PointDataContainer::Iterator    CellsIterator;

int itkDynamicQuadEdgeMeshTest(int, char* [] )
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
  displacement[2] = 0;

  pointA.Fill( 0.0 );
  pointB = pointA + displacement;
  pointC = pointB + displacement;
  pointD = pointC + displacement;

  PointsContainer::Pointer pointsContainter = mesh->GetPoints();

  pointsContainter->SetElement( 0, pointA );
  pointsContainter->SetElement( 1, pointB );
  pointsContainter->SetElement( 2, pointC );
  pointsContainter->SetElement( 3, pointD );


  std::cout << "Number of Points = " << mesh->GetNumberOfPoints() << std::endl;

  PointsIterator point    = pointsContainter->Begin();
  PointsIterator endpoint = pointsContainter->End();

  while( point != endpoint )
    {
    std::cout << point.Index() << " = " << point.Value() << std::endl;
    point++;
    }


  return 0;

}
