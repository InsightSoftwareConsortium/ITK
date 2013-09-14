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

#include <iostream>

#include "itkMesh.h"
#include "itkHexahedronCell.h"
#include "itkTetrahedronCell.h"
#include "itkQuadraticTriangleCell.h"
#include "itkPolygonCell.h"

/**
 * Define a mesh type that stores a PixelType of "int".  Use the defaults
 * for the other template parameters.
 */
typedef itk::Mesh<int>                        MeshType;
typedef MeshType::CellTraits                  CellTraits;
typedef itk::CellInterface< int, CellTraits > CellInterfaceType;

/**
 * Typedef the generic cell type for the mesh.  It is an abstract class,
 * so we can only use information from it, like get its pointer type.
 */
typedef MeshType::CellType              CellType;
typedef CellType::CellAutoPointer       CellAutoPointer;

// Test the cell interface

template<typename TCell> int TestCellInterface(std::string name, TCell *aCell)
{
  CellAutoPointer cell(aCell,true);

  std::cout << "-------- " << name << " (" << aCell->GetNameOfClass() << ")" << std::endl;
  std::cout << "    Type: " << cell->GetType() << std::endl;
  std::cout << "    Dimension: " << cell->GetDimension() << std::endl;
  std::cout << "    NumberOfPoints: " << cell->GetNumberOfPoints() << std::endl;
  std::cout << "    NumberOfBoundaryFeatures:" << std::endl;
  for (unsigned int i = 0; i < cell->GetDimension(); i++)
    {
    std::cout << "      " << i << ": " << cell->GetNumberOfBoundaryFeatures(i) << std::endl;
    for (unsigned int j = 0; j < cell->GetNumberOfBoundaryFeatures(i); j++)
      {
      CellAutoPointer feature;
      cell->GetBoundaryFeature(i, j, feature);
      }
    }

  std::cout << "    Iterator test: PointIds for empty cell: ";
  typename TCell::PointIdIterator pointId = cell->PointIdsBegin();
  typename TCell::PointIdIterator endId = cell->PointIdsEnd();
  while (pointId != endId)
    {
    std::cout << *pointId << ", ";
    pointId++;
    }
  std::cout << std::endl;

  std::cout << "    ConstIterator test: PointIds for empty cell: ";
  typename TCell::PointIdConstIterator cpointId = cell->PointIdsBegin();
  typename TCell::PointIdConstIterator cendId = cell->PointIdsEnd();
  while (cpointId != cendId)
    {
    std::cout << *cpointId << ", ";
    cpointId++;
    }
  std::cout << std::endl;

  // Add point ids
  std::cout << "    SetPointIds" << std::endl;

  typedef typename TCell::PointIdentifier   PointIdentifier;

  PointIdentifier *pointIds = new PointIdentifier[cell->GetNumberOfPoints() * 2];

  for (unsigned int i = 0; i < cell->GetNumberOfPoints() * 2; i++)
    {
    pointIds[i] = i;
    }

  cell->SetPointIds(pointIds);
  if (cell->GetNumberOfPoints() > 0)
    {
    cell->SetPointId(0, 100);
    }

  std::cout << "    ConstIterator test: PointIds for populated cell: ";
  typename TCell::PointIdConstIterator ppointId = cell->PointIdsBegin();
  typename TCell::PointIdConstIterator pendId = cell->PointIdsEnd();
  while (ppointId != pendId)
    {
    std::cout << *ppointId << ", ";
    ppointId++;
    }
  std::cout << std::endl;

  cell->SetPointIds(&pointIds[cell->GetNumberOfPoints()],
                    &pointIds[cell->GetNumberOfPoints() * 2]);
  std::cout << "    Iterator test: PointIds for populated cell: ";
  typename TCell::PointIdIterator pxpointId = cell->PointIdsBegin();
  typename TCell::PointIdIterator pxendId = cell->PointIdsEnd();
  while (pxpointId != pxendId)
    {
    std::cout << *pxpointId << ", ";
    pxpointId++;
    }
  std::cout << std::endl;

  // Make a copy
  CellAutoPointer copyOfCell;
  cell->MakeCopy(copyOfCell);
  std::cout << "    PointIds for copied cell: ";
  typename TCell::PointIdConstIterator xpointId = copyOfCell->PointIdsBegin();
  typename TCell::PointIdConstIterator xendId = copyOfCell->PointIdsEnd();
  while (xpointId != xendId)
    {
    std::cout << *xpointId << ", ";
    xpointId++;
    }
  std::cout << std::endl;


  delete[] pointIds;
  return EXIT_SUCCESS;
}
int itkCellInterfaceTest(int, char* [] )
{
  int status;

  typedef itk::VertexCell<CellInterfaceType> VertexCellType;
  status = TestCellInterface("Vertex", new VertexCellType);
  if (status != 0)
    {
    return EXIT_FAILURE;
    }

  typedef itk::LineCell<CellInterfaceType> LineCellType;
  status = TestCellInterface("Line", new LineCellType);
  if (status != 0)
    {
    return EXIT_FAILURE;
    }

  typedef itk::TriangleCell<CellInterfaceType> TriangleCellType;
  status = TestCellInterface("Triangle", new TriangleCellType);
  if (status != 0)
    {
    return EXIT_FAILURE;
    }

  typedef itk::HexahedronCell<CellInterfaceType> HexahedronCellType;
  status = TestCellInterface("HexahedronCell", new HexahedronCellType);
  if (status != 0)
    {
    return EXIT_FAILURE;
    }

  typedef itk::TetrahedronCell<CellInterfaceType> TetrahedronCellType;
  status = TestCellInterface("TetrahedronCell", new TetrahedronCellType);
  if (status != 0)
    {
    return EXIT_FAILURE;
    }

  typedef itk::QuadraticEdgeCell<CellInterfaceType> QuadraticEdgeCellType;
  status = TestCellInterface("QuadraticEdgeCell", new QuadraticEdgeCellType);
  if (status != 0)
    {
    return EXIT_FAILURE;
    }

  typedef itk::QuadraticTriangleCell<CellInterfaceType> QuadraticTriangleCellType;
  status = TestCellInterface("QuadraticTriangleCell", new QuadraticTriangleCellType);
  if (status != 0)
    {
    return EXIT_FAILURE;
    }

  typedef itk::QuadrilateralCell<CellInterfaceType> QuadrilateralCellType;
  status = TestCellInterface("QuadrilateralCell", new QuadrilateralCellType);
  if (status != 0)
    {
    return EXIT_FAILURE;
    }

  typedef itk::PolygonCell<CellInterfaceType> PolygonCellType;
  status = TestCellInterface("PolygonCell with 0 vertices", new PolygonCellType());
  if (status != 0)
    {
    return EXIT_FAILURE;
    }

  typedef itk::PolygonCell<CellInterfaceType> PolygonCellType;
  status = TestCellInterface("PolygonCell with 5 vertices", new PolygonCellType(5));
  if (status != 0)
    {
    return EXIT_FAILURE;
    }

  return status;
}
