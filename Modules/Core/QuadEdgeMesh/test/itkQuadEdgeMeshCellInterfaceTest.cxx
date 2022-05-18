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

#include <iostream>

#include "itkQuadEdgeMesh.h"

#include "itkHexahedronCell.h"
#include "itkTetrahedronCell.h"
#include "itkQuadraticTriangleCell.h"
#include "itkPolygonCell.h"


/**
 * Define a mesh type that stores a PixelType of "int".  Use the defaults
 * for the other template parameters.
 */
using PixelType = int;
using MeshType = itk::QuadEdgeMesh<PixelType, 3>;
using CellTraits = MeshType::CellTraits;
using QEType = CellTraits::QuadEdgeType;
using CellInterfaceType = itk::CellInterface<int, CellTraits>;
using QELineCellType = itk::QuadEdgeMeshLineCell<CellInterfaceType>;
using QEPolygonCellType = itk::QuadEdgeMeshPolygonCell<CellInterfaceType>;

/**
 * Typedef the generic cell type for the mesh.  It is an abstract class,
 * so we can only use information from it, like get its pointer type.
 */
using CellType = MeshType::CellType;
using CellAutoPointer = CellType::CellAutoPointer;

class CustomQELineVisitor
{
public:
  void
  Visit(unsigned long cellId, QELineCellType * t)
  {
    (void)cellId;
    (void)t;
  }
  virtual ~CustomQELineVisitor() = default;
};

class CustomQEPolyVisitor
{
public:
  void
  Visit(unsigned long cellId, QEPolygonCellType * t)
  {
    (void)cellId;
    (void)t;
  }
  virtual ~CustomQEPolyVisitor() = default;
};

// Test the cell interface
template <typename TCell>
int
TestCellInterface(std::string name, TCell * aCell)
{

  CellAutoPointer cell(aCell, true);
  const TCell *   cell2 = aCell;

  std::cout << "-------- " << name << "(" << aCell->GetNameOfClass() << ")" << std::endl;
  std::cout << "    Type: " << static_cast<int>(cell->GetType()) << std::endl;
  std::cout << "    Dimension: " << cell->GetDimension() << std::endl;
  std::cout << "    NumberOfPoints: " << cell->GetNumberOfPoints() << std::endl;
  std::cout << "    NumberOfBoundaryFeatures:" << std::endl;
  // Note the <= is here to test the default case
  for (unsigned int i = 0; i <= cell->GetDimension(); ++i)
  {
    std::cout << "      " << i << ": " << cell->GetNumberOfBoundaryFeatures(i) << std::endl;
    for (unsigned int j = 0; j < cell->GetNumberOfBoundaryFeatures(i); ++j)
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
  typename TCell::PointIdConstIterator cpointId = cell2->PointIdsBegin();
  typename TCell::PointIdConstIterator cendId = cell2->PointIdsEnd();
  while (cpointId != cendId)
  {
    std::cout << *cpointId << ", ";
    cpointId++;
  }
  std::cout << std::endl;

  // Add point ids
  std::cout << "    SetPointIds" << std::endl;

  using PointIdentifier = MeshType::PointIdentifier;

  auto * pointIds = new PointIdentifier[cell->GetNumberOfPoints() * 2];
  for (unsigned int i = 0; i < cell->GetNumberOfPoints() * 2; ++i)
  {
    pointIds[i] = i;
  }

  cell->SetPointIds(pointIds);
  // exercing the const GetPointIds() method
  // null for QE Cells
  if (cell2->GetPointIds())
  {
    cell->SetPointIds(cell2->GetPointIds());
  }
  if (cell->GetNumberOfPoints() > 0)
  {
    cell->SetPointId(0, 100);
  }

  std::cout << "    ConstIterator test: PointIds for populated cell: ";
  typename TCell::PointIdConstIterator ppointId = cell2->PointIdsBegin();
  typename TCell::PointIdConstIterator pendId = cell2->PointIdsEnd();
  while (ppointId != pendId)
  {
    std::cout << *ppointId << ", ";
    ppointId++;
  }
  std::cout << std::endl;

  cell->SetPointIds(&pointIds[cell->GetNumberOfPoints()], &pointIds[cell->GetNumberOfPoints() * 2]);
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

// Test the QEcell interface

template <typename TCell>
int
TestQECellInterface(std::string name, TCell * aCell)
{
  std::cout << "-------- " << name << "(" << aCell->GetNameOfClass() << ")" << std::endl;

  TCell *       cell = aCell;
  const TCell * cell2 = aCell;

  std::cout << "    QE Iterator test: PointIds for empty cell: ";
  typename TCell::PointIdInternalIterator pointId = cell->InternalPointIdsBegin();
  typename TCell::PointIdInternalIterator endId = cell->InternalPointIdsEnd();
  while (pointId != endId)
  {
    std::cout << *pointId << ", ";
    pointId++;
  }
  std::cout << std::endl;

  std::cout << "    ConstIterator test: PointIds for empty cell: ";
  typename TCell::PointIdInternalConstIterator cpointId = cell2->InternalPointIdsBegin();
  typename TCell::PointIdInternalConstIterator cendId = cell2->InternalPointIdsEnd();

  while (cpointId != cendId)
  {
    std::cout << *cpointId << ", ";
    cpointId++;
  }
  std::cout << std::endl;

  // Add point ids
  std::cout << "    SetPointIds" << std::endl;

  using PointIdentifier = typename TCell::PointIdentifier;

  auto * pointIds = new PointIdentifier[cell->GetNumberOfPoints() * 2];
  for (unsigned int i = 0; i < cell->GetNumberOfPoints() * 2; ++i)
  {
    pointIds[i] = i;
  }

  // actually populate
  cell->SetPointIds(pointIds);
  // exercing the non const internal equivalent.
  cell->InternalSetPointIds(cell->InternalGetPointIds());
  // exercing the const internal equivalent
  cell->InternalSetPointIds(cell2->InternalGetPointIds());


  std::cout << "    ConstIterator test: PointIds for populated cell: ";
  typename TCell::PointIdInternalConstIterator ppointId = cell2->InternalPointIdsBegin();
  typename TCell::PointIdInternalConstIterator pendId = cell2->InternalPointIdsEnd();
  while (ppointId != pendId)
  {
    std::cout << *ppointId << ", ";
    ppointId++;
  }
  std::cout << std::endl;

  cell->InternalSetPointIds(cell2->InternalPointIdsBegin(), cell2->InternalPointIdsEnd());
  std::cout << "    Iterator test: PointIds for populated cell: ";
  typename TCell::PointIdInternalIterator pxpointId = cell->InternalPointIdsBegin();
  typename TCell::PointIdInternalIterator pxendId = cell->InternalPointIdsEnd();
  while (pxpointId != pxendId)
  {
    std::cout << *pxpointId << ", ";
    pxpointId++;
  }
  std::cout << std::endl;

  delete[] pointIds;
  return EXIT_SUCCESS;
}


int
itkQuadEdgeMeshCellInterfaceTest(int, char *[])
{
  int status;

  /*
   * ITK NATIVE CELLS
   */
  using VertexCellType = itk::VertexCell<CellInterfaceType>;
  status = TestCellInterface("Vertex", new VertexCellType);
  if (status != 0)
  {
    return EXIT_FAILURE;
  }

  using LineCellType = itk::LineCell<CellInterfaceType>;
  status = TestCellInterface("Line", new LineCellType);
  if (status != 0)
  {
    return EXIT_FAILURE;
  }

  using TriangleCellType = itk::TriangleCell<CellInterfaceType>;
  status = TestCellInterface("Triangle", new TriangleCellType);
  if (status != 0)
  {
    return EXIT_FAILURE;
  }

  using HexahedronCellType = itk::HexahedronCell<CellInterfaceType>;
  status = TestCellInterface("HexahedronCell", new HexahedronCellType);
  if (status != 0)
  {
    return EXIT_FAILURE;
  }

  using TetrahedronCellType = itk::TetrahedronCell<CellInterfaceType>;
  status = TestCellInterface("TetrahedronCell", new TetrahedronCellType);
  if (status != 0)
  {
    return EXIT_FAILURE;
  }

  using QuadraticEdgeCellType = itk::QuadraticEdgeCell<CellInterfaceType>;
  status = TestCellInterface("QuadraticEdgeCell", new QuadraticEdgeCellType);
  if (status != 0)
  {
    return EXIT_FAILURE;
  }

  using QuadraticTriangleCellType = itk::QuadraticTriangleCell<CellInterfaceType>;
  status = TestCellInterface("QuadraticTriangleCell", new QuadraticTriangleCellType);
  if (status != 0)
  {
    return EXIT_FAILURE;
  }

  using QuadrilateralCellType = itk::QuadrilateralCell<CellInterfaceType>;
  status = TestCellInterface("QuadrilateralCell", new QuadrilateralCellType);
  if (status != 0)
  {
    return EXIT_FAILURE;
  }

  using PolygonCellType = itk::PolygonCell<CellInterfaceType>;
  status = TestCellInterface("PolygonCell with 0 vertices", new PolygonCellType());
  if (status != 0)
  {
    return EXIT_FAILURE;
  }

  using PolygonCellType = itk::PolygonCell<CellInterfaceType>;
  status = TestCellInterface("PolygonCell with 5 vertices", new PolygonCellType(5));
  if (status != 0)
  {
    return EXIT_FAILURE;
  }

  // ITK QuadEdgeMesh CELLS - Standard cell API test

  auto * tempQEPolygon = new QEPolygonCellType;
  status = TestCellInterface("QuadEdgePolygonCell with 0 vertices", tempQEPolygon);
  if (status != 0)
  {
    return EXIT_FAILURE;
  }

  tempQEPolygon = new QEPolygonCellType(5);
  status = TestCellInterface("QuadEdgePolygonCell with 5 vertices", tempQEPolygon);
  if (status != 0)
  {
    return EXIT_FAILURE;
  }

  status = TestCellInterface("QuadEdgeLineCell", new QELineCellType());
  if (status != 0)
  {
    return EXIT_FAILURE;
  }

  // ITK QuadEdgeMesh CELLS - Specific QEcell API test

  tempQEPolygon = new QEPolygonCellType();

  status = TestQECellInterface("QuadEdgePolygonCell with 0 vertices", tempQEPolygon);
  delete tempQEPolygon;

  if (status != 0)
  {
    return EXIT_FAILURE;
  }

  tempQEPolygon = new QEPolygonCellType(5);
  status = TestQECellInterface("QuadEdgePolygonCell with 5 vertices", tempQEPolygon);
  delete tempQEPolygon;

  if (status != 0)
  {
    return EXIT_FAILURE;
  }

  auto * tempQELine = new QELineCellType;
  status = TestQECellInterface("QuadEdgeLineCell", tempQELine);
  delete tempQELine;

  if (status != 0)
  {
    return EXIT_FAILURE;
  }

  // test the visitor API
  using QELineVisitorInterfaceType =
    itk::CellInterfaceVisitorImplementation<PixelType, MeshType::CellTraits, QELineCellType, CustomQELineVisitor>;
  auto QELineVisitor = QELineVisitorInterfaceType::New();

  using QEPolyVisitorInterfaceType =
    itk::CellInterfaceVisitorImplementation<PixelType, MeshType::CellTraits, QEPolygonCellType, CustomQEPolyVisitor>;
  auto QEPolyVisitor = QEPolyVisitorInterfaceType::New();


  using CellMultiVisitorType = CellType::MultiVisitor;
  auto multiVisitor = CellMultiVisitorType::New();
  multiVisitor->AddVisitor(QELineVisitor);
  multiVisitor->AddVisitor(QEPolyVisitor);

  auto                mesh = MeshType::New();
  MeshType::PointType pts[3];
  pts[0][0] = 0;
  pts[0][1] = 0;
  pts[0][2] = 0;
  pts[1][0] = 0;
  pts[1][1] = 0;
  pts[1][2] = 1;
  pts[2][0] = 0;
  pts[2][1] = 1;
  pts[2][2] = 0;
  mesh->SetPoint(0, pts[0]);
  mesh->SetPoint(1, pts[1]);
  mesh->SetPoint(2, pts[2]);
  mesh->AddFaceTriangle(0, 1, 2);

  mesh->Accept(multiVisitor);

  // test 4 very specific QELineCell destructor cases
  auto *   test = new QELineCellType();
  QEType * m_QuadEdgeGeom = test->GetQEGeom();
  delete m_QuadEdgeGeom->GetRot()->GetRot()->GetRot();
  m_QuadEdgeGeom->GetRot()->GetRot()->SetRot(nullptr);
  delete test;

  test = new QELineCellType();
  m_QuadEdgeGeom = test->GetQEGeom();
  delete m_QuadEdgeGeom->GetRot()->GetRot()->GetRot();
  m_QuadEdgeGeom->GetRot()->GetRot()->SetRot(nullptr);
  delete m_QuadEdgeGeom->GetRot()->GetRot();
  m_QuadEdgeGeom->GetRot()->SetRot(nullptr);
  delete test;

  test = new QELineCellType();
  m_QuadEdgeGeom = test->GetQEGeom();
  delete m_QuadEdgeGeom->GetRot()->GetRot()->GetRot();
  m_QuadEdgeGeom->GetRot()->GetRot()->SetRot(nullptr);
  delete m_QuadEdgeGeom->GetRot()->GetRot();
  m_QuadEdgeGeom->GetRot()->SetRot(nullptr);
  delete m_QuadEdgeGeom->GetRot();
  m_QuadEdgeGeom->SetRot(nullptr);
  delete test;

  return status;
}
