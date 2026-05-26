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

#include "itkMeshToPolyDataFilter.h"
#include "itkMesh.h"
#include "itkTriangleCell.h"
#include "itkTestingMacros.h"
#include "itkMath.h"

// Regression test for cell-data preservation: an input mesh carrying
// per-cell data must round-trip through MeshToPolyDataFilter with the
// cell data copied 1:1 in cell-id order.  Earlier the cell-id tracking
// containers were sized with VectorContainer::Reserve (which resizes
// with zero-filled entries) instead of the STL capacity hint reserve,
// producing spurious leading entries that corrupted the cell-data copy.
// The fixture-based test uses cow.vtk, which carries no cell data, so
// this in-memory test is the only coverage of that path.
int
itkMeshToPolyDataFilterCellDataTest(int, char *[])
{
  constexpr unsigned int Dimension = 3;
  using PixelType = float;
  using MeshType = itk::Mesh<PixelType, Dimension>;
  using PointType = MeshType::PointType;
  using CellType = MeshType::CellType;
  using CellAutoPointer = CellType::CellAutoPointer;
  using TriangleCellType = itk::TriangleCell<CellType>;

  auto mesh = MeshType::New();

  PointType p;
  p[0] = 0.0;
  p[1] = 0.0;
  p[2] = 0.0;
  mesh->SetPoint(0, p);
  p[0] = 1.0;
  mesh->SetPoint(1, p);
  p[1] = 1.0;
  mesh->SetPoint(2, p);
  p[0] = 0.0;
  mesh->SetPoint(3, p);

  // Two triangles sharing the (0,2) edge.
  CellAutoPointer triangle;
  triangle.TakeOwnership(new TriangleCellType);
  triangle->SetPointId(0, 0);
  triangle->SetPointId(1, 1);
  triangle->SetPointId(2, 2);
  mesh->SetCell(0, triangle);

  triangle.TakeOwnership(new TriangleCellType);
  triangle->SetPointId(0, 0);
  triangle->SetPointId(1, 2);
  triangle->SetPointId(2, 3);
  mesh->SetCell(1, triangle);

  mesh->SetCellData(0, 10.0f);
  mesh->SetCellData(1, 20.0f);

  using FilterType = itk::MeshToPolyDataFilter<MeshType>;
  auto filter = FilterType::New();
  filter->SetInput(mesh);
  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());

  using PolyDataType = FilterType::PolyDataType;
  const PolyDataType * polyData = filter->GetOutput();

  // Both cells are triangles, so they land in the polygons container.
  ITK_TEST_EXPECT_EQUAL(polyData->GetPolygons()->GetElement(0), 3);

  const auto * cellData = polyData->GetCellData();
  ITK_TEST_EXPECT_TRUE(cellData != nullptr);
  ITK_TEST_EXPECT_EQUAL(cellData->Size(), 2);
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual<float>(cellData->GetElement(0), 10.0f, 10, 1e-4));
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual<float>(cellData->GetElement(1), 20.0f, 10, 1e-4));

  return EXIT_SUCCESS;
}
