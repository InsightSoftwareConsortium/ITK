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
#include "itkLineCell.h"
#include "itkPolyLineCell.h"
#include "itkTestingMacros.h"
#include "itkMath.h"

// Regression test for cell-data ordering when a mesh interleaves LineCell
// and PolyLineCell cells.  Both map into the single output line container,
// which is merged polyline-cells-first then line-cells; the cell-data copy
// must follow that same order.  A shared, visit-order cell-id container made
// the data land on the wrong output cells whenever both types were present.
int
itkMeshToPolyDataFilterLinePolyLineCellDataTest(int, char *[])
{
  constexpr unsigned int Dimension = 3;
  using PixelType = float;
  using MeshType = itk::Mesh<PixelType, Dimension>;
  using PointType = MeshType::PointType;
  using CellType = MeshType::CellType;
  using CellAutoPointer = CellType::CellAutoPointer;
  using LineCellType = itk::LineCell<CellType>;
  using PolyLineCellType = itk::PolyLineCell<CellType>;

  auto mesh = MeshType::New();

  PointType p;
  p[1] = 0.0;
  p[2] = 0.0;
  for (unsigned int i = 0; i < 5; ++i)
  {
    p[0] = static_cast<double>(i);
    mesh->SetPoint(i, p);
  }

  // Interleave so cell-visit order (line, polyline, line) differs from the
  // output merge order (polyline, then the two lines).
  CellAutoPointer cell;

  cell.TakeOwnership(new LineCellType);
  cell->SetPointId(0, 0);
  cell->SetPointId(1, 1);
  mesh->SetCell(0, cell);

  cell.TakeOwnership(new PolyLineCellType(3));
  cell->SetPointId(0, 1);
  cell->SetPointId(1, 2);
  cell->SetPointId(2, 3);
  mesh->SetCell(1, cell);

  cell.TakeOwnership(new LineCellType);
  cell->SetPointId(0, 3);
  cell->SetPointId(1, 4);
  mesh->SetCell(2, cell);

  mesh->SetCellData(0, 10.0f); // line
  mesh->SetCellData(1, 20.0f); // polyline
  mesh->SetCellData(2, 30.0f); // line

  using FilterType = itk::MeshToPolyDataFilter<MeshType>;
  auto filter = FilterType::New();
  filter->SetInput(mesh);
  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());

  using PolyDataType = FilterType::PolyDataType;
  const PolyDataType * polyData = filter->GetOutput();

  const auto * cellData = polyData->GetCellData();
  ITK_TEST_EXPECT_TRUE(cellData != nullptr);
  ITK_TEST_EXPECT_EQUAL(cellData->Size(), 3);

  // Output order: polyline (cell 1) first, then the two lines (cells 0, 2).
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual<float>(cellData->GetElement(0), 20.0f, 10, 1e-4));
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual<float>(cellData->GetElement(1), 10.0f, 10, 1e-4));
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual<float>(cellData->GetElement(2), 30.0f, 10, 1e-4));

  return EXIT_SUCCESS;
}
