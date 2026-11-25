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

// Regression test for issue #3453: itkCleanQuadEdgeMeshFilter must
// (1) preserve per-point and per-cell data, and
// (2) leave every polygon cell referencing valid output point IDs after
//     SqueezePointsIds() compacts the point container.

#include "itkCleanQuadEdgeMeshFilter.h"
#include "itkQuadEdgeMesh.h"
#include "itkRegularSphereMeshSource.h"
#include "itkTestingMacros.h"

namespace
{

using PixelType = float;
constexpr unsigned int Dimension = 3;
using MeshType = itk::QuadEdgeMesh<PixelType, Dimension>;
using PointType = MeshType::PointType;

MeshType::Pointer
BuildSphereWithDataAttached()
{
  // Use the standard regular-sphere mesh source so the topology is
  // guaranteed valid, then attach distinct per-point and per-cell data
  // so we can detect loss after the cleaner runs.
  using SphereSource = itk::RegularSphereMeshSource<MeshType>;
  auto src = SphereSource::New();
  src->SetResolution(2); // 66 points, 128 triangles
  src->Update();

  const MeshType::Pointer mesh = src->GetOutput();

  for (auto pIt = mesh->GetPoints()->Begin(); pIt != mesh->GetPoints()->End(); ++pIt)
  {
    mesh->SetPointData(pIt.Index(), static_cast<PixelType>(pIt.Index()));
  }
  for (auto cIt = mesh->GetCells()->Begin(); cIt != mesh->GetCells()->End(); ++cIt)
  {
    mesh->SetCellData(cIt.Index(), static_cast<PixelType>(1000 + cIt.Index()));
  }
  return mesh;
}

} // namespace

int
itkCleanQuadEdgeMeshFilterDataPreservationTest(int, char *[])
{
  using FilterType = itk::CleanQuadEdgeMeshFilter<MeshType, MeshType>;

  const MeshType::Pointer input = BuildSphereWithDataAttached();
  std::cout << "Input  : " << input->GetNumberOfPoints() << " points, " << input->GetNumberOfCells() << " cells, "
            << input->GetPointData()->Size() << " point-data entries, "
            << (input->GetCellData() ? input->GetCellData()->Size() : 0) << " cell-data entries" << std::endl;

  auto filter = FilterType::New();
  filter->SetInput(input);
  // Relative tolerance large enough to force the decimator to merge
  // several edges, exercising both the data-copy and the
  // SqueezePointsIds() remap paths.
  filter->SetRelativeTolerance(0.3);

  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());

  const MeshType::Pointer output = filter->GetOutput();
  std::cout << "Output : " << output->GetNumberOfPoints() << " points, " << output->GetNumberOfCells() << " cells, "
            << (output->GetPointData() ? output->GetPointData()->Size() : 0) << " point-data entries, "
            << (output->GetCellData() ? output->GetCellData()->Size() : 0) << " cell-data entries" << std::endl;

  bool ok = true;

  // Sanity: cleaner should have merged the duplicate points.
  if (output->GetNumberOfPoints() >= input->GetNumberOfPoints())
  {
    std::cerr << "FAIL: filter did not reduce point count (input=" << input->GetNumberOfPoints()
              << ", output=" << output->GetNumberOfPoints() << ')' << std::endl;
    ok = false;
  }

  // Bug A1: per-point data must be preserved (one entry per output point).
  const MeshType::PointDataContainer * outPointData = output->GetPointData();
  if (!outPointData || outPointData->Size() != output->GetNumberOfPoints())
  {
    std::cerr << "FAIL: point data not preserved (have "
              << (outPointData ? static_cast<long>(outPointData->Size()) : -1L) << " entries, expected "
              << output->GetNumberOfPoints() << ')' << std::endl;
    ok = false;
  }

  // Bug A2: per-cell data must be preserved (one entry per output cell).
  const MeshType::CellDataContainer * outCellData = output->GetCellData();
  if (!outCellData || outCellData->Size() != output->GetNumberOfCells())
  {
    std::cerr << "FAIL: cell data not preserved (have " << (outCellData ? static_cast<long>(outCellData->Size()) : -1L)
              << " entries, expected " << output->GetNumberOfCells() << ')' << std::endl;
    ok = false;
  }

  // Bug B: every polygon cell's point IDs must reference an existing
  // output point.
  using CellsContainer = MeshType::CellsContainer;
  const CellsContainer * cells = output->GetCells();
  if (cells)
  {
    for (auto cIt = cells->Begin(); cIt != cells->End(); ++cIt)
    {
      const MeshType::CellType * cell = cIt.Value();
      for (auto pIt = cell->PointIdsBegin(); pIt != cell->PointIdsEnd(); ++pIt)
      {
        const MeshType::PointIdentifier id = *pIt;
        if (!output->GetPoints()->IndexExists(id))
        {
          std::cerr << "FAIL: cell " << cIt.Index() << " references nonexistent point " << id << std::endl;
          ok = false;
        }
      }
    }
  }

  return ok ? EXIT_SUCCESS : EXIT_FAILURE;
}
