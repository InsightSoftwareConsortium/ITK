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

// ITK
#include <itkQuadEdgeMesh.h>
#include <itkRegularSphereMeshSource.h>
#include <itkModifiedButterflyTriangleCellSubdivisionQuadEdgeMeshFilter.h>
#include <itkLinearTriangleCellSubdivisionQuadEdgeMeshFilter.h>
#include <itkLoopTriangleCellSubdivisionQuadEdgeMeshFilter.h>
#include <itkSquareThreeTriangleCellSubdivisionQuadEdgeMeshFilter.h>
#include <itkTriangleHelper.h>
#include <itkIterativeTriangleCellSubdivisionQuadEdgeMeshFilter.h>

template <typename SubdivisionType>
void
SubdivisionTestHelper(const bool uniform)
{

  // Typedefs
  const unsigned int Dimension = 3;
  using TCoordinate = float;
  using TQEMesh = itk::QuadEdgeMesh<TCoordinate, Dimension>;
  using TSource = itk::RegularSphereMeshSource<TQEMesh>;
  using TTriangleHelper = itk::TriangleHelper<typename TQEMesh::PointType>;
  using TSubdivision = SubdivisionType;
  using TSubdivisionIt = itk::IterativeTriangleCellSubdivisionQuadEdgeMeshFilter<TQEMesh, TSubdivision>;

  // Generate some input mesh data
  const auto sphere = TSource::New();
  sphere->Update();

  const auto i_mesh = TQEMesh::New();
  i_mesh->Graft(sphere->GetOutput());
  i_mesh->DisconnectPipeline();

  typename TSubdivisionIt::SubdivisionCellContainer cells_to_subdivide;

  // Assign cell data (different for each octant).
  for (auto it = i_mesh->GetCells()->Begin(); it != i_mesh->GetCells()->End(); ++it)
  {
    const auto cell = it.Value();

    const auto centroid = TTriangleHelper::ComputeGravityCenter(i_mesh->GetPoint(cell->GetPointIds()[0]),
                                                                i_mesh->GetPoint(cell->GetPointIds()[1]),
                                                                i_mesh->GetPoint(cell->GetPointIds()[2]));

    unsigned int data = 0;
    if (centroid[0] < 0 && centroid[1] < 0 && centroid[2] < 0)
    {
      data = 1;
    }
    else if (centroid[0] < 0 && centroid[1] < 0 && centroid[2] >= 0)
    {
      data = 2;
    }
    else if (centroid[0] < 0 && centroid[1] >= 0 && centroid[2] < 0)
    {
      data = 3;
    }
    else if (centroid[0] < 0 && centroid[1] >= 0 && centroid[2] >= 0)
    {
      data = 4;
      cells_to_subdivide.push_back(it.Index());
    }
    else if (centroid[0] >= 0 && centroid[1] < 0 && centroid[2] < 0)
    {
      data = 5;
    }
    else if (centroid[0] >= 0 && centroid[1] < 0 && centroid[2] >= 0)
    {
      data = 6;
    }
    else if (centroid[0] >= 0 && centroid[1] >= 0 && centroid[2] < 0)
    {
      data = 7;
    }
    else if (centroid[0] >= 0 && centroid[1] >= 0 && centroid[2] >= 0)
    {
      data = 8;
    }

    i_mesh->SetCellData(it.Index(), data);
  }

  // Assert one CellData entry for each Cell
  const auto i_cell = i_mesh->GetNumberOfCells();
  const auto i_data = i_mesh->GetCellData()->Size();
  itkAssertOrThrowMacro(i_cell == i_data, "Incorrect number of entries in input cell data array.");

  const size_t resolution = 3;

  const auto subdivide = TSubdivisionIt::New();
  subdivide->SetResolutionLevels(resolution);
  if (!uniform)
  {
    subdivide->SetCellsToBeSubdivided(cells_to_subdivide);
  }
  subdivide->SetInput(i_mesh);
  subdivide->Update();

  const auto o_mesh = TQEMesh::New();
  o_mesh->Graft(subdivide->GetOutput());
  o_mesh->DisconnectPipeline();

  // Assert one CellData entry for each Cell
  const auto o_cell = o_mesh->GetNumberOfCells();
  const auto o_data = o_mesh->GetCellData()->Size();
  itkAssertOrThrowMacro(o_cell == o_data, "Incorrect number of entries in output cell data array.");
}

int
itkTriangleCellSubdivisionQuadEdgeMeshFilterCellDataTest(int, char *[])
{

  const unsigned int Dimension = 3;
  using TCoordinate = float;
  using TQEMesh = itk::QuadEdgeMesh<TCoordinate, Dimension>;

  using TButterfly = itk::ModifiedButterflyTriangleCellSubdivisionQuadEdgeMeshFilter<TQEMesh, TQEMesh>;
  using TLinear = itk::LinearTriangleCellSubdivisionQuadEdgeMeshFilter<TQEMesh, TQEMesh>;
  using TLoop = itk::LoopTriangleCellSubdivisionQuadEdgeMeshFilter<TQEMesh, TQEMesh>;
  using TSquare = itk::SquareThreeTriangleCellSubdivisionQuadEdgeMeshFilter<TQEMesh, TQEMesh>;

  SubdivisionTestHelper<TButterfly>(true);
  SubdivisionTestHelper<TLinear>(true);
  SubdivisionTestHelper<TLoop>(true);
  SubdivisionTestHelper<TSquare>(true);

  SubdivisionTestHelper<TButterfly>(false);
  SubdivisionTestHelper<TLinear>(false);
  SubdivisionTestHelper<TLoop>(false);
  SubdivisionTestHelper<TSquare>(false);

  return EXIT_SUCCESS;
}
