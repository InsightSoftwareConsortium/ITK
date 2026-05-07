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

// Regressions for two bugs in the upstream ITKSubdivisionQuadEdgeMeshFilter
// module that the existing venus.vtk-driven tests did not exercise:
//
//   1. LoopTriangleCellSubdivisionQuadEdgeMeshFilter inserted cell IDs into
//      its smoothedPointSet rather than point IDs, so the non-uniform
//      adaptive Loop path silently skipped smoothing for nearly every
//      vertex it should have moved.
//
//   2. TriangleCellSubdivisionQuadEdgeMeshFilter::GenerateOutputCells() hung
//      on any input mesh containing a non-triangle cell — the cell iterator
//      was not advanced before the guard-branch `continue`. The same bug
//      also lived in SquareThreeTriangleCellSubdivision's override, but
//      that override's AddNewCellPoints throws on non-triangle input first,
//      so the hang was only reachable in the parent class via Loop's
//      non-uniform path (Loop does not override GenerateOutputCells).
//
// The existing venus.vtk regression tests cannot trigger either bug because
// venus.vtk is purely triangular AND the existing tests exercise only the
// uniform-subdivide-all path.

#include "itkLoopTriangleCellSubdivisionQuadEdgeMeshFilter.h"
#include "itkQuadEdgeMesh.h"
#include "itkQuadEdgeMeshPolygonCell.h"

#include <iostream>


namespace
{

using MeshType = itk::QuadEdgeMesh<double, 3>;
using PolygonCellType = itk::QuadEdgeMeshPolygonCell<MeshType::CellType>;


// Build a mesh with two triangles sharing edge 1-2:
//
//   Points: 0 (0,0,0), 1 (1,0,0), 2 (0,1,0), 3 (1,1,0)
//   Cells:  triangle (0,1,2), triangle (1,3,2)
MeshType::Pointer
BuildTwoTriangleMesh()
{
  const auto mesh = MeshType::New();

  MeshType::PointType p;
  p[2] = 0.0;
  p[0] = 0.0;
  p[1] = 0.0;
  mesh->SetPoint(0, p);
  p[0] = 1.0;
  p[1] = 0.0;
  mesh->SetPoint(1, p);
  p[0] = 0.0;
  p[1] = 1.0;
  mesh->SetPoint(2, p);
  p[0] = 1.0;
  p[1] = 1.0;
  mesh->SetPoint(3, p);

  mesh->AddFaceTriangle(0, 1, 2);
  mesh->AddFaceTriangle(1, 3, 2);
  return mesh;
}


// Inject a non-triangle (4-point) polygon cell into the cells container at
// an unused ID, bypassing AddFace's geometric validation. This produces
// exactly the shape the bug requires: GenerateOutputCells() iterates the
// cells container and must skip the non-triangle without spinning.
void
InjectFourPointPolygonCell(const MeshType::Pointer & mesh, MeshType::CellIdentifier id)
{
  auto * poly = new PolygonCellType(4);
  poly->SetPointId(0, 0);
  poly->SetPointId(1, 1);
  poly->SetPointId(2, 3);
  poly->SetPointId(3, 2);

  MeshType::CellAutoPointer ap;
  ap.TakeOwnership(poly);
  mesh->GetCells()->InsertElement(id, ap.ReleaseOwnership());
}


// Returns true if `ipt` and `opt` differ in any coordinate.
bool
PointMoved(const MeshType::PointType & ipt, const MeshType::PointType & opt)
{
  for (unsigned int k = 0; k < MeshType::PointDimension; ++k)
  {
    if (ipt[k] != opt[k])
    {
      return true;
    }
  }
  return false;
}

} // namespace


int
itkSubdivisionQuadEdgeMeshFilterRegressionTest(int, char *[])
{
  bool ok = true;

  // ---- Bug 2: GenerateOutputCells must terminate on a mesh with a
  // non-triangle cell. Use Loop in non-uniform mode and list only the two
  // triangle cell IDs in CellsToBeSubdivided so AddNewCellPoints is not
  // called on the non-triangle (which would throw). GenerateOutputCells
  // still iterates ALL cells, which is exactly the bug path. The CTest
  // TIMEOUT of 30 seconds catches a regression that re-introduces the hang.
  {
    const auto mesh = BuildTwoTriangleMesh();
    InjectFourPointPolygonCell(mesh, 100);

    using FilterType = itk::LoopTriangleCellSubdivisionQuadEdgeMeshFilter<MeshType, MeshType>;
    const auto filter = FilterType::New();

    typename FilterType::SubdivisionCellContainer cellsToSubdivide;
    cellsToSubdivide.push_back(0);
    cellsToSubdivide.push_back(1);
    filter->SetCellsToBeSubdivided(cellsToSubdivide);
    filter->SetInput(mesh);
    filter->Update();

    std::cout << "Loop terminated on mixed-cell input." << std::endl;
  }

  // ---- Bug 1: Loop non-uniform adaptive path must smooth ALL points of the
  // listed cells, not just those whose point-index coincidentally matches a
  // cell-index. With CellsToBeSubdivided = {0}, cell 0 has point IDs
  // {0, 1, 2}; the smoothing set must therefore contain {0, 1, 2} and the
  // output positions of points 0, 1, and 2 must all move. Before the fix,
  // smoothedPointSet contained {0} (the cell ID), so only point 0 ever moved.
  {
    const auto mesh = BuildTwoTriangleMesh();

    using FilterType = itk::LoopTriangleCellSubdivisionQuadEdgeMeshFilter<MeshType, MeshType>;
    const auto                                    filter = FilterType::New();
    typename FilterType::SubdivisionCellContainer cellsToSubdivide;
    cellsToSubdivide.push_back(0);
    filter->SetCellsToBeSubdivided(cellsToSubdivide);
    filter->SetInput(mesh);
    filter->Update();

    const auto out = filter->GetOutput();

    unsigned int nMoved = 0;
    for (MeshType::PointIdentifier pid = 0; pid < 4; ++pid)
    {
      MeshType::PointType ipt;
      MeshType::PointType opt;
      mesh->GetPoint(pid, &ipt);
      out->GetPoint(pid, &opt);
      if (PointMoved(ipt, opt))
      {
        ++nMoved;
      }
    }
    std::cout << "Loop non-uniform: " << nMoved << " of 4 points moved." << std::endl;
    if (nMoved < 3)
    {
      std::cerr << "FAIL: expected at least 3 of {0,1,2} to be smoothed (cell 0's vertices); "
                << "got " << nMoved << ". This is the smoothedPointSet cell-id-vs-point-id "
                << "regression." << std::endl;
      ok = false;
    }
  }

  return ok ? EXIT_SUCCESS : EXIT_FAILURE;
}
