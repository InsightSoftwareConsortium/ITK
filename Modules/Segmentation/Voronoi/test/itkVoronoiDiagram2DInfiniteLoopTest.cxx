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

#include "itkVoronoiDiagram2DGenerator.h"
#include "itkTestingMacros.h"

// Regression test for ITK issue #4386: near-collinear seed configurations
// caused an infinite loop in VoronoiDiagram2DGenerator::ConstructDiagram().
//
// Root cause: Fortune's algorithm produces near-zero-length edges when
// boundary intersection points coincide within floating-point tolerance.
// These degenerate edges have different vertex IDs but geometrically
// identical endpoints (within DIFF_TOLERENCE = 0.001).  They cannot be
// attached to the growing boundary chain because:
//   1. Their vertex IDs don't match any chain endpoint.
//   2. The chain may already be closed (front == back).
//   3. Boundary-bridging logic doesn't apply when chain endpoints are
//      interior (not on the domain boundary).
// The fix explicitly detects and drops these degenerate edges using the
// existing differentPoint() tolerance check.
int
itkVoronoiDiagram2DInfiniteLoopTest(int argc, char * argv[])
{
  if (argc != 1)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << std::endl;
    return EXIT_FAILURE;
  }

  using VoronoiDiagramType = itk::VoronoiDiagram2D<double>;
  using VoronoiGeneratorType = itk::VoronoiDiagram2DGenerator<double>;
  using PointType = VoronoiDiagramType::PointType;

  // Six near-collinear seeds (x in [-1.40, -1.21]) that produce a
  // degenerate Voronoi edge on the left domain boundary where two
  // intersection points are ~0.00003 apart.
  auto vg = VoronoiGeneratorType::New();
  vg->SetOrigin(PointType{ { -1.61569, -1.76726 } });
  vg->SetBoundary(PointType{ { 1.60174, 1.76345 } });
  vg->AddOneSeed(PointType{ { -1.39649, 0.322212 } });
  vg->AddOneSeed(PointType{ { -1.30128, 0.231786 } });
  vg->AddOneSeed(PointType{ { -1.21509, 0.0515039 } });
  vg->AddOneSeed(PointType{ { -1.22364, -0.030281 } });
  vg->AddOneSeed(PointType{ { -1.22125, -0.120815 } });
  vg->AddOneSeed(PointType{ { -1.25159, -0.23593 } });

  // Without the fix, this call loops forever.  With the fix, the
  // degenerate near-zero-length edge is detected and dropped, and
  // the exception guard ensures no non-degenerate edges are lost.
  ITK_TRY_EXPECT_NO_EXCEPTION(vg->Update());

  // Verify all 6 cells were constructed with valid boundaries
  auto vd = vg->GetOutput();
  for (unsigned int i = 0; i < 6; ++i)
  {
    VoronoiDiagramType::CellAutoPointer cellPtr;
    vd->GetCellId(i, cellPtr);
    ITK_TEST_EXPECT_TRUE(cellPtr->GetNumberOfPoints() >= 2);
  }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
