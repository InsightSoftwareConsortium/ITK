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

int
itkVoronoiDiagram2DInfiniteLoopTest(int argc, char * argv[])
{
  if (argc != 1)
  {
    std::cerr << "Takes no parameters, but " << argc << "parameters given" << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << std::endl;
    return EXIT_FAILURE;
  }

  // From https://github.com/InsightSoftwareConsortium/ITK/issues/4386
  using VoronoiDiagramType = itk::VoronoiDiagram2D<double>;
  using VoronoiGeneratorType = itk::VoronoiDiagram2DGenerator<double>;
  using PointType = VoronoiDiagramType::PointType;
  VoronoiGeneratorType::Pointer vg = VoronoiGeneratorType::New();
  vg->SetOrigin(PointType{ { -1.61569, -1.76726 } });
  vg->SetBoundary(PointType{ { 1.60174, 1.76345 } });
  vg->AddOneSeed(PointType{ { -1.39649, 0.322212 } });
  vg->AddOneSeed(PointType{ { -1.30128, 0.231786 } });
  vg->AddOneSeed(PointType{ { -1.21509, 0.0515039 } });
  vg->AddOneSeed(PointType{ { -1.22364, -0.030281 } });
  vg->AddOneSeed(PointType{ { -1.22125, -0.120815 } });
  vg->AddOneSeed(PointType{ { -1.25159, -0.23593 } });
  vg->Update();

  return EXIT_SUCCESS;
}
