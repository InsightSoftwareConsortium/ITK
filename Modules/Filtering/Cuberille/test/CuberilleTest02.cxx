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

// STD
#include <array>

// ITK
#include "itkTestingMacros.h"
#include "itkImage.h"
#include "itkMesh.h"
#include "itkCuberilleImageToMeshFilter.h"
#include "itkNearestNeighborInterpolateImageFunction.h"

const unsigned int Dimension = 3;
using TPixel = unsigned char;
using TCoordinate = double;
using TImage = itk::Image<TPixel, Dimension>;
using TMesh = itk::Mesh<TCoordinate, Dimension>;
using TImageToMesh = itk::CuberilleImageToMeshFilter<TImage, TMesh>;
using TInterp = itk::NearestNeighborInterpolateImageFunction<TImage>;

TImage::Pointer
CuberilleTestCreateImage(const std::array<bool, 3> & flips)
{
  const auto image = TImage::New();

  TImage::IndexType start;
  start.Fill(0);

  TImage::SizeType size;
  size.Fill(3);

  TImage::RegionType region;
  region.SetSize(size);
  region.SetIndex(start);

  image->SetRegions(region);
  image->Allocate();
  image->FillBuffer(0);

  TImage::IndexType index;
  index.Fill(1);
  image->SetPixel(index, 1);

  TImage::DirectionType direction;
  direction.SetIdentity();

  for (size_t i = 0; i < flips.size(); ++i)
  {
    if (flips[i])
    {
      direction(i, i) *= -1;
    }
  }

  image->SetDirection(direction);

  return image;
}

void
CuberilleTestHelper(TImage::Pointer input)
{

  const auto image_to_mesh = TImageToMesh::New();
  image_to_mesh->SetInput(input);
  image_to_mesh->ProjectVerticesToIsoSurfaceOff();
  image_to_mesh->Update();

  const auto mesh = TMesh::New();
  mesh->Graft(image_to_mesh->GetOutput());
  mesh->DisconnectPipeline();

  TMesh::PointType center;
  center.Fill(0.0);

  for (auto it = mesh->GetPoints()->Begin(); it != mesh->GetPoints()->End(); ++it)
  {
    center[0] += it.Value()[0];
    center[1] += it.Value()[1];
    center[2] += it.Value()[2];
  }

  center[0] /= 8.0;
  center[1] /= 8.0;
  center[2] /= 8.0;

  const auto interp = TInterp::New();
  interp->SetInputImage(input);

  if (1 != interp->Evaluate(center))
  {
    throw 0;
  }
}

int
CuberilleTest02(int itkNotUsed(argc), char * itkNotUsed(argv)[])
{

  CuberilleTestHelper(CuberilleTestCreateImage({ { false, false, false } }));
  CuberilleTestHelper(CuberilleTestCreateImage({ { false, false, true } }));
  CuberilleTestHelper(CuberilleTestCreateImage({ { false, true, false } }));
  CuberilleTestHelper(CuberilleTestCreateImage({ { false, true, true } }));
  CuberilleTestHelper(CuberilleTestCreateImage({ { true, false, false } }));
  CuberilleTestHelper(CuberilleTestCreateImage({ { true, false, true } }));
  CuberilleTestHelper(CuberilleTestCreateImage({ { true, true, false } }));
  CuberilleTestHelper(CuberilleTestCreateImage({ { true, true, true } }));

  return EXIT_SUCCESS;
}
