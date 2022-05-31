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
#include "itkTriangleHelper.h"

const unsigned int Dimension = 3;
using TPixel = unsigned char;
using TCoordinate = double;
using TImage = itk::Image<TPixel, Dimension>;
using TMesh = itk::Mesh<TCoordinate, Dimension>;
using TImageToMesh = itk::CuberilleImageToMeshFilter<TImage, TMesh>;
using TInterp = itk::NearestNeighborInterpolateImageFunction<TImage>;
using TTriangleHelper = itk::TriangleHelper<typename TMesh::PointType>;

TImage::Pointer
CuberilleTest03CreateImage()
{
  const auto image = TImage::New();

  TImage::IndexType start;
  start.Fill(0);

  TImage::SizeType size;
  size[0] = 3;
  size[1] = 5;
  size[2] = 3;

  TImage::RegionType region;
  region.SetSize(size);
  region.SetIndex(start);

  image->SetRegions(region);
  image->Allocate();
  image->FillBuffer(0);

  TImage::IndexType index;
  index.Fill(1);
  image->SetPixel(index, 1);
  index[1] = 2;
  image->SetPixel(index, 2);
  index[1] = 3;
  image->SetPixel(index, 3);

  return image;
}

void
CuberilleTest03Helper(TImage::Pointer image)
{

  const auto image_to_mesh = TImageToMesh::New();
  image_to_mesh->SetInput(image);
  image_to_mesh->SavePixelAsCellDataOn();
  image_to_mesh->GenerateTriangleFacesOff();
  image_to_mesh->ProjectVerticesToIsoSurfaceOff();
  image_to_mesh->Update();

  const auto mesh = TMesh::New();
  mesh->Graft(image_to_mesh->GetOutput());
  mesh->DisconnectPipeline();

  const auto interp = TInterp::New();
  interp->SetInputImage(image);

  const auto half_spacing = image->GetSpacing() * 0.5;

  for (auto it = mesh->GetCells()->Begin(); it != mesh->GetCells()->End(); ++it)
  {
    const auto cell = it.Value();

    typename TImage::PointType centroid;
    centroid.SetToMidPoint(mesh->GetPoint(cell->GetPointIds()[0]), mesh->GetPoint(cell->GetPointIds()[2]));

    auto normal = TTriangleHelper::ComputeNormal(mesh->GetPoint(cell->GetPointIds()[0]),
                                                 mesh->GetPoint(cell->GetPointIds()[1]),
                                                 mesh->GetPoint(cell->GetPointIds()[2]));

    normal.Normalize();

    for (size_t i = 0; i < 3; ++i)
    {
      normal[i] *= half_spacing[i];
    }

    const auto resample = centroid + -1.0f * normal;
    const auto label = interp->Evaluate(resample);

    const auto data = mesh->GetCellData()->ElementAt(it.Index());

    if (label != data)
    {
      std::cerr << "Calculated Pixel (" << label << ") != Cell Data (" << data << ").\n";
      throw 0;
    }
  }
}

int
CuberilleTest03Parameters(const bool triangles, const bool project)
{

  const auto image = CuberilleTest03CreateImage();
  const auto image_to_mesh = TImageToMesh::New();
  image_to_mesh->SetInput(image);
  image_to_mesh->SavePixelAsCellDataOn();
  image_to_mesh->SetGenerateTriangleFaces(triangles);
  image_to_mesh->SetProjectVerticesToIsoSurface(project);
  ITK_TRY_EXPECT_NO_EXCEPTION(image_to_mesh->Update());
  ITK_TEST_EXPECT_EQUAL(image_to_mesh->GetOutput()->GetCells()->Size(),
                        image_to_mesh->GetOutput()->GetCellData()->Size());

  return EXIT_SUCCESS;
}

// - Create a test image, 3x3x5, with zeros along the edges and [1 2 3]
// in the interior.
// - Extract a Cuberille mesh calling SavePixelAsCellDataOn().
// - For each cell in the mesh, calculate the centroid and normal.
// - Walk one-half pixel width from the centroid along the normal into the mesh.
// - Sample that point in the input segmentation image.
// - Assert that that cell data is equal to the adjacent pixel value.
int
CuberilleTest03(int itkNotUsed(argc), char * itkNotUsed(argv)[])
{

  { // Test Set/Get Methods
    const auto image_to_mesh = TImageToMesh::New();
    bool       save = true;
    ITK_TEST_SET_GET_BOOLEAN(image_to_mesh, SavePixelAsCellData, save);
  }

  if (EXIT_FAILURE == CuberilleTest03Parameters(false, false))
  {
    throw 0;
  }
  if (EXIT_FAILURE == CuberilleTest03Parameters(false, true))
  {
    throw 0;
  }
  if (EXIT_FAILURE == CuberilleTest03Parameters(true, false))
  {
    throw 0;
  }
  if (EXIT_FAILURE == CuberilleTest03Parameters(true, true))
  {
    throw 0;
  }

  const auto image = CuberilleTest03CreateImage();
  CuberilleTest03Helper(image);

  return EXIT_SUCCESS;
}
