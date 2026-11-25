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

#include <itkTestingMacros.h>
#include <itkImage.h>
#include <itkConstantPadImageFilter.h>
#include <itkQuadEdgeMesh.h>
#include <itkCuberilleImageToMeshFilter.h>

int
CuberilleTest04(int itkNotUsed(argc), char * itkNotUsed(argv)[])
{

  using TPixel = unsigned char;
  using TCoordinate = double;
  const unsigned int Dimension = 3;
  using TImage = itk::Image<TPixel, Dimension>;
  using TMesh = itk::QuadEdgeMesh<TCoordinate, Dimension>;
  using TPad = itk::ConstantPadImageFilter<TImage, TImage>;
  using TExtract = itk::CuberilleImageToMeshFilter<TImage, TMesh>;

  for (size_t mask = 1; mask < std::pow(2, 8); ++mask)
  {

    std::bitset<8> bitmask(mask);

    std::cout << mask << ' ' << bitmask << std::endl;

    const auto       image = TImage::New();
    TImage::SizeType size;
    size.Fill(2);

    TImage::IndexType origin;
    origin.Fill(0);

    TImage::RegionType region(origin, size);

    image->SetRegions(region);
    image->Allocate();
    image->FillBuffer(0);

    for (size_t index = 0; index < std::pow(2, 3); ++index)
    {
      std::bitset<3> bitindex(index);
      image->SetPixel({ { bitindex[0], bitindex[1], bitindex[2] } }, bitmask[index]);
    }

    typename TImage::SizeType padding;
    padding.Fill(1);

    const auto pad = TPad::New();
    pad->SetInput(image);
    pad->SetPadUpperBound(padding);
    pad->SetPadLowerBound(padding);
    pad->SetConstant(static_cast<TPixel>(0));

    const auto extract = TExtract::New();
    extract->SetInput(pad->GetOutput());
    extract->GenerateTriangleFacesOn();
    extract->ProjectVerticesToIsoSurfaceOff();
    extract->SavePixelAsCellDataOn();
    ITK_TRY_EXPECT_NO_EXCEPTION(extract->Update());

    const auto out = TMesh::New();
    out->Graft(extract->GetOutput());

    ITK_TEST_EXPECT_TRUE(out->GetNumberOfFaces() == out->GetCellData()->Size());

    for (auto it = out->GetEdgeCells()->Begin(); it != out->GetEdgeCells()->End(); ++it)
    {
      using TEdge = TMesh::EdgeCellType;
      const auto qe = dynamic_cast<TEdge *>(it.Value())->GetQEGeom();
      ITK_TEST_EXPECT_TRUE(qe->IsLeftSet());
      ITK_TEST_EXPECT_TRUE(qe->IsRightSet());
    }
  }

  return EXIT_SUCCESS;
}
