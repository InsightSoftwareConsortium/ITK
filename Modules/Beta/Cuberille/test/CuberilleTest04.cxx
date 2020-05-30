/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
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

// In the case where a 2x2x2 region contains two background pixels at
// opposite corners, and foreground pixels elsewhere, it is necessary
// that there be two vertices at the center.  Previously, this was
// handled incorrectly with a single vertex, causing non-manifold
// geometry.  This test covers the four cases in which this occurs.

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

  // 126 01111110
  // 189 10111101
  // 219 11011011
  // 231 11100111

  std::array<size_t, 4> masks{ 126, 189, 219, 231 };

  for (const auto & mask : masks)
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
    extract->Update();

    ITK_TEST_EXPECT_EQUAL(extract->GetOutput()->GetNumberOfPoints(), 26);
    ITK_TEST_EXPECT_EQUAL(extract->GetOutput()->GetNumberOfCells(), 48);
  }

  return EXIT_SUCCESS;
}
