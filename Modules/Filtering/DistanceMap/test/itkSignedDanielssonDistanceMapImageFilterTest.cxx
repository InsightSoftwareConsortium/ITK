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

#include "itkShowDistanceMap.h"
#include "itkSignedDanielssonDistanceMapImageFilter.h"
#include "itkStdStreamStateSave.h"

void
test(int);

int
itkSignedDanielssonDistanceMapImageFilterTest(int, char *[])
{
  test(1); // Test with a 9x9 square, with a 5x5 subsquare in the middle ON
  test(0); // Test with 2 points.. same test and results as
           // DanielssonDistanceMap
  return EXIT_SUCCESS;
}


void
test(int testIdx)
{

  // Save the format stream variables for std::cout
  // They will be restored when coutState goes out of scope
  // scope.
  itk::StdStreamStateSave coutState(std::cout);

  constexpr unsigned int Dimension = 2;
  using PixelType = float;

  using myImageType2D1 = itk::Image<unsigned char, Dimension>;
  using myImageType2D2 = itk::Image<PixelType, Dimension>;

  /* TEST 1: For a point image, SignedDaniessonDistanceMapImageFilter should
   * give the same output as DaniessonDistanceMapImageFilter  */

  /* Allocate the 2D image */
  myImageType2D1::SizeType size2D;
  size2D.Fill(9);

  myImageType2D1::IndexType index2D;
  index2D.Fill(0);

  myImageType2D1::RegionType region2D;
  region2D.SetSize(size2D);
  region2D.SetIndex(index2D);

  auto inputImage2D = myImageType2D1::New();
  inputImage2D->SetRegions(region2D);
  inputImage2D->Allocate(true);

  if (!testIdx)
  {
    std::cout << "Compute with a 9x9 image, pixels (4,4) and (1,6) set to ON."
              << " This subtest is the same as the DanielssonDistanceMapTest "
              << "and should yield the same results." << std::endl
              << std::endl;
    /* Set pixel (4,4) with the value 1
     * and pixel (1,6) with the value 2
     * The Danielsson Distance is performed for each pixel with a value > 0
     * The ClosestPoints computation is based on the value of the pixel.
     * Test 1 contains two isolated points and should return the same result
     * as if the filter SignedDanielssonDistanceMapImageFilter was changed
     * to DanielssonDistanceMapImageFilter
     */
    index2D[0] = 4;
    index2D[1] = 4;
    inputImage2D->SetPixel(index2D, 1);
    index2D[0] = 1;
    index2D[1] = 6;
    inputImage2D->SetPixel(index2D, 2);
  }
  else
  {
    std::cout << "Compute with a 9x9 image, with a 5x5 square at the center set to ON." << std::endl << std::endl;
    // Test the signed Danielsson Output for the a 5x5 square in a 9x9 image
    int i, j;
    for (i = 2; i <= 6; ++i)
    {
      for (j = 2; j <= 6; ++j)
      {
        index2D[0] = i;
        index2D[1] = j;
        inputImage2D->SetPixel(index2D, 1);
      }
    }
  }

  /* Create Danielsson Distance Map filter */
  using myFilterType2D = itk::SignedDanielssonDistanceMapImageFilter<myImageType2D1, myImageType2D2>;

  auto filter2D = myFilterType2D::New();

  filter2D->SetInput(inputImage2D);
  myImageType2D2::Pointer outputDistance2D = filter2D->GetOutput();

  using VoronoiImageType = myFilterType2D::VoronoiImageType;

  VoronoiImageType::Pointer outputVoronoi2D = filter2D->GetVoronoiMap();

  myFilterType2D::VectorImageType::Pointer outputComponents = filter2D->GetVectorDistanceMap();

  filter2D->Update();

  ShowDistanceMap(outputDistance2D);

  std::cout << std::endl << std::endl;
  std::cout << "Voronoi Map Image 2D" << std::endl << std::endl;
  ShowDistanceMap(outputVoronoi2D);

  /* Show VectorsComponents Points map */
  std::cout << std::endl << std::endl;
  std::cout << "Components Map Image 2D" << std::endl << std::endl;

  itk::ImageSliceConstIteratorWithIndex<myFilterType2D::VectorImageType> it2D4(outputComponents,
                                                                               outputComponents->GetRequestedRegion());

  it2D4.SetFirstDirection(0);
  it2D4.SetSecondDirection(1);

  while (!it2D4.IsAtEnd())
  {
    while (!it2D4.IsAtEndOfSlice())
    {
      while (!it2D4.IsAtEndOfLine())
      {
        std::cout << "[";
        for (unsigned int i = 0; i < 2; ++i)
        {
          std::cout << it2D4.Get()[i];
          if (i == 0)
          {
            std::cout << ",";
          }
        }
        std::cout << "]";
        std::cout << "\t";
        ++it2D4;
      }
      std::cout << std::endl;
      it2D4.NextLine();
    }
    it2D4.NextSlice();
  }

  /* Test Distance functionality */
  filter2D->Update();

  std::cout << "Distance Map " << std::endl;
  ShowDistanceMap(outputDistance2D);
}
