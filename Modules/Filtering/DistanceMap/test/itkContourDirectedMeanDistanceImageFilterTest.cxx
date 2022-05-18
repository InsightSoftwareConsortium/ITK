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

#include "itkContourDirectedMeanDistanceImageFilter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"

int
itkContourDirectedMeanDistanceImageFilterTest(int, char *[])
{

  using Pixel1Type = unsigned int;
  using Pixel2Type = float;

  constexpr unsigned int ImageDimension = 3;

  using Image1Type = itk::Image<Pixel1Type, ImageDimension>;
  using Image2Type = itk::Image<Pixel2Type, ImageDimension>;

  auto image1 = Image1Type::New();
  auto image2 = Image2Type::New();

  Image1Type::SizeType size;
  size.Fill(50);

  image1->SetRegions(size);
  image2->SetRegions(size);

  image1->Allocate();
  image2->Allocate();

  image1->FillBuffer(itk::NumericTraits<Pixel1Type>::ZeroValue());
  image2->FillBuffer(itk::NumericTraits<Pixel2Type>::ZeroValue());

  using RegionType = Image1Type::RegionType;
  RegionType region1;
  RegionType region2;

  using IndexType = Image1Type::IndexType;
  IndexType index;

  size.Fill(20);
  index.Fill(10);
  region1.SetSize(size);
  region1.SetIndex(index);

  size.Fill(15);
  index.Fill(20);
  region2.SetSize(size);
  region2.SetIndex(index);

  itk::ImageRegionIterator<Image1Type> it1(image1, region1);
  Pixel1Type                           count = itk::NumericTraits<Pixel1Type>::ZeroValue();
  while (!it1.IsAtEnd())
  {
    it1.Set(++count);
    ++it1;
  }

  itk::ImageRegionIterator<Image2Type> it2(image2, region2);
  while (!it2.IsAtEnd())
  {
    it2.Set(7.2);
    ++it2;
  }

  auto useImageSpacing = true;

  // Compute the directed Mean distance h(image1,image2)
  {
    using FilterType = itk::ContourDirectedMeanDistanceImageFilter<Image1Type, Image2Type>;
    auto filter = FilterType::New();

    ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, ContourDirectedMeanDistanceImageFilter, ImageToImageFilter);

    itk::SimpleFilterWatcher watcher(filter, "filter");


    ITK_TEST_SET_GET_BOOLEAN(filter, UseImageSpacing, useImageSpacing);

    filter->SetInput1(image1);
    filter->SetInput2(image2);
    filter->Update();

    // Check results

    FilterType::RealType trueDistance = 8.37831;
    FilterType::RealType distance = filter->GetContourDirectedMeanDistance();

    std::cout << " True     distance: " << trueDistance << std::endl;
    std::cout << " Computed distance: " << distance << std::endl;

    if (itk::Math::abs(trueDistance - distance) > 0.1)
    {
      std::cout << "Test failed. " << std::endl;
      return EXIT_FAILURE;
    }
  }

  // Compute the directed Mean distance h(image2,image1)
  {
    using FilterType = itk::ContourDirectedMeanDistanceImageFilter<Image2Type, Image1Type>;
    auto filter = FilterType::New();

    ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, ContourDirectedMeanDistanceImageFilter, ImageToImageFilter);


    ITK_TEST_SET_GET_BOOLEAN(filter, UseImageSpacing, useImageSpacing);

    filter->SetInput1(image2);
    filter->SetInput2(image1);
    filter->Update();


    // Check results
    FilterType::RealType trueDistance = 4.2053;
    FilterType::RealType distance = filter->GetContourDirectedMeanDistance();

    std::cout << " True     distance: " << trueDistance << std::endl;
    std::cout << " Computed distance: " << distance << std::endl;

    if (itk::Math::abs(trueDistance - distance) > 0.1)
    {
      std::cout << "Test failed. " << std::endl;
      return EXIT_FAILURE;
    }
  }

  std::cout << "Test passed. " << std::endl;
  return EXIT_SUCCESS;
}
