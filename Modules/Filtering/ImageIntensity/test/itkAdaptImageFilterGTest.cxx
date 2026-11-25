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

/**
 *
 *  This program illustrates the AdaptImageFilter
 *
 *  The example shows how an Accessor can be used to
 *  convert an RGBPixel image to an image that has
 *  just the red component.
 *
 *  That will allow to pass the red component of this
 *  image as input any filter that expects
 *  a float image
 *
 */


#include "itkAdaptImageFilter.h"
#include "itkRedPixelAccessor.h"
#include "itkGreenPixelAccessor.h"
#include "itkBluePixelAccessor.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkSimpleFilterWatcher.h"
#include "itkMath.h"
#include "itkGTest.h"

#include <algorithm> // For generate.
#include <random>    // For mt19937.


//-------------------------------------
//     Typedefs for convenience
//-------------------------------------
namespace
{
using myRGBImageType = itk::Image<itk::RGBPixel<float>, 2>;
using myRGBIteratorType = itk::ImageRegionIteratorWithIndex<myRGBImageType>;

using myRedAccessorType = itk::RedPixelAccessor<float>;
using myGreenAccessorType = itk::GreenPixelAccessor<float>;
using myBlueAccessorType = itk::BluePixelAccessor<float>;

using myImageType = itk::Image<float, 2>;
using myIteratorType = itk::ImageRegionIteratorWithIndex<myImageType>;
} // namespace

TEST(AdaptImageFilter, ConvertedLegacyTest)
{
  auto size = myRGBImageType::SizeType::Filled(2);

  myRGBImageType::IndexType index;
  index[0] = 0;
  index[1] = 0;

  const myRGBImageType::RegionType region{ index, size };

  auto myImage = myRGBImageType::New();


  myImage->SetRegions(region);
  myImage->Allocate();

  myRGBIteratorType it1(myImage, myImage->GetRequestedRegion());

  // Value to initialize the pixels
  myRGBImageType::PixelType color;

  std::mt19937                          randomNumberEngine{};
  std::uniform_real_distribution<float> randomNumberDistribution{};

  // Initializing all the pixel in the image
  it1.GoToBegin();
  while (!it1.IsAtEnd())
  {
    std::generate(color.begin(), color.end(), [&randomNumberEngine, &randomNumberDistribution] {
      return randomNumberDistribution(randomNumberEngine);
    });
    it1.Set(color);
    ++it1;
  }

  // Convert to a red image
  const itk::AdaptImageFilter<myRGBImageType, myImageType, myRedAccessorType>::Pointer adaptImageToRed =
    itk::AdaptImageFilter<myRGBImageType, myImageType, myRedAccessorType>::New();
  const itk::SimpleFilterWatcher redWatcher(adaptImageToRed, "Red");
  adaptImageToRed->SetInput(myImage);
  adaptImageToRed->UpdateLargestPossibleRegion();

  myIteratorType it(adaptImageToRed->GetOutput(), adaptImageToRed->GetOutput()->GetRequestedRegion());

  it.GoToBegin();
  it1.GoToBegin();
  while (!it.IsAtEnd())
  {
    EXPECT_EQ(it.Get(), it1.Get().GetRed());
    ++it;
    ++it1;
  }

  // Convert to a green image
  const itk::AdaptImageFilter<myRGBImageType, myImageType, myGreenAccessorType>::Pointer adaptImageToGreen =
    itk::AdaptImageFilter<myRGBImageType, myImageType, myGreenAccessorType>::New();
  const itk::SimpleFilterWatcher greenWatcher(adaptImageToGreen, "Green");

  adaptImageToGreen->SetInput(myImage);
  adaptImageToGreen->UpdateLargestPossibleRegion();

  it = myIteratorType(adaptImageToGreen->GetOutput(), adaptImageToGreen->GetOutput()->GetRequestedRegion());

  it.GoToBegin();
  it1.GoToBegin();
  while (!it.IsAtEnd())
  {
    EXPECT_EQ(it.Get(), it1.Get().GetGreen());
    ++it;
    ++it1;
  }

  // Convert to a blue image
  const itk::AdaptImageFilter<myRGBImageType, myImageType, myBlueAccessorType>::Pointer adaptImageToBlue =
    itk::AdaptImageFilter<myRGBImageType, myImageType, myBlueAccessorType>::New();
  const itk::SimpleFilterWatcher blueWatcher(adaptImageToBlue, "Blue");

  adaptImageToBlue->SetInput(myImage);
  adaptImageToBlue->UpdateLargestPossibleRegion();

  it = myIteratorType(adaptImageToBlue->GetOutput(), adaptImageToBlue->GetOutput()->GetRequestedRegion());

  it.GoToBegin();
  it1.GoToBegin();
  while (!it.IsAtEnd())
  {
    EXPECT_EQ(it.Get(), it1.Get().GetBlue());
    ++it;
    ++it1;
  }
}
