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

#include "itkAddImageAdaptor.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkSubtractImageFilter.h"
#include "itkGTest.h"

TEST(AddImageAdaptor, ConvertedLegacyTest)
{
  // Define the dimension of the images
  constexpr unsigned int Dimension{ 3 };

  // Declare the pixel type
  using PixelType = int;

  // Declare the types of the image
  using ImageType = itk::Image<PixelType, Dimension>;

  // Declare the type of the index to access images
  using IndexType = itk::Index<Dimension>;

  // Declare the type of the size
  using SizeType = itk::Size<Dimension>;

  // Declare the type of the Region
  using RegionType = itk::ImageRegion<Dimension>;

  // Create input image
  auto inputImage = ImageType::New();

  // Define their size and region
  constexpr SizeType size{ 2, 2, 2 };
  RegionType         region{ size };

  // Initialize Image
  inputImage->SetRegions(region);
  inputImage->Allocate();

  // Declare Iterator type appropriate for this image
  using IteratorType = itk::ImageRegionIteratorWithIndex<ImageType>;

  // Create one iterator for Image A.
  IteratorType it1(inputImage, inputImage->GetBufferedRegion());

  // Initialize the content of Image A
  PixelType value = 13;
  while (!it1.IsAtEnd())
  {
    it1.Set(value);
    value += 1;
    ++it1;
  }

  //---------------------------------------
  // This section tests for AddImageAdaptor
  //---------------------------------------

  using AdaptorType = itk::AddImageAdaptor<ImageType>;

  auto addAdaptor = AdaptorType::New();

  constexpr PixelType additiveConstant{ 19 };

  addAdaptor->SetImage(inputImage);
  addAdaptor->SetValue(additiveConstant);

  using DiffFilterType = itk::SubtractImageFilter<AdaptorType, ImageType, ImageType>;

  auto diffFilter = DiffFilterType::New();

  diffFilter->SetInput1(addAdaptor);
  diffFilter->SetInput2(inputImage);

  diffFilter->Update();

  // Get the Smart Pointer to the Diff filter Output
  const ImageType::Pointer diffImage = diffFilter->GetOutput();

  //  Check the content of the diff image
  // Create an iterator for going through the image output
  IteratorType dt(diffImage, diffImage->GetBufferedRegion());

  using RealPixelType = itk::NumericTraits<PixelType>::RealType;

  dt.GoToBegin();

  while (!dt.IsAtEnd())
  {
    auto v1 = static_cast<RealPixelType>(dt.Get());
    auto v2 = static_cast<RealPixelType>(additiveConstant);

    const RealPixelType diff = itk::Math::Absolute(v1 - v2);

    EXPECT_LE(diff, itk::Math::eps);
    ++dt;
  }

  IndexType index;

  index[0] = 1;
  index[1] = 1;
  index[2] = 1;

  // Exercise GetPixel for code coverage; original test printed but did
  // not assert on this read.
  (void)addAdaptor->GetPixel(index);

  constexpr PixelType newValue{ 27 };

  addAdaptor->SetPixel(index, newValue);

  const PixelType p2 = addAdaptor->GetPixel(index);
  EXPECT_EQ(p2, newValue);
}
