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

#include "itkCropImageFilter.h"
#include "itkImageBufferRange.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"

#include <iostream>
#include <numeric> // For iota.

int
itkCropImageFilterTest(int, char *[])
{

  // Define the dimension of the images
  constexpr unsigned int ImageDimension{ 2 };

  // Declare the pixel types of the images
  using PixelType = short;

  // Declare the types of the images
  using ImageType = itk::Image<PixelType, ImageDimension>;

  auto inputImage = ImageType::New();

  // Fill in the image
  constexpr ImageType::SizeType size{ 8, 12 };
  ImageType::RegionType         region{ size };
  inputImage->SetLargestPossibleRegion(region);
  inputImage->SetBufferedRegion(region);
  inputImage->Allocate();

  const itk::ImageBufferRange<ImageType> imageBufferRange(*inputImage);
  std::iota(imageBufferRange.begin(), imageBufferRange.end(), PixelType{});

  // Create the filter
  const itk::CropImageFilter<ImageType, ImageType>::Pointer cropFilter =
    itk::CropImageFilter<ImageType, ImageType>::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(cropFilter, CropImageFilter, ExtractImageFilter);

  const itk::SimpleFilterWatcher watcher(cropFilter);

  cropFilter->SetInput(inputImage);

  ImageType::SizeType extractSize = { { 8, 12 } };
  extractSize[0] = 1;
  extractSize[1] = 1;

  cropFilter->SetBoundaryCropSize(extractSize);

  cropFilter->SetUpperBoundaryCropSize(extractSize);
  ITK_TEST_SET_GET_VALUE(extractSize, cropFilter->GetUpperBoundaryCropSize());

  cropFilter->SetLowerBoundaryCropSize(extractSize);
  ITK_TEST_SET_GET_VALUE(extractSize, cropFilter->GetLowerBoundaryCropSize());

  cropFilter->UpdateLargestPossibleRegion();

  // Test all the region types from a CropImageFilter
  const ImageType::RegionType requestedRegion = cropFilter->GetOutput()->GetRequestedRegion();
  const ImageType::RegionType bufferedRegion = cropFilter->GetOutput()->GetBufferedRegion();
  const ImageType::RegionType largestRegion = cropFilter->GetOutput()->GetLargestPossibleRegion();

  for (auto & currRegion : { requestedRegion, bufferedRegion, largestRegion })
  {
    if (currRegion.GetSize()[0] != 6 || currRegion.GetSize()[1] != 10)
    {
      return EXIT_FAILURE;
    }

    if (currRegion.GetIndex()[0] != 1 || currRegion.GetIndex()[1] != 1)
    {
      return EXIT_FAILURE;
    }
  }

  return EXIT_SUCCESS;
}
