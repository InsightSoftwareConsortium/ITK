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

#include <iostream>

#include "itkAffineTransform.h"
#include "itkResampleImageFilter.h"
#include "itkTimeProbe.h"
#include "itkTestingMacros.h"

int
itkResampleImageTest4(int argc, char * argv[])
{

  constexpr unsigned int VDimension = 2;

  using PixelType = float;

  using ImageType = itk::Image<PixelType, VDimension>;
  using ImageIndexType = ImageType::IndexType;
  using ImagePointerType = ImageType::Pointer;
  using ImageRegionType = ImageType::RegionType;
  using ImageSizeType = ImageType::SizeType;

  using CoordRepType = double;

  using AffineTransformType = itk::AffineTransform<CoordRepType, VDimension>;

  using InterpolatorType = itk::LinearInterpolateImageFunction<ImageType, CoordRepType>;


  float scaling = 10.0;
  if (argc > 1)
  {
    scaling = std::stod(argv[1]);
  }

  // Create and configure an image
  ImagePointerType image = ImageType::New();
  ImageIndexType   index = { { 0, 0 } };
  ImageSizeType    size = { { 64, 64 } };
  ImageRegionType  region;
  region.SetSize(size);
  region.SetIndex(index);
  image->SetRegions(region);
  image->Allocate();

  auto          newDims = static_cast<unsigned int>(64 * scaling);
  ImageSizeType osize = { { newDims, newDims } };

  ImageType::SpacingType spacing;
  spacing[0] = size[0] / static_cast<double>(osize[0]);
  spacing[1] = size[1] / static_cast<double>(osize[1]);

  // Fill image with a ramp
  itk::ImageRegionIteratorWithIndex<ImageType> iter(image, region);
  PixelType                                    value;
  for (iter.GoToBegin(); !iter.IsAtEnd(); ++iter)
  {
    index = iter.GetIndex();
    value = index[0] + index[1];
    iter.Set(value);
  }

  // Create an affine transformation
  auto aff = AffineTransformType::New();
  aff->Scale(0.9);

  // Create a linear interpolation image function
  auto interp = InterpolatorType::New();
  interp->SetInputImage(image);

  // Create and configure a resampling filter
  itk::ResampleImageFilter<ImageType, ImageType>::Pointer resample =
    itk::ResampleImageFilter<ImageType, ImageType>::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(resample, ResampleImageFilter, ImageToImageFilter);

  resample->SetInterpolator(interp);

  resample->SetInput(image);
  ITK_TEST_SET_GET_VALUE(image, resample->GetInput());

  resample->SetSize(osize);
  ITK_TEST_SET_GET_VALUE(osize, resample->GetSize());

  resample->SetTransform(aff);
  ITK_TEST_SET_GET_VALUE(aff, resample->GetTransform());

  resample->SetInterpolator(interp);
  ITK_TEST_SET_GET_VALUE(interp, resample->GetInterpolator());

  index.Fill(0);
  resample->SetOutputStartIndex(index);
  ITK_TEST_SET_GET_VALUE(index, resample->GetOutputStartIndex());

  ImageType::PointType origin;
  origin.Fill(0.0);
  resample->SetOutputOrigin(origin);
  ITK_TEST_SET_GET_VALUE(origin, resample->GetOutputOrigin());

  resample->SetOutputSpacing(spacing);
  ITK_TEST_SET_GET_VALUE(spacing, resample->GetOutputSpacing());

  // Run the resampling filter
  itk::TimeProbe clock;
  clock.Start();
  std::cout << "Input: " << image << std::endl;
  resample->UpdateLargestPossibleRegion();
  clock.Stop();

  std::cout << "Resampling from " << size << " to " << osize << " took " << clock.GetMean() << " s" << std::endl;

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
