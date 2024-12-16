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
#include "itkStreamingImageFilter.h"
#include "itkTestingMacros.h"
#include "itkMath.h"

/* itkResampleImageFilter output compared to streamed output
 */

int
itkResampleImageTest7(int, char *[])
{

  constexpr unsigned int VDimension = 2;

  using PixelType = float;

  using ImageType = itk::Image<PixelType, VDimension>;
  using ImageIndexType = ImageType::IndexType;
  using ImagePointerType = ImageType::Pointer;
  using ImageRegionType = ImageType::RegionType;
  using ImageSizeType = ImageType::SizeType;

  using CoordinateType = double;

  using AffineTransformType = itk::AffineTransform<CoordinateType, VDimension>;

  using InterpolatorType = itk::LinearInterpolateImageFunction<ImageType, CoordinateType>;

  // Create and configure an image
  const ImagePointerType  image = ImageType::New();
  ImageIndexType          index = { { 0, 0 } };
  constexpr ImageSizeType size = { { 64, 64 } };
  const ImageRegionType   region{ index, size };
  image->SetRegions(region);
  image->Allocate();

  // Fill image with a ramp
  itk::ImageRegionIteratorWithIndex<ImageType> iter(image, region);
  for (iter.GoToBegin(); !iter.IsAtEnd(); ++iter)
  {
    index = iter.GetIndex();
    const PixelType value = index[0] + index[1];
    iter.Set(value);
  }

  // Create an affine transformation
  auto aff = AffineTransformType::New();
  aff->Scale(0.9);

  // Create a linear interpolation image function
  auto interp = InterpolatorType::New();
  interp->SetInputImage(image);

  // Create and configure a resampling filter
  const itk::ResampleImageFilter<ImageType, ImageType>::Pointer resample =
    itk::ResampleImageFilter<ImageType, ImageType>::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(resample, ResampleImageFilter, ImageToImageFilter);
  resample->SetInterpolator(interp);

  resample->SetInput(image);
  ITK_TEST_SET_GET_VALUE(image.GetPointer(), resample->GetInput());

  resample->SetSize(size);
  ITK_TEST_SET_GET_VALUE(size, resample->GetSize());

  resample->SetTransform(aff);
  ITK_TEST_SET_GET_VALUE(aff, resample->GetTransform());

  resample->SetInterpolator(interp);
  ITK_TEST_SET_GET_VALUE(interp, resample->GetInterpolator());

  index.Fill(0);
  resample->SetOutputStartIndex(index);
  ITK_TEST_SET_GET_VALUE(index, resample->GetOutputStartIndex());

  using StreamerType = itk::StreamingImageFilter<ImageType, ImageType>;
  auto streamer = StreamerType::New();

  std::cout << "Test with normal AffineTransform." << std::endl;
  streamer->SetInput(resample->GetOutput());

  // Run the resampling filter without streaming, i.e. 1 StreamDivisions
  unsigned char numStreamDiv = 1; // do not split, i.e. do not stream
  streamer->SetNumberOfStreamDivisions(numStreamDiv);
  ITK_TRY_EXPECT_NO_EXCEPTION(streamer->UpdateLargestPossibleRegion());

  const ImagePointerType outputNoSDI = streamer->GetOutput(); // save output for later comparison
  outputNoSDI->DisconnectPipeline();                          // disconnect to create new output

  // Run the resampling filter with streaming
  image->Modified();
  numStreamDiv = 8; // split into numStream pieces for streaming.
  streamer->SetNumberOfStreamDivisions(numStreamDiv);
  ITK_TRY_EXPECT_NO_EXCEPTION(streamer->UpdateLargestPossibleRegion());

  // Verify that we only requested a smaller chunk when streaming
  const ImageRegionType finalRequestedRegion(image->GetRequestedRegion());
  ITK_TEST_SET_GET_VALUE(0, finalRequestedRegion.GetIndex(0));
  ITK_TEST_SET_GET_VALUE(48, finalRequestedRegion.GetIndex(1));
  ITK_TEST_SET_GET_VALUE(60, finalRequestedRegion.GetSize(0));
  ITK_TEST_SET_GET_VALUE(12, finalRequestedRegion.GetSize(1));

  const ImagePointerType outputSDI = streamer->GetOutput();
  outputSDI->DisconnectPipeline();

  itk::ImageRegionIterator<ImageType> itNoSDI(outputNoSDI, outputNoSDI->GetLargestPossibleRegion());
  itk::ImageRegionIterator<ImageType> itSDI(outputSDI, outputSDI->GetLargestPossibleRegion());
  for (itNoSDI.GoToBegin(), itSDI.GoToBegin(); !itNoSDI.IsAtEnd() && !itSDI.IsAtEnd(); ++itNoSDI, ++itSDI)
  {
    if (itk::Math::NotAlmostEquals(itNoSDI.Value(), itSDI.Value()))
    {
      std::cout << "Pixels differ " << itNoSDI.Value() << ' ' << itSDI.Value() << std::endl;
      std::cerr << "Test failed!" << std::endl;
      std::cerr << "Error in pixel value at index [" << itNoSDI.GetIndex() << ']' << std::endl;
      std::cerr << "Expected difference " << itNoSDI.Get() - itSDI.Get() << std::endl;
      std::cerr << " differs from 0 ";
      return EXIT_FAILURE;
    }
  }
  if (itNoSDI.IsAtEnd() != itSDI.IsAtEnd())
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Iterators don't agree on end of image" << std::endl;
    std::cerr << "at index [" << itNoSDI.GetIndex() << ']' << std::endl;
    return EXIT_FAILURE;
  }

  // Test non default values
  constexpr auto origin = itk::MakeFilled<ImageType::PointType>(1234.0);
  resample->SetOutputOrigin(origin);
  ITK_TEST_SET_GET_VALUE(origin, resample->GetOutputOrigin());

  auto spacing = itk::MakeFilled<ImageType::SpacingType>(9876.0);
  resample->SetOutputSpacing(spacing);
  ITK_TEST_SET_GET_VALUE(spacing, resample->GetOutputSpacing());


  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
