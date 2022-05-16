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
#include "itkImageFileWriter.h"
#include "itkResampleImageFilter.h"
#include "itkTimeProbe.h"
#include "itkTestingMacros.h"

int
itkResampleImageTest5(int argc, char * argv[])
{

  if (argc < 2)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cout << " scaling outputFilename" << std::endl;
    return EXIT_FAILURE;
  }

  // Resample an RGB image
  constexpr unsigned int VDimension = 2;

  using RGBPixelType = itk::RGBPixel<unsigned char>;
  using ImageType = itk::Image<RGBPixelType, 2>;

  using ImageIndexType = ImageType::IndexType;
  using ImagePointerType = ImageType::Pointer;
  using ImageRegionType = ImageType::RegionType;
  using ImageSizeType = ImageType::SizeType;

  using CoordRepType = double;

  using AffineTransformType = itk::AffineTransform<CoordRepType, VDimension>;
  using InterpolatorType = itk::LinearInterpolateImageFunction<ImageType, CoordRepType>;
  using WriterType = itk::ImageFileWriter<ImageType>;

  float scaling = std::stod(argv[1]);

  // Create and configure an image
  ImagePointerType image = ImageType::New();
  ImageIndexType   index = { { 0, 0 } };
  ImageSizeType    size = { { 64, 64 } };
  ImageRegionType  region;
  region.SetSize(size);
  region.SetIndex(index);
  image->SetLargestPossibleRegion(region);
  image->SetBufferedRegion(region);
  image->Allocate();

  auto          newDims = static_cast<unsigned int>(64 * scaling);
  ImageSizeType osize = { { newDims, newDims } };

  ImageType::SpacingType spacing;
  spacing[0] = size[0] / static_cast<double>(osize[0]);
  spacing[1] = size[1] / static_cast<double>(osize[1]);

  // Fill image with a ramp
  itk::ImageRegionIteratorWithIndex<ImageType> iter(image, region);
  for (iter.GoToBegin(); !iter.IsAtEnd(); ++iter)
  {
    index = iter.GetIndex();
    const auto rgbPixel = itk::MakeFilled<RGBPixelType>(index[0] + index[1]);
    iter.Set(rgbPixel);
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
  resample->Update();
  clock.Stop();

  std::cout << "Resampling from " << size << " to " << osize << " took " << clock.GetMean() << " s" << std::endl;

  auto writer = WriterType::New();
  writer->SetInput(resample->GetOutput());
  writer->SetFileName(argv[2]);
  writer->Update();

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
