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

#include "itkVectorResampleImageFilter.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"

int
itkVectorResampleImageFilterTest(int argc, char * argv[])
{

  if (argc < 2)
  {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " outputImage" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;
  using PixelComponentType = unsigned char;

  using PixelType = itk::RGBPixel<PixelComponentType>;
  using ImageType = itk::Image<PixelType, Dimension>;

  using FilterType = itk::VectorResampleImageFilter<ImageType, ImageType>;

  FilterType::Pointer filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, VectorResampleImageFilter, ImageToImageFilter);

  itk::SimpleFilterWatcher watcher(filter);

  using InterpolatorType = itk::VectorLinearInterpolateImageFunction<ImageType, double>;

  InterpolatorType::Pointer interpolator = InterpolatorType::New();

  filter->SetInterpolator(interpolator);
  ITK_TEST_SET_GET_VALUE(interpolator, filter->GetInterpolator());

  using TransformType = itk::IdentityTransform<double, Dimension>;
  TransformType::Pointer transform = TransformType::New();

  filter->SetTransform(transform);
  ITK_TEST_SET_GET_VALUE(transform, filter->GetTransform());

  ImageType::SpacingType spacing;
  spacing.Fill(1.0);

  ImageType::PointType origin;
  origin.Fill(0.0);

  ImageType::RegionType region;
  ImageType::SizeType   size;
  ImageType::IndexType  start;

  size[0] = 128;
  size[1] = 128;

  start[0] = 0;
  start[1] = 0;

  region.SetSize(size);
  region.SetIndex(start);

  ImageType::Pointer image = ImageType::New();

  image->SetOrigin(origin);
  image->SetSpacing(spacing);
  image->SetRegions(region);
  image->Allocate();

  PixelType pixelValue;

  itk::ImageRegionIteratorWithIndex<ImageType> it(image, region);

  // Fill the image with some color pattern
  it.GoToBegin();
  while (!it.IsAtEnd())
  {
    ImageType::IndexType index = it.GetIndex();
    pixelValue.SetRed(index[0] * 2);
    pixelValue.SetGreen(index[0] + index[1]);
    pixelValue.SetBlue(index[1] * 2);
    it.Set(pixelValue);
    ++it;
  }

  PixelType blackValue;
  blackValue.Fill(0);

  filter->SetDefaultPixelValue(blackValue);
  ITK_TEST_SET_GET_VALUE(blackValue, filter->GetDefaultPixelValue());

  // Set the spacing for the resampling
  spacing[0] *= 2.0;
  spacing[1] *= 2.0;

  filter->SetOutputSpacing(spacing);
  ITK_TEST_SET_GET_VALUE(spacing, filter->GetOutputSpacing());

  // Keep the input image origin
  filter->SetOutputOrigin(origin);
  ITK_TEST_SET_GET_VALUE(origin, filter->GetOutputOrigin());

  // Set the size
  size[0] /= 2;
  size[1] /= 2;

  filter->SetSize(size);
  ITK_TEST_SET_GET_VALUE(size, filter->GetSize());

  // Set the output direction
  FilterType::DirectionType outputDirection = image->GetDirection();

  filter->SetOutputDirection(outputDirection);
  ITK_TEST_SET_GET_VALUE(outputDirection, filter->GetOutputDirection());

  // Set the start index
  FilterType::IndexType outputStartIndex = image->GetLargestPossibleRegion().GetIndex();

  filter->SetOutputStartIndex(outputStartIndex);
  ITK_TEST_SET_GET_VALUE(outputStartIndex, filter->GetOutputStartIndex());


  filter->SetInput(image);

  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());

  // Write an image for regression testing
  using WriterType = itk::ImageFileWriter<ImageType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(filter->GetOutput());
  writer->SetFileName(argv[1]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  return EXIT_SUCCESS;
}
