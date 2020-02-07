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

#include "itkImageFileWriter.h"
#include "itkGridForwardWarpImageFilter.h"
#include "itkNumericTraits.h"
#include "itkTestingMacros.h"


int
itkGridForwardWarpImageFilterTest(int argc, char * argv[])
{

  if (argc != 2)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << argv[0] << " outputImage" << std::endl;
    return EXIT_FAILURE;
  }

  // Define the dimension of the images
  constexpr unsigned int ImageDimension = 3;

  using DeformationPixelType = itk::Vector<double, ImageDimension>;
  using OutputPixelType = unsigned char;

  // Declare the types of the images
  using DisplacementFieldType = itk::Image<DeformationPixelType, ImageDimension>;
  using OutputImageType = itk::Image<OutputPixelType, ImageDimension>;

  // Declare iterator types apropriated for each image
  using DeformationIteratorType = itk::ImageRegionIteratorWithIndex<DisplacementFieldType>;


  // Declare the type of the index to access images
  using IndexType = itk::Index<ImageDimension>;

  // Declare the type of the size
  using SizeType = itk::Size<ImageDimension>;

  // Declare the type of the region
  using RegionType = itk::ImageRegion<ImageDimension>;

  // Create an input image
  DisplacementFieldType ::Pointer inputDisplacementField = DisplacementFieldType ::New();

  // Define its size, and start index
  SizeType size;
  size[0] = 2;
  size[1] = 2;
  size[2] = 2;

  IndexType start;
  start[0] = 0;
  start[1] = 0;
  start[2] = 0;

  RegionType region;
  region.SetIndex(start);
  region.SetSize(size);

  // Initialize the input image
  inputDisplacementField->SetLargestPossibleRegion(region);
  inputDisplacementField->SetBufferedRegion(region);
  inputDisplacementField->SetRequestedRegion(region);
  inputDisplacementField->Allocate();

  // Create one iterator for the input image (this is a light object)
  DeformationIteratorType it(inputDisplacementField, inputDisplacementField->GetBufferedRegion());

  // Initialize the content of the input image
  DeformationPixelType vectorValue;
  vectorValue.Fill(5.0); // FIXME: replace with something more interesting...

  it.GoToBegin();
  while (!it.IsAtEnd())
  {
    it.Set(vectorValue);
    ++it;
  }

  // Declare the type for the GridForwardWarpImageFilter filter
  using FilterType = itk::GridForwardWarpImageFilter<DisplacementFieldType, OutputImageType>;


  // Create the filter instance
  FilterType::Pointer filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, GridForwardWarpImageFilter, ImageToImageFilter);

  FilterType::PixelType backgroundValue = itk::NumericTraits<FilterType::PixelType>::ZeroValue();
  filter->SetBackgroundValue(backgroundValue);
  ITK_TEST_SET_GET_VALUE(backgroundValue, filter->GetBackgroundValue());

  FilterType::PixelType foregroundValue = itk::NumericTraits<FilterType::PixelType>::OneValue();
  filter->SetForegroundValue(foregroundValue);
  ITK_TEST_SET_GET_VALUE(foregroundValue, filter->GetForegroundValue());


  // Set the input image
  filter->SetInput(inputDisplacementField);

  // Execute the filter
  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());


  // Write the result image
  using WriterType = itk::ImageFileWriter<OutputImageType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(filter->GetOutput());
  writer->SetFileName(argv[1]);


  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
