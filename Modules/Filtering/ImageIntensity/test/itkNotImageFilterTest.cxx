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

#include "itkNotImageFilter.h"
#include "itkTestingMacros.h"


int
itkNotImageFilterTest(int, char *[])
{

  // Define the dimension of the images
  constexpr unsigned int Dimension = 3;

  // Declare the types of the images
  using PixelType = unsigned char;

  // Declare the types of the images
  using InputImageType = itk::Image<PixelType, Dimension>;
  using OutputImageType = itk::Image<PixelType, Dimension>;

  // Declare the type of the index to access images
  using IndexType = itk::Index<Dimension>;

  // Declare the type of the size
  using SizeType = itk::Size<Dimension>;

  // Declare the type of the Region
  using RegionType = itk::ImageRegion<Dimension>;

  // Declare the type for the filter
  using NotImageFilterType = itk::NotImageFilter<InputImageType, OutputImageType>;

  // Create the input image
  InputImageType::Pointer inputImage = InputImageType::New();

  // Define their size, and start index
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

  // Initialize input image
  inputImage->SetLargestPossibleRegion(region);
  inputImage->SetBufferedRegion(region);
  inputImage->SetRequestedRegion(region);
  inputImage->Allocate();

  // Declare appropriate Iterator types for each image
  using InputIteratorType = itk::ImageRegionIteratorWithIndex<InputImageType>;
  using OutputIteratorType = itk::ImageRegionIteratorWithIndex<OutputImageType>;

  // Create one iterator for Image A (this is a light object)
  InputIteratorType it(inputImage, inputImage->GetBufferedRegion());
  it.GoToBegin();

  // Initialize the content of Image A
  while (!it.IsAtEnd())
  {
    it.Set(true);
    ++it;
  }

  // Create the filter
  NotImageFilterType::Pointer filter = NotImageFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, NotImageFilter, UnaryFunctorImageFilter);

  filter->SetForegroundValue(2);
  ITK_TEST_SET_GET_VALUE(2, filter->GetForegroundValue());
  filter->SetForegroundValue(1);

  filter->SetBackgroundValue(10);
  ITK_TEST_SET_GET_VALUE(10, filter->GetBackgroundValue());
  filter->SetBackgroundValue(0);

  // Set the input images
  filter->SetInput(inputImage);

  filter->SetFunctor(filter->GetFunctor());

  // Execute the filter
  filter->Update();

  // Get the filter output
  OutputImageType::Pointer outputImage = filter->GetOutput();

  // Create an iterator for going through the image output
  OutputIteratorType ot(outputImage, outputImage->GetBufferedRegion());
  ot.GoToBegin();

  // Check the content of the result image
  ot.GoToBegin();
  it.GoToBegin();
  while (!ot.IsAtEnd())
  {
    const InputImageType::PixelType  input = it.Get();
    const OutputImageType::PixelType output = ot.Get();
    ITK_TEST_EXPECT_EQUAL(input, !output);
    ++ot;
    ++it;
  }


  // All objects should be automatically destroyed at this point
  return EXIT_SUCCESS;
}
