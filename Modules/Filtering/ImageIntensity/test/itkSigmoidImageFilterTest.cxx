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

#include "itkMath.h"
#include "itkSigmoidImageFilter.h"
#include "itkTestingMacros.h"


int
itkSigmoidImageFilterTest(int, char *[])
{

  // Define the dimension of the images
  constexpr unsigned int ImageDimension = 3;

  // Declare the types of the images
  using InputPixelType = float;
  using OutputPixelType = float;

  using InputImageType = itk::Image<InputPixelType, ImageDimension>;
  using OutputImageType = itk::Image<OutputPixelType, ImageDimension>;

  // Declare appropriate Iterator types for each image
  using InputIteratorType = itk::ImageRegionIteratorWithIndex<InputImageType>;
  using OutputIteratorType = itk::ImageRegionIteratorWithIndex<OutputImageType>;

  // Declare the type of the index to access images
  using IndexType = itk::Index<ImageDimension>;

  // Declare the type of the size
  using SizeType = itk::Size<ImageDimension>;

  // Declare the type of the Region
  using RegionType = itk::ImageRegion<ImageDimension>;

  // Create the input images
  auto inputImage = InputImageType::New();

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

  // Initialize the input image
  inputImage->SetRegions(region);
  inputImage->Allocate();

  // Create one iterator for the input image (this is a light object)
  InputIteratorType it(inputImage, inputImage->GetBufferedRegion());

  // Initialize the content of the input image
  constexpr double value = 30;
  it.GoToBegin();
  while (!it.IsAtEnd())
  {
    it.Set(value);
    ++it;
  }

  // Declare the type for the Sigmoid filter
  using FilterType = itk::SigmoidImageFilter<InputImageType, OutputImageType>;

  // Create the filter
  auto filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, SigmoidImageFilter, UnaryFunctorImageFilter);

  // Set the input image
  filter->SetInput(inputImage);

  // Set the filter parameters
  constexpr double alpha = 2.0;
  constexpr double beta = 3.0;

  filter->SetAlpha(alpha);
  ITK_TEST_SET_GET_VALUE(alpha, filter->GetAlpha());

  filter->SetBeta(beta);
  ITK_TEST_SET_GET_VALUE(beta, filter->GetBeta());

  constexpr OutputPixelType maximum = 1.0;
  const OutputPixelType     minimum = -1.0;

  filter->SetOutputMinimum(minimum);
  ITK_TEST_SET_GET_VALUE(minimum, filter->GetOutputMinimum());

  filter->SetOutputMaximum(maximum);
  ITK_TEST_SET_GET_VALUE(maximum, filter->GetOutputMaximum());

  filter->SetFunctor(filter->GetFunctor());

  // Execute the filter
  filter->Update();

  // Get the filter output
  OutputImageType::Pointer outputImage = filter->GetOutput();

  // Create an iterator for going through the image output
  OutputIteratorType ot(outputImage, outputImage->GetRequestedRegion());

  // Check the content of the result image
  const OutputImageType::PixelType epsilon = 1e-6;
  ot.GoToBegin();
  it.GoToBegin();
  while (!ot.IsAtEnd())
  {
    const InputImageType::PixelType  input = it.Get();
    const OutputImageType::PixelType output = ot.Get();
    const double                     x1 = (input - beta) / alpha;
    const double                     x2 = (maximum - minimum) * (1.0 / (1.0 + std::exp(-x1))) + minimum;
    const auto                       sigmoid = static_cast<OutputImageType::PixelType>(x2);
    if (!itk::Math::FloatAlmostEqual(sigmoid, output, 10, epsilon))
    {
      std::cerr.precision(static_cast<int>(itk::Math::abs(std::log10(epsilon))));
      std::cerr << "Error " << std::endl;
      std::cerr << " simoid( " << input << ") = " << sigmoid << std::endl;
      std::cerr << " differs from " << output;
      std::cerr << " by more than " << epsilon << std::endl;
      return EXIT_FAILURE;
    }
    ++ot;
    ++it;
  }

  return EXIT_SUCCESS;
}
