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

#include "itkExpImageFilter.h"
#include "itkExpImageAdaptor.h"
#include "itkSubtractImageFilter.h"


int
itkExpImageFilterAndAdaptorTest(int, char *[])
{

  // Define the dimension of the images
  constexpr unsigned int ImageDimension = 3;

  // Declare the types of the images
  using InputImageType = itk::Image<float, ImageDimension>;
  using OutputImageType = itk::Image<float, ImageDimension>;

  // Declare Iterator types apropriated for each image
  using InputIteratorType = itk::ImageRegionIteratorWithIndex<InputImageType>;
  using OutputIteratorType = itk::ImageRegionIteratorWithIndex<OutputImageType>;

  // Declare the type of the index to access images
  using IndexType = itk::Index<ImageDimension>;

  // Declare the type of the size
  using SizeType = itk::Size<ImageDimension>;

  // Declare the type of the Region
  using RegionType = itk::ImageRegion<ImageDimension>;

  // Create two images
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

  // Initialize Image A
  inputImage->SetRegions(region);
  inputImage->Allocate();
  // Create one iterator for the Input Image (this is a light object)
  InputIteratorType it(inputImage, inputImage->GetBufferedRegion());

  // Initialize the content of Image A
  const double value = itk::Math::pi / 6.0;
  std::cout << "Content of the Input " << '\n';
  it.GoToBegin();
  while (!it.IsAtEnd())
  {
    it.Set(value);
    std::cout << it.Get() << '\n';
    ++it;
  }

  // Declare the type for the Exp filter
  using FilterType = itk::ExpImageFilter<InputImageType, OutputImageType>;


  // Create an ADD Filter
  auto filter = FilterType::New();


  // Connect the input images
  filter->SetInput(inputImage);
  filter->SetNumberOfWorkUnits(1);

  // Get the Smart Pointer to the Filter Output
  const OutputImageType::Pointer outputImage = filter->GetOutput();


  // Execute the filter
  filter->Update();

  // Create an iterator for going through the image output
  OutputIteratorType ot(outputImage, outputImage->GetRequestedRegion());

  //  Check the content of the result image
  std::cout << "Verification of the output " << '\n';
  const OutputImageType::PixelType epsilon = 1e-6;
  ot.GoToBegin();
  it.GoToBegin();
  while (!ot.IsAtEnd())
  {
    std::cout << ot.Get() << " = ";
    std::cout << std::exp(it.Get()) << '\n';
    const InputImageType::PixelType  input = it.Get();
    const OutputImageType::PixelType output = ot.Get();
    const OutputImageType::PixelType exponential = std::exp(input);
    if (itk::Math::abs(exponential - output) > epsilon)
    {
      std::cerr << "Error in itkExpImageFilterTest " << '\n';
      std::cerr << " std::exp( " << input << ") = " << exponential << '\n';
      std::cerr << " differs from " << output;
      std::cerr << " by more than " << epsilon << '\n';
      return EXIT_FAILURE;
    }
    ++ot;
    ++it;
  }


  //---------------------------------------
  // This section tests for ExpImageAdaptor
  //---------------------------------------

  using AdaptorType = itk::ExpImageAdaptor<InputImageType, OutputImageType::PixelType>;

  auto expAdaptor = AdaptorType::New();

  expAdaptor->SetImage(inputImage);

  using DiffFilterType = itk::SubtractImageFilter<OutputImageType, AdaptorType, OutputImageType>;

  auto diffFilter = DiffFilterType::New();

  diffFilter->SetInput1(outputImage);
  diffFilter->SetInput2(expAdaptor);

  diffFilter->Update();

  // Get the Smart Pointer to the Diff filter Output
  const OutputImageType::Pointer diffImage = diffFilter->GetOutput();

  //  Check the content of the diff image
  std::cout << "Comparing the results with those of an Adaptor" << '\n';
  std::cout << "Verification of the output " << '\n';

  // Create an iterator for going through the image output
  OutputIteratorType dt(diffImage, diffImage->GetRequestedRegion());

  dt.GoToBegin();
  while (!dt.IsAtEnd())
  {
    std::cout << dt.Get() << '\n';
    const OutputImageType::PixelType diff = dt.Get();
    if (itk::Math::abs(diff) > epsilon)
    {
      std::cerr << "Error in itkExpImageFilterTest " << '\n';
      std::cerr << "Comparing results with Adaptors" << '\n';
      std::cerr << " difference = " << diff << '\n';
      std::cerr << " differs from 0 ";
      std::cerr << " by more than " << epsilon << '\n';
      return EXIT_FAILURE;
    }
    ++dt;
  }


  return EXIT_SUCCESS;
}
