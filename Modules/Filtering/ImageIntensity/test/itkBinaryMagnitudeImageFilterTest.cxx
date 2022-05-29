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

#include "itkBinaryMagnitudeImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkMath.h"
#include "itkTestingMacros.h"


int
itkBinaryMagnitudeImageFilterTest(int, char *[])
{

  // Define the dimension of the images
  constexpr unsigned int Dimension = 3;

  // Declare the pixel types of the images
  using PixelType = float;

  // Declare the types of the images
  using InputImageType1 = itk::Image<PixelType, Dimension>;
  using InputImageType2 = itk::Image<PixelType, Dimension>;
  using OutputImageType = itk::Image<PixelType, Dimension>;

  // Declare the type of the index to access images
  using IndexType = itk::Index<Dimension>;

  // Declare the type of the size
  using SizeType = itk::Size<Dimension>;

  // Declare the type of the Region
  using RegionType = itk::ImageRegion<Dimension>;

  // Create the input images
  auto inputImageA = InputImageType1::New();
  auto inputImageB = InputImageType2::New();

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
  inputImageA->SetRegions(region);
  inputImageA->Allocate();

  // Initialize Image B
  inputImageB->SetRegions(region);
  inputImageB->Allocate();

  // Declare appropriate Iterator types for each image
  using InputImage1IteratorType = itk::ImageRegionIteratorWithIndex<InputImageType1>;
  using InputImage2IteratorType = itk::ImageRegionIteratorWithIndex<InputImageType2>;
  using OutputImageIteratorType = itk::ImageRegionIteratorWithIndex<OutputImageType>;

  // Create one iterator for Image A (this is a light object)
  InputImage1IteratorType it1(inputImageA, inputImageA->GetBufferedRegion());

  // Initialize the content of Image A
  constexpr InputImageType1::PixelType input1Value = 3.0;
  while (!it1.IsAtEnd())
  {
    it1.Set(input1Value);
    ++it1;
  }

  // Create one iterator for Image B (this is a light object)
  InputImage2IteratorType it2(inputImageB, inputImageB->GetBufferedRegion());

  // Initialize the content of Image B
  constexpr InputImageType2::PixelType input2Value = 4.0;
  while (!it2.IsAtEnd())
  {
    it2.Set(input2Value);
    ++it2;
  }

  // Define the values of the output image
  constexpr OutputImageType::PixelType outputValue = 5.0;


  // Declare the type for the BinaryMagnitudeImageFilter
  using FilterType = itk::BinaryMagnitudeImageFilter<InputImageType1, InputImageType2, OutputImageType>;

  // Create the BinaryMagnitudeImageFilter
  auto filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, BinaryMagnitudeImageFilter, BinaryGeneratorImageFilter);

  // Set the input images
  filter->SetInput1(inputImageA);
  filter->SetInput2(inputImageB);


  // Execute the filter
  filter->Update();

  // Get the filter output
  OutputImageType::Pointer outputImage = filter->GetOutput();

  // Create an iterator for going through the image output
  OutputImageIteratorType oIt(outputImage, outputImage->GetBufferedRegion());

  // Check the content of the result image
  const float epsilon = 1e-6;
  while (!oIt.IsAtEnd())
  {
    if (!itk::Math::FloatAlmostEqual(oIt.Get(), outputValue, 10, epsilon))
    {
      std::cerr.precision(static_cast<int>(itk::Math::abs(std::log10(epsilon))));
      std::cerr << "Error in the output" << std::endl;
      std::cerr << "Value should be  " << outputValue << std::endl;
      std::cerr << "but is           " << oIt.Get() << std::endl;
      return EXIT_FAILURE;
    }
    ++oIt;
  }

  // All objects should be automatically destroyed at this point
  return EXIT_SUCCESS;
}
