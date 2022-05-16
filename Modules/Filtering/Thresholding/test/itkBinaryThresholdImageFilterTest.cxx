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

#include "itkRandomImageSource.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkNumericTraits.h"
#include "itkMath.h"
#include "itkTestingMacros.h"


int
itkBinaryThresholdImageFilterTest(int, char *[])
{
  // Define the dimension of the images
  constexpr unsigned int Dimension = 3;

  using InputPixelType = unsigned char;
  using OutputPixelType = float;

  // Declare the types of the images
  using InputImageType = itk::Image<InputPixelType, Dimension>;
  using OutputImageType = itk::Image<OutputPixelType, Dimension>;

  // Declare iterator type
  using InputIteratorType = itk::ImageRegionIteratorWithIndex<InputImageType>;

  using OutputIteratorType = itk::ImageRegionIteratorWithIndex<OutputImageType>;

  // Use a random image source as input
  using SourceType = itk::RandomImageSource<InputImageType>;
  auto source = SourceType::New();

  InputImageType::SizeValueType sizeArray[Dimension] = { 3, 3, 3 };

  source->SetMin(itk::NumericTraits<InputPixelType>::ZeroValue());
  source->SetMax(itk::NumericTraits<InputPixelType>::max());
  source->SetSize(sizeArray);

  // Declare the type for the binary threshold filter
  using FilterType = itk::BinaryThresholdImageFilter<InputImageType, OutputImageType>;

  auto filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, BinaryThresholdImageFilter, UnaryFunctorImageFilter);

  // Set up ivars
  InputPixelType lower = 64;
  InputPixelType upper = 128;


  // No input set: check that thresholds are created using the input
  // pixel type non-positive min value
  //

  // Lower threshold
  FilterType::InputPixelObjectType::Pointer lowerThreshold1 = filter->GetLowerThresholdInput();

  if (lowerThreshold1->Get() != itk::NumericTraits<InputPixelType>::NonpositiveMin())
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in GetLowerThresholdInput():" << std::endl;
    std::cerr << "Expected: "
              << static_cast<itk::NumericTraits<OutputPixelType>::PrintType>(
                   itk::NumericTraits<InputPixelType>::NonpositiveMin())
              << ", but got: " << static_cast<itk::NumericTraits<OutputPixelType>::PrintType>(lowerThreshold1->Get())
              << std::endl;
    return EXIT_FAILURE;
  }

  FilterType::InputPixelObjectType::Pointer lowerThreshold2 = FilterType::InputPixelObjectType::New();
  filter->SetLowerThresholdInput(lowerThreshold2);

  if (lowerThreshold2->Get() != itk::NumericTraits<InputPixelType>::NonpositiveMin())
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in GetLowerThresholdInput():" << std::endl;
    std::cerr << "Expected: "
              << static_cast<itk::NumericTraits<OutputPixelType>::PrintType>(
                   itk::NumericTraits<InputPixelType>::NonpositiveMin())
              << ", but got: " << static_cast<itk::NumericTraits<OutputPixelType>::PrintType>(lowerThreshold2->Get())
              << std::endl;
    return EXIT_FAILURE;
  }

  // Upper threshold
  FilterType::InputPixelObjectType::Pointer upperThreshold1 = filter->GetUpperThresholdInput();

  if (lowerThreshold1->Get() != itk::NumericTraits<InputPixelType>::NonpositiveMin())
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in GetUpperThresholdInput():" << std::endl;
    std::cerr << "Expected: "
              << static_cast<itk::NumericTraits<OutputPixelType>::PrintType>(
                   itk::NumericTraits<InputPixelType>::NonpositiveMin())
              << ", but got: " << static_cast<itk::NumericTraits<OutputPixelType>::PrintType>(upperThreshold1->Get())
              << std::endl;
    return EXIT_FAILURE;
  }

  FilterType::InputPixelObjectType::Pointer upperThreshold2 = FilterType::InputPixelObjectType::New();
  filter->SetUpperThresholdInput(upperThreshold2);

  if (upperThreshold2->Get() != itk::NumericTraits<InputPixelType>::NonpositiveMin())
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in GetLowerThresholdInput():" << std::endl;
    std::cerr << "Expected: "
              << static_cast<itk::NumericTraits<OutputPixelType>::PrintType>(
                   itk::NumericTraits<InputPixelType>::NonpositiveMin())
              << ", but got: " << static_cast<itk::NumericTraits<OutputPixelType>::PrintType>(upperThreshold2->Get())
              << std::endl;
    return EXIT_FAILURE;
  }

  // Exercise the const variants
  FilterType::ConstPointer constFilter = (const FilterType *)(filter.GetPointer());

  const typename FilterType::InputPixelObjectType * lowerThresholdInput = constFilter->GetLowerThresholdInput();
  ITK_TEST_SET_GET_VALUE(lowerThresholdInput->Get(), lowerThreshold2->Get());

  const typename FilterType::InputPixelObjectType * upperThresholdInput = constFilter->GetUpperThresholdInput();
  ITK_TEST_SET_GET_VALUE(upperThresholdInput->Get(), upperThreshold2->Get());


  // Deliberately cause an exception by setting lower threshold to be
  // greater than the upper threshold
  filter->SetLowerThreshold(upper);
  filter->SetUpperThreshold(lower);

  ITK_TRY_EXPECT_EXCEPTION(filter->Update());


  filter->SetLowerThreshold(lower);
  ITK_TEST_SET_GET_VALUE(lower, filter->GetLowerThreshold());

  filter->SetUpperThreshold(upper);
  ITK_TEST_SET_GET_VALUE(upper, filter->GetUpperThreshold());

  OutputPixelType inside = -0.5;
  filter->SetInsideValue(inside);
  ITK_TEST_SET_GET_VALUE(inside, filter->GetInsideValue());

  OutputPixelType outside = 0.5;
  filter->SetOutsideValue(outside);
  ITK_TEST_SET_GET_VALUE(outside, filter->GetOutsideValue());


  filter->SetFunctor(filter->GetFunctor());

  filter->SetInput(source->GetOutput());


  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());

  // Get the filter output
  OutputImageType::Pointer outputImage = filter->GetOutput();

  // Create an iterator for going through the image output
  InputIteratorType  it(source->GetOutput(), source->GetOutput()->GetRequestedRegion());
  OutputIteratorType ot(outputImage, outputImage->GetRequestedRegion());

  // Check the content of the result image
  ot.GoToBegin();
  it.GoToBegin();
  while (!ot.IsAtEnd())
  {
    const InputPixelType  input = it.Get();
    const OutputPixelType output = ot.Get();

    if (lower <= input && input <= upper)
    {
      if (itk::Math::NotExactlyEquals(output, inside))
      {
        std::cerr << "Test failed!" << std::endl;
        std::cerr << "Error checking the inside value:" << std::endl;
        std::cout << "Lower threshold: " << static_cast<itk::NumericTraits<InputPixelType>::PrintType>(lower)
                  << ", upper threshold: " << static_cast<itk::NumericTraits<InputPixelType>::PrintType>(upper)
                  << std::endl;
        std::cerr << "Expected: " << static_cast<itk::NumericTraits<OutputPixelType>::PrintType>(inside)
                  << ", but got: " << static_cast<itk::NumericTraits<OutputPixelType>::PrintType>(output) << std::endl;
        return EXIT_FAILURE;
      }
    }
    else if (itk::Math::NotExactlyEquals(output, outside))
    {
      std::cerr << "Test failed!" << std::endl;
      std::cerr << "Error checking the outside value:" << std::endl;
      std::cout << "Lower threshold: " << static_cast<itk::NumericTraits<InputPixelType>::PrintType>(lower)
                << ", upper threshold: " << static_cast<itk::NumericTraits<InputPixelType>::PrintType>(upper)
                << std::endl;
      std::cerr << "Expected: " << static_cast<itk::NumericTraits<OutputPixelType>::PrintType>(outside)
                << ", but got: " << static_cast<itk::NumericTraits<OutputPixelType>::PrintType>(output) << std::endl;
      return EXIT_FAILURE;
    }
    ++ot;
    ++it;
  }

  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
