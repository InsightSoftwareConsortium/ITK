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
#include "itkGTest.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkNumericTraits.h"


TEST(BinaryThresholdImageFilter, ConvertedLegacyTest)
{
  // Define the dimension of the images
  constexpr unsigned int Dimension{ 3 };

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

  source->SetMin(InputPixelType{});
  source->SetMax(itk::NumericTraits<InputPixelType>::max());
  source->SetSize(sizeArray);

  // Declare the type for the binary threshold filter
  using FilterType = itk::BinaryThresholdImageFilter<InputImageType, OutputImageType>;

  auto filter = FilterType::New();

  ITK_GTEST_EXERCISE_BASIC_OBJECT_METHODS(filter, BinaryThresholdImageFilter, UnaryFunctorImageFilter);

  // Set up ivars
  constexpr InputPixelType lower{ 64 };
  constexpr InputPixelType upper{ 128 };


  // No input set: check that thresholds are created using the input
  // pixel type non-positive min value
  //

  // Lower threshold
  const FilterType::InputPixelObjectType::Pointer lowerThreshold1 = filter->GetLowerThresholdInput();

  EXPECT_EQ(lowerThreshold1->Get(), itk::NumericTraits<InputPixelType>::NonpositiveMin())
    << "GetLowerThresholdInput() default";

  const FilterType::InputPixelObjectType::Pointer lowerThreshold2 = FilterType::InputPixelObjectType::New();
  filter->SetLowerThresholdInput(lowerThreshold2);

  EXPECT_EQ(lowerThreshold2->Get(), itk::NumericTraits<InputPixelType>::NonpositiveMin()) << "GetLowerThresholdInput()";

  // Upper threshold
  const FilterType::InputPixelObjectType::Pointer upperThreshold1 = filter->GetUpperThresholdInput();

  EXPECT_EQ(upperThreshold1->Get(), itk::NumericTraits<InputPixelType>::max()) << "GetUpperThresholdInput() default";

  const FilterType::InputPixelObjectType::Pointer upperThreshold2 = FilterType::InputPixelObjectType::New();
  filter->SetUpperThresholdInput(upperThreshold2);

  EXPECT_EQ(upperThreshold2->Get(), itk::NumericTraits<InputPixelType>::NonpositiveMin()) << "GetUpperThresholdInput()";

  // Exercise the const variants
  const FilterType::ConstPointer constFilter = (const FilterType *)(filter.GetPointer());

  const FilterType::InputPixelObjectType * lowerThresholdInput = constFilter->GetLowerThresholdInput();
  EXPECT_EQ(lowerThresholdInput->Get(), lowerThreshold2->Get());

  const FilterType::InputPixelObjectType * upperThresholdInput = constFilter->GetUpperThresholdInput();
  EXPECT_EQ(upperThresholdInput->Get(), upperThreshold2->Get());


  // Deliberately cause an exception by setting lower threshold to be
  // greater than the upper threshold
  filter->SetLowerThreshold(upper);
  filter->SetUpperThreshold(lower);

  EXPECT_THROW(filter->Update(), itk::ExceptionObject);


  filter->SetLowerThreshold(lower);
  EXPECT_EQ(lower, filter->GetLowerThreshold());

  filter->SetUpperThreshold(upper);
  EXPECT_EQ(upper, filter->GetUpperThreshold());

  constexpr OutputPixelType inside{ -0.5 };
  filter->SetInsideValue(inside);
  EXPECT_EQ(inside, filter->GetInsideValue());

  constexpr OutputPixelType outside{ 0.5 };
  filter->SetOutsideValue(outside);
  EXPECT_EQ(outside, filter->GetOutsideValue());


  filter->SetFunctor(filter->GetFunctor());

  filter->SetInput(source->GetOutput());


  ASSERT_NO_THROW(filter->Update());

  // Get the filter output
  const OutputImageType::Pointer outputImage = filter->GetOutput();

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
      EXPECT_EQ(output, inside) << "Error checking the inside value:" << std::endl
                                << "Lower threshold: "
                                << static_cast<itk::NumericTraits<InputPixelType>::PrintType>(lower)
                                << ", upper threshold: "
                                << static_cast<itk::NumericTraits<InputPixelType>::PrintType>(upper);
    }
    else
    {
      EXPECT_EQ(output, outside) << "Error checking the outside value:" << std::endl
                                 << "Lower threshold: "
                                 << static_cast<itk::NumericTraits<InputPixelType>::PrintType>(lower)
                                 << ", upper threshold: "
                                 << static_cast<itk::NumericTraits<InputPixelType>::PrintType>(upper);
    }
    ++ot;
    ++it;
  }
}
