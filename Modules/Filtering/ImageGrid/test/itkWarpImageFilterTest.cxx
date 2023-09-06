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

#include "itkMath.h"
#include "itkWarpImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkStreamingImageFilter.h"
#include "itkVectorImage.h"
#include "itkMath.h"
#include "itkTestingMacros.h"

// class to produce a linear image pattern
template <int VDimension>
class ImagePattern
{
public:
  using IndexType = itk::Index<VDimension>;
  using IndexValueType = typename IndexType::IndexValueType;
  using SizeType = itk::Size<VDimension>;
  using PixelType = float;

  ImagePattern()
  {
    m_Offset = 0.0;
    for (unsigned int j = 0; j < VDimension; ++j)
    {
      m_Coeff[j] = 0.0;
    }
  }

  double
  Evaluate(const IndexType & index, const SizeType & size, const SizeType & clampSize, const PixelType & padValue)
  {
    double accum = m_Offset;
    for (unsigned int j = 0; j < VDimension; ++j)
    {
      if (index[j] < static_cast<IndexValueType>(size[j]))
      {
        if (index[j] >= static_cast<IndexValueType>(clampSize[j]))
        {
          // Interpolators behave this way in half-pixel band at image perimeter
          accum += m_Coeff[j] * static_cast<double>(clampSize[j] - 1);
        }
        else
        {
          accum += m_Coeff[j] * static_cast<double>(index[j]);
        }
      }
      else
      {
        accum = padValue;
        break;
      }
    }

    return accum;
  }

  double m_Coeff[VDimension];
  double m_Offset;
};

// The following three classes are used to support callbacks
// on the filter in the pipeline that follows later
class ShowProgressObject
{
public:
  ShowProgressObject(itk::ProcessObject * o) { m_Process = o; }
  void
  ShowProgress()
  {
    std::cout << "Progress " << m_Process->GetProgress() << std::endl;
  }
  itk::ProcessObject::Pointer m_Process;
};

int
itkWarpImageFilterTest(int, char *[])
{
  constexpr unsigned int ImageDimension = 2;

  using PixelType = float;
  using ImageType = itk::Image<PixelType, ImageDimension>;
  using VectorImageType = itk::VectorImage<PixelType, ImageDimension>;
  using VectorType = itk::Vector<float, ImageDimension>;
  using FieldType = itk::Image<VectorType, ImageDimension>;

  int testPassed = EXIT_SUCCESS;

  std::cout << "Create the input image pattern." << std::endl;
  ImageType::RegionType region;
  ImageType::SizeType   size = { { 64, 64 } };
  region.SetSize(size);

  auto input = ImageType::New();
  input->SetLargestPossibleRegion(region);
  input->SetBufferedRegion(region);
  input->Allocate();

  ImageType::PixelType padValue = 4.0;

  ImagePattern<ImageDimension> pattern;

  pattern.m_Offset = 64;
  for (unsigned int j = 0; j < ImageDimension; ++j)
  {
    pattern.m_Coeff[j] = 1.0;
  }

  using Iterator = itk::ImageRegionIteratorWithIndex<ImageType>;

  for (Iterator inIter(input, region); !inIter.IsAtEnd(); ++inIter)
  {
    inIter.Set(pattern.Evaluate(inIter.GetIndex(), size, size, padValue));
  }

  std::cout << "Create the input displacement field." << std::endl;

  // Tested with { 2, 4 } and { 2, 5 } as well...
  unsigned int factors[ImageDimension] = { 2, 3 };

  ImageType::RegionType fieldRegion;
  ImageType::SizeType   fieldSize;
  for (unsigned int j = 0; j < ImageDimension; ++j)
  {
    fieldSize[j] = size[j] * factors[j] + 5;
  }
  fieldRegion.SetSize(fieldSize);

  auto field = FieldType::New();
  field->SetLargestPossibleRegion(fieldRegion);
  field->SetBufferedRegion(fieldRegion);
  field->Allocate();

  using FieldIterator = itk::ImageRegionIteratorWithIndex<FieldType>;

  for (FieldIterator fieldIter(field, fieldRegion); !fieldIter.IsAtEnd(); ++fieldIter)
  {
    ImageType::IndexType index = fieldIter.GetIndex();
    VectorType           displacement;
    for (unsigned int j = 0; j < ImageDimension; ++j)
    {
      displacement[j] = static_cast<float>(index[j]) * ((1.0 / factors[j]) - 1.0);
    }
    fieldIter.Set(displacement);
  }

  std::cout << "Instantiate WarpImageFilter with VectorImage." << std::endl;

  using WarpVectorImageFilterType = itk::WarpImageFilter<VectorImageType, VectorImageType, VectorImageType>;
  auto warpVectorImageFilter = WarpVectorImageFilterType::New();

  std::cout << "Run WarpImageFilter in standalone mode with progress." << std::endl;
  using WarperType = itk::WarpImageFilter<ImageType, ImageType, FieldType>;
  auto warper = WarperType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(warper, WarpImageFilter, ImageToImageFilter);


  warper->SetDisplacementField(field);
  ITK_TEST_SET_GET_VALUE(field, warper->GetDisplacementField());

  auto interpolator = WarperType::DefaultInterpolatorType::New();
  warper->SetInterpolator(interpolator);
  ITK_TEST_SET_GET_VALUE(interpolator, warper->GetInterpolator());

  warper->SetEdgePaddingValue(padValue);
  ITK_TEST_SET_GET_VALUE(padValue, warper->GetEdgePaddingValue());

  itk::FixedArray<double, ImageDimension> array;
  array.Fill(2.0);
  warper->SetOutputSpacing(array.GetDataPointer());
  ITK_TEST_SET_GET_VALUE(array, warper->GetOutputSpacing());

  array.Fill(1.0);
  warper->SetOutputSpacing(array.GetDataPointer());
  ITK_TEST_SET_GET_VALUE(array, warper->GetOutputSpacing());

  WarperType::PointType ptarray;
  ptarray.Fill(-10.0);
  warper->SetOutputOrigin(ptarray.GetDataPointer());
  ITK_TEST_SET_GET_VALUE(ptarray, warper->GetOutputOrigin());

  ptarray.Fill(0.0);
  warper->SetOutputOrigin(ptarray.GetDataPointer());
  ITK_TEST_SET_GET_VALUE(ptarray, warper->GetOutputOrigin());

  typename WarperType::DirectionType outputDirection;
  outputDirection.SetIdentity();
  warper->SetOutputDirection(outputDirection);
  ITK_TEST_SET_GET_VALUE(outputDirection, warper->GetOutputDirection());

  typename WarperType::IndexType::value_type outputStartIndexVal = 0;
  typename WarperType::IndexType             outputStartIndex;
  outputStartIndex.Fill(outputStartIndexVal);
  warper->SetOutputStartIndex(outputStartIndex);
  ITK_TEST_SET_GET_VALUE(outputStartIndex, warper->GetOutputStartIndex());

  typename WarperType::SizeType::value_type outputSizeVal = 0;
  typename WarperType::SizeType             outputSize;
  outputSize.Fill(outputSizeVal);
  warper->SetOutputSize(outputSize);
  ITK_TEST_SET_GET_VALUE(outputSize, warper->GetOutputSize());

  warper->SetInput(input);

  ShowProgressObject                                    progressWatch(warper);
  itk::SimpleMemberCommand<ShowProgressObject>::Pointer command;
  command = itk::SimpleMemberCommand<ShowProgressObject>::New();
  command->SetCallbackFunction(&progressWatch, &ShowProgressObject::ShowProgress);
  warper->AddObserver(itk::ProgressEvent(), command);

  // Update the filter
  warper->Update();

  std::cout << "Checking the output against expected." << std::endl;

  // compute non-padded output region
  ImageType::RegionType validRegion;
  ImageType::SizeType   validSize;

  // Needed to deal with incompatibility of various IsInside()s &
  // nearest-neighbour type interpolation on half-band at perimeter of
  // image. Evaluate() now has logic for this outer half-band.
  ImageType::SizeType decrementForScaling;
  ImageType::SizeType clampSizeDecrement;
  ImageType::SizeType clampSize;
  for (unsigned int j = 0; j < ImageDimension; ++j)
  {
    validSize[j] = size[j] * factors[j];

    // Consider as inside anything < 1/2 pixel of (size[j]-1)*factors[j]
    //(0-63) map to (0,126), with 127 exactly at 1/2 pixel, therefore
    // edged out; or to (0,190), with 190 just beyond 189 by 1/3 pixel;
    // or to (0,253), with 254 exactly at 1/2 pixel, therefore out
    // also; or (0, 317), with 317 at 2/5 pixel beyond 315. And so on.

    decrementForScaling[j] = factors[j] / 2;

    validSize[j] -= decrementForScaling[j];

    // This part of logic determines what is inside, but in outer
    // 1/2 pixel band, which has to be clamped to that nearest outer
    // pixel scaled by factor: (0,63) maps to (0,190) as inside, but
    // pixel 190 is outside of (0,189), and must be clamped to it.
    // If factor is 2 or less, this decrement has no effect.

    if (factors[j] < 1 + decrementForScaling[j])
    {
      clampSizeDecrement[j] = 0;
    }
    else
    {
      clampSizeDecrement[j] = (factors[j] - 1 - decrementForScaling[j]);
    }
    clampSize[j] = validSize[j] - clampSizeDecrement[j];
  }

  validRegion.SetSize(validSize);

  // adjust the pattern coefficients to match
  for (unsigned int j = 0; j < ImageDimension; ++j)
  {
    pattern.m_Coeff[j] /= static_cast<double>(factors[j]);
  }

  Iterator outIter(warper->GetOutput(), warper->GetOutput()->GetBufferedRegion());
  while (!outIter.IsAtEnd())
  {
    ImageType::IndexType index = outIter.GetIndex();

    double value = outIter.Get();

    if (validRegion.IsInside(index))
    {

      double trueValue = pattern.Evaluate(outIter.GetIndex(), validSize, clampSize, padValue);

      double epsilon = 1e-4;
      if (itk::Math::abs(trueValue - value) > epsilon)
      {
        std::cerr.precision(static_cast<int>(itk::Math::abs(std::log10(epsilon))));
        std::cerr << "Test failed!" << std::endl;
        std::cerr << "Error in Evaluate at index [" << index << "]" << std::endl;
        std::cerr << "Expected value " << trueValue << std::endl;
        std::cerr << " differs from " << value;
        std::cerr << " by more than " << epsilon << std::endl;
        testPassed = EXIT_FAILURE;
      }
    }
    else
    {

      if (itk::Math::NotExactlyEquals(value, padValue))
      {
        std::cerr << "Test failed!" << std::endl;
        std::cerr << "Error in Evaluate at index [" << index << "]" << std::endl;
        std::cerr << "Expected value " << padValue << std::endl;
        std::cerr << " differs from " << value << std::endl;
        testPassed = EXIT_FAILURE;
      }
    }
    ++outIter;
  }

  std::cout << "Run ExpandImageFilter with streamer" << std::endl;

  using VectorCasterType = itk::CastImageFilter<FieldType, FieldType>;
  auto vcaster = VectorCasterType::New();

  vcaster->SetInput(warper->GetDisplacementField());

  auto warper2 = WarperType::New();

  warper2->SetInput(warper->GetInput());
  warper2->SetDisplacementField(vcaster->GetOutput());
  warper2->SetEdgePaddingValue(warper->GetEdgePaddingValue());

  using StreamerType = itk::StreamingImageFilter<ImageType, ImageType>;
  auto streamer = StreamerType::New();
  streamer->SetInput(warper2->GetOutput());
  streamer->SetNumberOfStreamDivisions(3);
  streamer->Update();

  std::cout << "Compare standalone and streamed outputs" << std::endl;

  Iterator streamIter(streamer->GetOutput(), streamer->GetOutput()->GetBufferedRegion());

  outIter.GoToBegin();
  streamIter.GoToBegin();

  while (!outIter.IsAtEnd())
  {
    if (itk::Math::NotAlmostEquals(outIter.Get(), streamIter.Get()))
    {
      std::cerr << "Test failed!" << std::endl;
      std::cerr << "Error in streamed output at index [" << outIter.GetIndex() << "]" << std::endl;
      std::cerr << "Expected value " << outIter.Get() << std::endl;
      std::cerr << " differs from " << streamIter.Get() << std::endl;
      testPassed = EXIT_FAILURE;
    }
    ++outIter;
    ++streamIter;
  }

  // Exercise error handling

  using InterpolatorType = WarperType::InterpolatorType;
  InterpolatorType::Pointer interp = warper->GetModifiableInterpolator();

  std::cout << "Setting interpolator to nullptr" << std::endl;
  warper->SetInterpolator(nullptr);

  ITK_TRY_EXPECT_EXCEPTION(warper->Update());


  warper->ResetPipeline();
  warper->SetInterpolator(interp);


  std::cout << "Test finished." << std::endl;
  return testPassed;
}
