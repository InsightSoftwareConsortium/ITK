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

#include "itkVectorExpandImageFilter.h"
#include "itkVectorNearestNeighborInterpolateImageFunction.h"
#include "itkCastImageFilter.h"
#include "itkStreamingImageFilter.h"
#include "itkMath.h"
#include "itkTestingMacros.h"

// class to produce a linear image pattern
template <int VDimension>
class ImagePattern
{
public:
  using IndexType = itk::Index<VDimension>;

  ImagePattern()
  {
    m_Offset = 0.0;
    for (int j = 0; j < VDimension; ++j)
    {
      m_Coeff[j] = 0.0;
    }
  }

  double
  Evaluate(const IndexType & index)
  {
    double accum = m_Offset;
    for (int j = 0; j < VDimension; ++j)
    {
      accum += m_Coeff[j] * static_cast<double>(index[j]);
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
itkVectorExpandImageFilterTest(int, char *[])
{
  using ValueType = float;
  enum
  {
    VectorDimension = 3
  };
  using PixelType = itk::Vector<ValueType, VectorDimension>;
  enum
  {
    ImageDimension = 2
  };
  using ImageType = itk::Image<PixelType, ImageDimension>;

  int testPassed = EXIT_SUCCESS;


  std::cout << "Create the input image pattern." << std::endl;
  ImageType::RegionType         region;
  constexpr ImageType::SizeType size = { { 64, 64 } };
  region.SetSize(size);

  auto input = ImageType::New();
  input->SetLargestPossibleRegion(region);
  input->SetBufferedRegion(region);
  input->Allocate();


  ImagePattern<ImageDimension> pattern;
  pattern.m_Offset = 64;
  for (unsigned int j = 0; j < ImageDimension; ++j)
  {
    pattern.m_Coeff[j] = 1.0;
  }

  constexpr double vectorCoeff[VectorDimension] = { 1.0, 4.0, 6.0 };

  using Iterator = itk::ImageRegionIteratorWithIndex<ImageType>;
  Iterator inIter(input, region);

  for (; !inIter.IsAtEnd(); ++inIter)
  {

    const double value = pattern.Evaluate(inIter.GetIndex());
    PixelType    pixel;
    for (unsigned int k = 0; k < VectorDimension; ++k)
    {
      pixel[k] = vectorCoeff[k] * value;
    }

    inIter.Set(pixel);
  }


  std::cout << "Run ExpandImageFilter in standalone mode with progress." << std::endl;
  using ExpanderType = itk::VectorExpandImageFilter<ImageType, ImageType>;
  auto expander = ExpanderType::New();

  expander->SetInput(input);

  using InterpolatorType = itk::VectorNearestNeighborInterpolateImageFunction<ImageType, double>;
  auto interpolator = InterpolatorType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(
    interpolator, VectorNearestNeighborInterpolateImageFunction, VectorInterpolateImageFunction);


  expander->SetInterpolator(interpolator);
  ITK_TEST_SET_GET_VALUE(interpolator, expander->GetInterpolator());

  expander->SetExpandFactors(5);

  unsigned int factors[ImageDimension] = { 2, 3 };
  expander->SetExpandFactors(factors);

  using PixelType = ImageType::PixelType;
  using ValueType = PixelType::ValueType;
  ValueType            padValueArray[VectorDimension] = { 2.0, 7.0, 9.0 };
  ImageType::PixelType padValue(padValueArray);
  // TEST_RMV20100728   expander->SetEdgePaddingValue( padValue );

  ShowProgressObject                                    progressWatch(expander);
  itk::SimpleMemberCommand<ShowProgressObject>::Pointer command = itk::SimpleMemberCommand<ShowProgressObject>::New();
  command->SetCallbackFunction(&progressWatch, &ShowProgressObject::ShowProgress);
  expander->AddObserver(itk::ProgressEvent(), command);

  expander->Print(std::cout);
  expander->Update();

  ImageType * expanderOutput = expander->GetOutput();


  std::cout << "Checking the output against expected." << std::endl;

  // compute non-padded output region
  ImageType::RegionType     validRegion = expanderOutput->GetLargestPossibleRegion();
  const ImageType::SizeType validSize = validRegion.GetSize();

  validRegion.SetSize(validSize);
  Iterator outIter(expanderOutput, expanderOutput->GetBufferedRegion());
  for (; !outIter.IsAtEnd(); ++outIter)
  {
    const ImageType::IndexType index = outIter.GetIndex();
    ImageType::PixelType       value = outIter.Get();

    if (validRegion.IsInside(index))
    {

      ImageType::PointType point;
      expanderOutput->TransformIndexToPhysicalPoint(outIter.GetIndex(), point);
      const ImageType::IndexType inputIndex = input->TransformPhysicalPointToIndex(point);
      const double               baseValue = pattern.Evaluate(inputIndex);

      unsigned int k = 0;
      for (; k < VectorDimension; ++k)
      {
        if (itk::Math::abs(baseValue * vectorCoeff[k] - value[k]) > 1e-4)
        {
          break;
        }
      }
      if (k < VectorDimension)
      {
        std::cerr << "Test failed!" << std::endl;
        std::cerr << "Error in vector dimension at index [" << index << "]" << std::endl;
        std::cerr << "Expected value less than " << VectorDimension << std::endl;
        std::cerr << " differs from " << k << std::endl;
        testPassed = EXIT_FAILURE;
      }
    }
    else
    {
      unsigned int k = 0;
      for (; k < VectorDimension; ++k)
      {
        if (itk::Math::NotExactlyEquals(value[k], padValue[k]))
        {
          break;
        }
      }
      if (k < VectorDimension)
      {
        std::cerr << "Test failed!" << std::endl;
        std::cerr << "Error in vector dimension at index [" << index << "]" << std::endl;
        std::cerr << "Expected value less than " << VectorDimension << std::endl;
        std::cerr << " differs from " << k << std::endl;
        testPassed = EXIT_FAILURE;
      }
    }
  }


  std::cout << "Run ExpandImageFilter with streamer" << std::endl;

  using CasterType = itk::CastImageFilter<ImageType, ImageType>;
  auto caster = CasterType::New();

  caster->SetInput(expander->GetInput());

  auto expander2 = ExpanderType::New();

  expander2->SetInput(caster->GetOutput());
  expander2->SetExpandFactors(expander->GetExpandFactors());
  // TEST_RMV20100728   expander2->SetEdgePaddingValue( expander->GetEdgePaddingValue() );
  expander2->SetInterpolator(expander->GetModifiableInterpolator());

  using StreamerType = itk::StreamingImageFilter<ImageType, ImageType>;
  auto streamer = StreamerType::New();
  streamer->SetInput(expander2->GetOutput());
  streamer->SetNumberOfStreamDivisions(3);
  streamer->Update();


  std::cout << "Compare standalone and streamed outputs" << std::endl;

  Iterator streamIter(streamer->GetOutput(), streamer->GetOutput()->GetBufferedRegion());

  outIter.GoToBegin();
  streamIter.GoToBegin();

  while (!outIter.IsAtEnd())
  {

    for (unsigned int k = 0; k < VectorDimension; ++k)
    {
      if (itk::Math::NotExactlyEquals(outIter.Get()[k], streamIter.Get()[k]))
      {
        std::cerr << "Test failed!" << std::endl;
        std::cerr << "Error in vector dimension at index [" << k << "]" << std::endl;
        std::cerr << "Expected value " << outIter.Get()[k] << std::endl;
        std::cerr << " differs from " << streamIter.Get()[k] << std::endl;
        testPassed = EXIT_FAILURE;
      }
    }

    ++outIter;
    ++streamIter;
  }

  // Test error handling
  std::cout << "Setting Input to nullptr" << std::endl;

  expander->SetInput(nullptr);
  ITK_TRY_EXPECT_EXCEPTION(expander->Update());


  expander->ResetPipeline();
  expander->SetInput(input);

  std::cout << "Setting Interpolator to nullptr" << std::endl;
  expander->SetInterpolator(nullptr);
  ITK_TRY_EXPECT_EXCEPTION(expander->Update());


  expander->ResetPipeline();
  expander->SetInterpolator(interpolator);


  std::cout << "Test finished." << std::endl;
  return testPassed;
}
