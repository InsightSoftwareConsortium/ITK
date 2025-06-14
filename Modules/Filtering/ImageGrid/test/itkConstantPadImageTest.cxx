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
#include "itkConstantPadImageFilter.h"
#include "itkStreamingImageFilter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkMath.h"

int
itkConstantPadImageTest(int, char *[])
{
  // type alias to simplify the syntax
  using ShortImage = itk::Image<short, 2>;
  using FloatImage = itk::Image<float, 2>;

  // Test the creation of an image with native type
  auto image = ShortImage::New();

  // fill in an image
  ShortImage::IndexType  index = { { 0, 0 } };
  ShortImage::SizeType   size = { { 8, 12 } };
  ShortImage::RegionType region;
  region.SetSize(size);
  region.SetIndex(index);
  image->SetLargestPossibleRegion(region);
  image->SetBufferedRegion(region);
  image->Allocate();

  itk::ImageRegionIterator<ShortImage> iterator(image, region);

  for (short i = 0; !iterator.IsAtEnd(); ++iterator, ++i)
  {
    iterator.Set(i);
  }

  // Create a filter
  using PadFilterType = itk::ConstantPadImageFilter<ShortImage, FloatImage>;
  auto                           constantPad = PadFilterType::New();
  const itk::SimpleFilterWatcher watch(constantPad);
  constantPad->SetInput(image);

  using SizeValueType = ShortImage::SizeValueType;
  using IndexValueType = ShortImage::IndexValueType;

  SizeValueType upperFactors[2] = { 0, 0 };
  SizeValueType lowerFactors[2] = { 0, 0 };

  constexpr float constant = 13.3f;
  constantPad->SetConstant(constant);
  // check the method using the SizeType rather than the simple table type.
  constexpr ShortImage::SizeType stfactors{};
  constantPad->SetPadLowerBound(stfactors);
  constantPad->SetPadUpperBound(stfactors);
  constantPad->SetPadBound(stfactors);
  constantPad->UpdateLargestPossibleRegion();

  std::cout << constantPad << std::endl;
  std::cout << "Input spacing: " << image->GetSpacing()[0] << ", " << image->GetSpacing()[1] << std::endl;
  std::cout << "Output spacing: " << constantPad->GetOutput()->GetSpacing()[0] << ", "
            << constantPad->GetOutput()->GetSpacing()[1] << std::endl;

  // CASE 1
  lowerFactors[0] = 1;
  lowerFactors[1] = 2;
  upperFactors[0] = 3;
  upperFactors[1] = 4;
  constantPad->SetPadLowerBound(lowerFactors);
  constantPad->SetPadUpperBound(upperFactors);
  constantPad->UpdateLargestPossibleRegion();
  ShortImage::RegionType requestedRegion = constantPad->GetOutput()->GetRequestedRegion();


  bool passed = true;
  size = requestedRegion.GetSize();
  index = requestedRegion.GetIndex();
  if ((index[0] != (0 - static_cast<IndexValueType>(lowerFactors[0]))) ||
      (index[1] != (0 - static_cast<IndexValueType>(lowerFactors[1]))) ||
      (size[0] != (8 + lowerFactors[0] + upperFactors[0])) || (size[1] != (12 + lowerFactors[1] + upperFactors[1])))
  {
    passed = false;
  }
  else
  {
    for (itk::ImageRegionIterator<FloatImage> iteratorIn1(constantPad->GetOutput(), requestedRegion);
         !iteratorIn1.IsAtEnd();
         ++iteratorIn1)
    {
      const int row = iteratorIn1.GetIndex()[0];
      const int column = iteratorIn1.GetIndex()[1];
      if ((row < 0) || (row > 7) || (column < 0) || (column > 11))
      {
        if (itk::Math::NotExactlyEquals(iteratorIn1.Get(), constant))
        {
          passed = false;
        }
      }
      else
      {
        const int nextVal = 8 * column + row;
        if (itk::Math::NotExactlyEquals(iteratorIn1.Get(), nextVal))
        {
          std::cout << "Error: (" << row << ", " << column << "), expected " << nextVal << " got " << iteratorIn1.Get()
                    << std::endl;
          passed = false;
        }
      }
    }
  }

  if (passed)
  {
    std::cout << "constantPadImageFilter case 1 passed." << std::endl;
  }
  else
  {
    std::cout << "constantPadImageFilter case 1 failed." << std::endl;
    return EXIT_FAILURE;
  }


  // CASE 2
  lowerFactors[0] = 10;
  upperFactors[1] = 15;
  constantPad->SetPadLowerBound(lowerFactors);
  constantPad->SetPadUpperBound(upperFactors);

  // Create a stream
  using StreamingFilter = itk::StreamingImageFilter<FloatImage, FloatImage>;
  auto stream = StreamingFilter::New();
  stream->SetInput(constantPad->GetOutput());
  stream->SetNumberOfStreamDivisions(2);


  if ((constantPad->GetPadUpperBound()[0] != upperFactors[0]) ||
      (constantPad->GetPadUpperBound()[1] != upperFactors[1]) ||
      (constantPad->GetPadLowerBound()[0] != lowerFactors[0]) ||
      (constantPad->GetPadLowerBound()[1] != lowerFactors[1]))
  {
    passed = false;
  }
  else
  {
    stream->UpdateLargestPossibleRegion();
    requestedRegion = stream->GetOutput()->GetRequestedRegion();

    passed = true;
    size = requestedRegion.GetSize();
    index = requestedRegion.GetIndex();
    if ((index[0] != (0 - static_cast<IndexValueType>(lowerFactors[0]))) ||
        (index[1] != (0 - static_cast<IndexValueType>(lowerFactors[1]))) ||
        (size[0] != (8 + lowerFactors[0] + upperFactors[0])) || (size[1] != (12 + lowerFactors[1] + upperFactors[1])))
    {
      passed = false;
    }
    else
    {
      for (itk::ImageRegionIterator<FloatImage> iteratorIn2(stream->GetOutput(), requestedRegion);
           !iteratorIn2.IsAtEnd();
           ++iteratorIn2)
      {
        const int row = iteratorIn2.GetIndex()[0];
        const int column = iteratorIn2.GetIndex()[1];
        if ((row < 0) || (row > 7) || (column < 0) || (column > 11))
        {
          if (itk::Math::NotExactlyEquals(iteratorIn2.Get(), constant))
          {
            passed = false;
          }
        }
        else
        {
          const int nextVal = 8 * column + row;
          if (itk::Math::NotExactlyEquals(iteratorIn2.Get(), nextVal))
          {
            std::cout << "Error: (" << row << ", " << column << "), expected " << nextVal << " got "
                      << iteratorIn2.Get() << std::endl;
            passed = false;
          }
        }
      }
    }
  }

  if (passed)
  {
    std::cout << "constantPadImageFilter case 2 passed." << std::endl;
  }
  else
  {
    std::cout << "constantPadImageFilter case 2 failed." << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
