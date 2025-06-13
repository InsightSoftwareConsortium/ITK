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

#include <fstream>
#include "itkMirrorPadImageFilter.h"
#include "itkStreamingImageFilter.h"
#include "itkSimpleFilterWatcher.h"

//
// Check that val represents the correct pixel value.  This routine
// allows the pad region to extend to twice the size of the input.
//
namespace
{
int
VerifyPixel(int row, int col, int val)
{
  int rowVal = row;
  int colVal = col;

  if (row < 0)
  {
    row += 8;
    rowVal = 7 - row;
  }
  if (row < 0)
  {
    row += 8;
    rowVal = row;
  }
  if (row > 7)
  {
    row -= 8;
    rowVal = 7 - row;
  }
  if (row > 7)
  {
    row -= 8;
    rowVal = row;
  }
  if (col < 0)
  {
    col += 12;
    colVal = 11 - col;
  }
  if (col < 0)
  {
    col += 12;
    colVal = col;
  }
  if (col > 11)
  {
    col -= 12;
    colVal = 11 - col;
  }
  if (col > 11)
  {
    col -= 12;
    colVal = col;
  }
  const int nextVal = 8 * colVal + rowVal;
  return (val == nextVal);
}
} // namespace

int
itkMirrorPadImageTest(int, char *[])
{
  // type alias to simplify the syntax
  using SimpleImage = itk::Image<short, 2>;
  auto simpleImage = SimpleImage::New();
  std::cout << "Simple image spacing: " << simpleImage->GetSpacing()[0] << ", " << simpleImage->GetSpacing()[1]
            << std::endl;

  // type alias to simplify the syntax
  using ShortImage = itk::Image<short, 2>;

  // Test the creation of an image with native type
  auto if2 = ShortImage::New();

  // fill in an image
  ShortImage::IndexType  index = { { 0, 0 } };
  ShortImage::SizeType   size = { { 8, 12 } };
  ShortImage::RegionType region;
  region.SetSize(size);
  region.SetIndex(index);
  if2->SetLargestPossibleRegion(region);
  if2->SetBufferedRegion(region);
  if2->Allocate();

  {
    short i = 0;
    for (itk::ImageRegionIterator<ShortImage> iterator(if2, region); !iterator.IsAtEnd(); ++iterator)
    {
      iterator.Set(i);
      ++i;
    }
  }
  // Create a filter
  const itk::MirrorPadImageFilter<ShortImage, ShortImage>::Pointer mirrorPad =
    itk::MirrorPadImageFilter<ShortImage, ShortImage>::New();
  const itk::SimpleFilterWatcher watcher(mirrorPad);

  mirrorPad->SetInput(if2);

  using SizeValueType = ShortImage::SizeValueType;
  using IndexValueType = ShortImage::IndexValueType;

  SizeValueType upperfactors[2] = { 0, 0 };
  SizeValueType lowerfactors[2] = { 0, 0 };

  mirrorPad->SetPadLowerBound(lowerfactors);
  mirrorPad->SetPadUpperBound(upperfactors);
  mirrorPad->UpdateLargestPossibleRegion();

  std::cout << mirrorPad << std::endl;

  std::cout << "Input spacing: " << if2->GetSpacing()[0] << ", " << if2->GetSpacing()[1] << std::endl;
  std::cout << "Output spacing: " << mirrorPad->GetOutput()->GetSpacing()[0] << ", "
            << mirrorPad->GetOutput()->GetSpacing()[1] << std::endl;

  // CASE 1
  lowerfactors[0] = 3;
  lowerfactors[1] = 7;
  upperfactors[0] = 5;
  upperfactors[1] = 9;
  mirrorPad->SetPadLowerBound(lowerfactors);
  mirrorPad->SetPadUpperBound(upperfactors);
  mirrorPad->UpdateLargestPossibleRegion();
  ShortImage::RegionType requestedRegion = mirrorPad->GetOutput()->GetRequestedRegion();


  bool passed = true;
  size = requestedRegion.GetSize();
  index = requestedRegion.GetIndex();
  if ((index[0] != (0 - static_cast<IndexValueType>(lowerfactors[0]))) ||
      (index[1] != (0 - static_cast<IndexValueType>(lowerfactors[1]))) ||
      (size[0] != (8 + lowerfactors[0] + upperfactors[0])) || (size[1] != (12 + lowerfactors[1] + upperfactors[1])))
  {
    passed = false;
  }
  else
  {
    for (itk::ImageRegionIterator<ShortImage> iteratorIn1(mirrorPad->GetOutput(), requestedRegion);
         !iteratorIn1.IsAtEnd();
         ++iteratorIn1)
    {
      const int row = iteratorIn1.GetIndex()[0];
      const int column = iteratorIn1.GetIndex()[1];
      if (!VerifyPixel(row, column, iteratorIn1.Get()))
      {
        std::cout << "Error: (" << row << ", " << column << "), got " << iteratorIn1.Get() << std::endl;
        passed = false;
      }
    }
  }

  if (passed)
  {
    std::cout << "mirrorPadImageFilter case 1 passed." << std::endl;
  }
  else
  {
    std::cout << "mirrorPadImageFilter case 1 failed." << std::endl;
    return EXIT_FAILURE;
  }


  // CASE 2
  lowerfactors[0] = 10;
  upperfactors[1] = 15;
  mirrorPad->SetPadLowerBound(lowerfactors);
  mirrorPad->SetPadUpperBound(upperfactors);

  if ((mirrorPad->GetPadUpperBound()[0] != upperfactors[0]) || (mirrorPad->GetPadUpperBound()[1] != upperfactors[1]) ||
      (mirrorPad->GetPadLowerBound()[0] != lowerfactors[0]) || (mirrorPad->GetPadLowerBound()[1] != lowerfactors[1]))
  {
    passed = false;
  }
  else
  {
    mirrorPad->UpdateLargestPossibleRegion();
    requestedRegion = mirrorPad->GetOutput()->GetRequestedRegion();


    passed = true;
    size = requestedRegion.GetSize();
    index = requestedRegion.GetIndex();
    if ((index[0] != (0 - static_cast<IndexValueType>(lowerfactors[0]))) ||
        (index[1] != (0 - static_cast<IndexValueType>(lowerfactors[1]))) ||
        (size[0] != (8 + lowerfactors[0] + upperfactors[0])) || (size[1] != (12 + lowerfactors[1] + upperfactors[1])))
    {
      passed = false;
    }
    else
    {
      for (itk::ImageRegionIterator<ShortImage> iteratorIn2(mirrorPad->GetOutput(), requestedRegion);
           !iteratorIn2.IsAtEnd();
           ++iteratorIn2)
      {
        const int row = iteratorIn2.GetIndex()[0];
        const int column = iteratorIn2.GetIndex()[1];
        if (!VerifyPixel(row, column, iteratorIn2.Get()))
        {
          std::cout << "Error: (" << row << ", " << column << "), got " << iteratorIn2.Get() << std::endl;
          passed = false;
        }
      }
    }
  }

  if (passed)
  {
    std::cout << "mirrorPadImageFilter case 2 passed." << std::endl;
  }
  else
  {
    std::cout << "mirrorPadImageFilter case 2 failed." << std::endl;
    return EXIT_FAILURE;
  }


  // CASE 3
  lowerfactors[1] = 16;
  upperfactors[0] = 9;
  mirrorPad->SetPadLowerBound(lowerfactors);
  mirrorPad->SetPadUpperBound(upperfactors);

  // Create a stream
  itk::StreamingImageFilter<ShortImage, ShortImage>::Pointer stream;
  stream = itk::StreamingImageFilter<ShortImage, ShortImage>::New();
  stream->SetInput(mirrorPad->GetOutput());
  stream->SetNumberOfStreamDivisions(3);

  if ((mirrorPad->GetPadUpperBound()[0] != upperfactors[0]) || (mirrorPad->GetPadUpperBound()[1] != upperfactors[1]) ||
      (mirrorPad->GetPadLowerBound()[0] != lowerfactors[0]) || (mirrorPad->GetPadLowerBound()[1] != lowerfactors[1]))
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
    if ((index[0] != (0 - static_cast<IndexValueType>(lowerfactors[0]))) ||
        (index[1] != (0 - static_cast<IndexValueType>(lowerfactors[1]))) ||
        (size[0] != (8 + lowerfactors[0] + upperfactors[0])) || (size[1] != (12 + lowerfactors[1] + upperfactors[1])))
    {
      passed = false;
    }
    else
    {
      for (itk::ImageRegionIterator<ShortImage> iteratorIn3(stream->GetOutput(), requestedRegion);
           !iteratorIn3.IsAtEnd();
           ++iteratorIn3)
      {
        const int row = iteratorIn3.GetIndex()[0];
        const int column = iteratorIn3.GetIndex()[1];
        if (!VerifyPixel(row, column, iteratorIn3.Get()))
        {
          std::cout << "Error: (" << row << ", " << column << "), got " << iteratorIn3.Get() << std::endl;
          passed = false;
        }
      }
    }
  }

  if (passed)
  {
    std::cout << "mirrorPadImageFilter case 3 passed." << std::endl;
  }
  else
  {
    std::cout << "mirrorPadImageFilter case 3 failed." << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
