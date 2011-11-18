/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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

#include <fstream>
#include "itkMirrorPadImageFilter.h"
#include "itkStreamingImageFilter.h"
#include "itkFilterWatcher.h"

//
// Check that val represents the correct pixel value.  This routine
// allows the pad region to extend to twice the size of the input.
//
namespace
{
int VerifyPixel(int row, int col, int val)
{
  int nextVal;
  int rowVal;
  int colVal;

  rowVal = row;
  colVal = col;

  if (row < 0)
  {
      row += 8;
    rowVal = 7 - row;
  }
  if (row < 0)
  {
    row += 8;
      rowVal=row;
  }
  if (row > 7)
  {
      row -= 8;
      rowVal = 7 - row;
  }
  if (row > 7)
  {
      row -= 8;
      rowVal=row;
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
  nextVal = 8*colVal+rowVal;
  return (val == nextVal);
}
}

int itkMirrorPadImageTest(int, char* [] )
{
//  itk::FileOutputWindow::Pointer fow = itk::FileOutputWindow::New();
//  fow->SetInstance(fow);

//  itk::MultiThreader::SetGlobalDefaultNumberOfThreads(8);

  // typedefs to simplify the syntax
  typedef itk::Image<short, 2>   SimpleImage;
  SimpleImage::Pointer simpleImage = SimpleImage::New();
  std::cout << "Simple image spacing: " << simpleImage->GetSpacing()[0] << ", "
            << simpleImage->GetSpacing()[1] << std::endl;

  // typedefs to simplify the syntax
  typedef itk::Image<short, 2>   ShortImage;

  // Test the creation of an image with native type
  ShortImage::Pointer if2 = ShortImage::New();

  // fill in an image
  ShortImage::IndexType  index = {{0, 0}};
  ShortImage::SizeType   size = {{8, 12}};
  ShortImage::RegionType region;
  int row, column;
  region.SetSize( size );
  region.SetIndex( index );
  if2->SetLargestPossibleRegion( region );
  if2->SetBufferedRegion( region );
  if2->Allocate();

  itk::ImageRegionIterator<ShortImage> iterator(if2, region);

  short i=0;
  for (; !iterator.IsAtEnd(); ++iterator, ++i)
    {
      iterator.Set( i );
    }

  // Create a filter
  itk::MirrorPadImageFilter< ShortImage, ShortImage >::Pointer mirrorPad;

  mirrorPad = itk::MirrorPadImageFilter< ShortImage, ShortImage >::New();
  FilterWatcher watcher(mirrorPad);

  mirrorPad->SetInput( if2 );

  typedef ShortImage::SizeValueType    SizeValueType;
  typedef ShortImage::IndexValueType   IndexValueType;

  SizeValueType upperfactors[2] = { 0, 0};
  SizeValueType lowerfactors[2] = { 0, 0};

  mirrorPad->SetPadLowerBound(lowerfactors);
  mirrorPad->SetPadUpperBound(upperfactors);
  mirrorPad->UpdateLargestPossibleRegion();

  std::cout << mirrorPad << std::endl;

  std::cout << "Input spacing: " << if2->GetSpacing()[0] << ", "
            << if2->GetSpacing()[1] << std::endl;
  std::cout << "Output spacing: " << mirrorPad->GetOutput()->GetSpacing()[0]
            << ", "
            << mirrorPad->GetOutput()->GetSpacing()[1] << std::endl;


  ShortImage::RegionType requestedRegion;
  bool passed;

  // CASE 1
  lowerfactors[0] = 3; lowerfactors[1] = 7;
  upperfactors[0] = 5; upperfactors[1] = 9;
  mirrorPad->SetPadLowerBound(lowerfactors);
  mirrorPad->SetPadUpperBound(upperfactors);
  mirrorPad->UpdateLargestPossibleRegion();
  requestedRegion = mirrorPad->GetOutput()->GetRequestedRegion();

  itk::ImageRegionIterator<ShortImage>
    iteratorIn1(mirrorPad->GetOutput(), requestedRegion);

  passed = true;
  size = requestedRegion.GetSize();
  index = requestedRegion.GetIndex();
  if ((index[0] != (0 - (IndexValueType) lowerfactors[0]))
      || (index[1] != (0 - (IndexValueType) lowerfactors[1]))
      || (size[0] != (8 + lowerfactors[0] + upperfactors[0]))
      || (size[1] != (12 + lowerfactors[1] + upperfactors[1]))) {
    passed = false;
  } else {

    for (; !iteratorIn1.IsAtEnd(); ++iteratorIn1)
      {
  row = iteratorIn1.GetIndex()[0];
  column = iteratorIn1.GetIndex()[1];
  if (!VerifyPixel(row, column, iteratorIn1.Get()))
    {
      std::cout << "Error: (" << row << ", " << column
          << "), got "
          << iteratorIn1.Get() << std::endl;
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

  if ((mirrorPad->GetPadUpperBound()[0] != upperfactors[0])
      || (mirrorPad->GetPadUpperBound()[1] != upperfactors[1])
      || (mirrorPad->GetPadLowerBound()[0] != lowerfactors[0])
      || (mirrorPad->GetPadLowerBound()[1] != lowerfactors[1]))
    {
      passed = false;
    }
  else
    {
      mirrorPad->UpdateLargestPossibleRegion();
      requestedRegion = mirrorPad->GetOutput()->GetRequestedRegion();

      itk::ImageRegionIterator<ShortImage>
  iteratorIn2(mirrorPad->GetOutput(), requestedRegion);

      passed = true;
      size = requestedRegion.GetSize();
      index = requestedRegion.GetIndex();
      if ((index[0] != (0 - (IndexValueType) lowerfactors[0]))
    || (index[1] != (0 - (IndexValueType) lowerfactors[1]))
    || (size[0] != (8 + lowerfactors[0] + upperfactors[0]))
    || (size[1] != (12 + lowerfactors[1] + upperfactors[1]))) {
  passed = false;
      } else {
  for (; !iteratorIn2.IsAtEnd(); ++iteratorIn2)
    {
      row = iteratorIn2.GetIndex()[0];
      column = iteratorIn2.GetIndex()[1];
      if (!VerifyPixel(row, column, iteratorIn2.Get()))
        {
    std::cout << "Error: (" << row << ", " << column
        << "), got "
        << iteratorIn2.Get() << std::endl;
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
  itk::StreamingImageFilter< ShortImage, ShortImage >::Pointer stream;
  stream = itk::StreamingImageFilter< ShortImage, ShortImage >::New();
  stream->SetInput( mirrorPad->GetOutput() );
  stream->SetNumberOfStreamDivisions(3);

  if ((mirrorPad->GetPadUpperBound()[0] != upperfactors[0])
      || (mirrorPad->GetPadUpperBound()[1] != upperfactors[1])
      || (mirrorPad->GetPadLowerBound()[0] != lowerfactors[0])
      || (mirrorPad->GetPadLowerBound()[1] != lowerfactors[1]))
    {
      passed = false;
    }
  else
    {
      stream->UpdateLargestPossibleRegion();
      requestedRegion = stream->GetOutput()->GetRequestedRegion();

      itk::ImageRegionIterator<ShortImage>
        iteratorIn3(stream->GetOutput(), requestedRegion);

      passed = true;
      size = requestedRegion.GetSize();
      index = requestedRegion.GetIndex();
      if ((index[0] != (0 - (IndexValueType) lowerfactors[0]))
    || (index[1] != (0 - (IndexValueType) lowerfactors[1]))
    || (size[0] != (8 + lowerfactors[0] + upperfactors[0]))
    || (size[1] != (12 + lowerfactors[1] + upperfactors[1]))) {
  passed = false;
      } else {
  for (; !iteratorIn3.IsAtEnd(); ++iteratorIn3)
    {
      row = iteratorIn3.GetIndex()[0];
      column = iteratorIn3.GetIndex()[1];
      if (!VerifyPixel(row, column, iteratorIn3.Get()))
        {
    std::cout << "Error: (" << row << ", " << column
        << "), got "
        << iteratorIn3.Get() << std::endl;
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
