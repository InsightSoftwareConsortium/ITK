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

#include <iostream>
#include "itkExtractImageFilter.h"
#include "itkFileOutputWindow.h"
#include "itkStreamingImageFilter.h"
#include "itkRandomImageSource.h"
#include "itkCastImageFilter.h"
#include "itkTestingMacros.h"

namespace {
bool ExtractImageInPlaceTest( void )
{
  // This is to test the InPlace option
  typedef itk::Image< float, 3 > ImageType;

  typedef itk::RandomImageSource< ImageType > SourceType;
  SourceType::Pointer source = SourceType::New();
  ImageType::SizeType   size = {{32, 32, 32}};
  source->SetSize( size );

  source->UpdateLargestPossibleRegion();


  ImageType::IndexType  extractIndex = {{16, 16, 16}};
  ImageType::SizeType   extractSize = {{8, 8, 8}};
  ImageType::SizeType   zeroSize = {{0,0,0}};

  typedef itk::ExtractImageFilter<ImageType, ImageType> ExtractFilterType;
  ExtractFilterType::Pointer extract = ExtractFilterType::New();
  extract->SetDirectionCollapseToSubmatrix();
  extract->SetExtractionRegion( ImageType::RegionType( extractIndex, extractSize ) );
  extract->InPlaceOn();
  extract->SetInput( source->GetOutput() );
  extract->UpdateLargestPossibleRegion();

  // check that the it was not run in-place
  TEST_EXPECT_TRUE( source->GetOutput()->GetBufferedRegion().GetSize() != zeroSize );

  // add a filter between which will produce the requested region, and
  // enable in-place operation
  typedef itk::CastImageFilter<ImageType, ImageType> SomeStreamableFitlerType;
  SomeStreamableFitlerType::Pointer filter = SomeStreamableFitlerType::New();
  filter->SetInput( source->GetOutput() );
  filter->InPlaceOff(); // ensure a copy is performed

  extract->SetInput( filter->GetOutput() );
  extract->UpdateLargestPossibleRegion();


  // this buffer should still be ok
  TEST_EXPECT_TRUE( source->GetOutput()->GetBufferedRegion().GetSize() != zeroSize );

  // this should have been taken by the in-place;
  TEST_EXPECT_TRUE( filter->GetOutput()->GetBufferedRegion().GetSize() == zeroSize );

  // try with in-place disabled
  extract->InPlaceOff();
  extract->UpdateLargestPossibleRegion();


  // these buffers should still be ok
  TEST_EXPECT_TRUE( source->GetOutput()->GetBufferedRegion().GetSize() != zeroSize );
  TEST_EXPECT_TRUE( filter->GetOutput()->GetBufferedRegion().GetSize() != zeroSize );

  return EXIT_SUCCESS;
}
}

int itkExtractImageTest(int, char* [] )
{
  itk::FileOutputWindow::Pointer fow = itk::FileOutputWindow::New();
  fow->SetInstance(fow);

  int nextVal;

  // typedefs to simplify the syntax
  typedef itk::Image<short, 2>   SimpleImage;
  SimpleImage::Pointer simpleImage = SimpleImage::New();
  std::cout << "Simple image spacing: " << simpleImage->GetSpacing()[0] << ", "
            << simpleImage->GetSpacing()[1] << std::endl;

  // typedefs to simplify the syntax
  typedef itk::Image<short, 2>   ShortImage;
  typedef itk::Image<short, 1>   LineImage;

  // Test the creation of an image with native type
  ShortImage::Pointer if2 = ShortImage::New();

  // fill in an image
  ShortImage::IndexType  index = {{0, 0}};
  ShortImage::SizeType   size = {{8, 12}};
  ShortImage::RegionType region;
  region.SetSize( size );
  region.SetIndex( index );
  if2->SetLargestPossibleRegion( region );
  if2->SetBufferedRegion( region );
  if2->Allocate();

  ShortImage::DirectionType directions;
    directions.SetIdentity();
    directions[0][0] = 0.0;
    directions[1][0] = 1.0;
    directions[0][1] = 1.0;
    directions[1][1] = 0.0;

    if2->SetDirection (directions);

  itk::ImageRegionIterator<ShortImage> iterator(if2, region);

  short i=0;
  for (; !iterator.IsAtEnd(); ++iterator, ++i)
    {
      iterator.Set( i );
    }

  std::cout << "Input Image: " << if2 << std::endl;

  // Create a filter
  itk::ExtractImageFilter< ShortImage, ShortImage >::Pointer extract;
  extract = itk::ExtractImageFilter< ShortImage, ShortImage >::New();
  extract->SetInput( if2 );

  // fill in an image
  ShortImage::IndexType  extractIndex = {{0, 0}};
  ShortImage::SizeType   extractSize = {{8, 12}};
  ShortImage::RegionType extractRegion;
  extractRegion.SetSize( extractSize );
  extractRegion.SetIndex( extractIndex );
  extract->SetExtractionRegion(extractRegion);
  extract->UpdateLargestPossibleRegion();

  std::cout << extract << std::endl;
  std::cout << "Input spacing: " << if2->GetSpacing()[0] << ", "
            << if2->GetSpacing()[1] << std::endl;
  std::cout << "Output spacing: " << extract->GetOutput()->GetSpacing()[0]
            << ", "
            << extract->GetOutput()->GetSpacing()[1] << std::endl;


  ShortImage::RegionType requestedRegion;

  // CASE 1
  extractIndex[0] = 1; extractIndex[1] = 2;
  extractSize[0] = 5; extractSize[1] = 6;
  extractRegion.SetSize( extractSize );
  extractRegion.SetIndex( extractIndex );
  extract->SetExtractionRegion(extractRegion);
  extract->UpdateLargestPossibleRegion();
  requestedRegion = extract->GetOutput()->GetRequestedRegion();

  itk::ImageRegionIterator<ShortImage>
    iteratorIn1(extract->GetOutput(), requestedRegion);

  bool passed = true;
  size = requestedRegion.GetSize();
  index = requestedRegion.GetIndex();

  if ((index[0] != extractIndex[0])
      || (index[1] != extractIndex[1])
      || (size[0] != extractSize[0])
      || (size[1] != extractSize[1]))
    {
      passed = false;
    } else {

      for (; !iteratorIn1.IsAtEnd(); ++iteratorIn1)
        {
          const ShortImage::IndexType::IndexValueType &row = iteratorIn1.GetIndex()[0];
          const ShortImage::IndexType::IndexValueType &column = iteratorIn1.GetIndex()[1];
          if ((row < 0) || (row>7) || (column < 0) || (column > 11)) {
            if ( iteratorIn1.Get() != 13 )
              {
                passed = false;
              }
          } else {
            nextVal = 8*column+row;
            if (iteratorIn1.Get() != nextVal)
              {
                std::cout << "Error: (" << row << ", " << column
                          << "), expected " << nextVal << " got "
                          << iteratorIn1.Get() << std::endl;
                passed = false;
              }
          }
        }
    }

  if (passed)
    {
      std::cout << "ExtractImageFilter case 1 passed." << std::endl;
    }
  else
    {
      std::cout << "ExtractImageFilter case 1 failed." << std::endl;
      return EXIT_FAILURE;
    }

  extract->GetOutput()->Print(std::cout);
  // CASE 2
  extractIndex[0] = 1; extractIndex[1] = 1;
  extractSize[0] = 7; extractSize[1] = 11;
  extractRegion.SetSize( extractSize );
  extractRegion.SetIndex( extractIndex );
  extract->SetExtractionRegion(extractRegion);

  // Create a stream
  itk::StreamingImageFilter< ShortImage, ShortImage >::Pointer stream;
  stream = itk::StreamingImageFilter< ShortImage, ShortImage >::New();
  stream->SetInput( extract->GetOutput() );
  stream->SetNumberOfStreamDivisions(2);

  ShortImage::RegionType setRegion = extract->GetExtractionRegion();
  size = setRegion.GetSize();
  index = setRegion.GetIndex();

  if ((index[0] != extractIndex[0])
      || (index[1] != extractIndex[1])
      || (size[0] != extractSize[0])
      || (size[1] != extractSize[1]))
    {
      passed = false;
    }
  else
    {
      stream->UpdateLargestPossibleRegion();
      requestedRegion = stream->GetOutput()->GetRequestedRegion();

      itk::ImageRegionIterator<ShortImage>
  iteratorIn2(stream->GetOutput(), requestedRegion);

      passed = true;
      size = requestedRegion.GetSize();
      index = requestedRegion.GetIndex();
      if ((index[0] != extractIndex[0])
          || (index[1] != extractIndex[1])
          || (size[0] != extractSize[0])
          || (size[1] != extractSize[1]))
        {
          passed = false;
        } else {
          for (; !iteratorIn2.IsAtEnd(); ++iteratorIn2)
            {
              const ShortImage::IndexType::IndexValueType &row = iteratorIn2.GetIndex()[0];
              const ShortImage::IndexType::IndexValueType &column = iteratorIn2.GetIndex()[1];
              if ((row < 0) || (row>7) || (column < 0) || (column > 11)) {
                if ( iteratorIn2.Get() != 13 )
                  {
                    passed = false;
                  }
              } else {
                nextVal = 8*column+row;
                if (iteratorIn2.Get() != nextVal)
                  {
                    std::cout << "Error: (" << row << ", " << column
                              << "), expected " << nextVal << " got "
                              << iteratorIn2.Get() << std::endl;
                    passed = false;
                  }
              }
            }
        }
    }


  // need to put in code to check whether the proper region was extracted.
  //

  if (passed)
    {
      std::cout << "ExtractImageFilter case 2 passed." << std::endl;
    }
  else
    {
      std::cout << "ExtractImageFilter case 2 failed." << std::endl;
      return EXIT_FAILURE;
    }

  //Case 3: Try extracting a single row
  itk::ExtractImageFilter<ShortImage, LineImage>::Pointer lineExtract;
  lineExtract = itk::ExtractImageFilter<ShortImage, LineImage>::New();
  lineExtract->SetDirectionCollapseToGuess();
  lineExtract->SetInput( if2 );

  extractIndex[0] = 2;
  extractIndex[1] = 0;
  extractSize[0] = 0;
  extractSize[1] = 3;
  extractRegion.SetIndex( extractIndex );
  extractRegion.SetSize( extractSize );

  lineExtract->SetExtractionRegion( extractRegion );
  lineExtract->UpdateLargestPossibleRegion();
  lineExtract->GetOutput()->Print(std::cout);

  std::cout << "After 1D extraction. " << std::endl;

  //test the dimension collapse
  LineImage::RegionType requestedLineRegion;

  requestedLineRegion = lineExtract->GetOutput()->GetRequestedRegion();

  itk::ImageRegionIterator<LineImage>
    iteratorLineIn(lineExtract->GetOutput(), requestedLineRegion);

  ShortImage::IndexType testIndex;
  for (; !iteratorLineIn.IsAtEnd(); ++iteratorLineIn)
    {
    LineImage::PixelType linePixelValue = iteratorLineIn.Get();
    testIndex[0] = extractIndex[0];
    testIndex[1] = iteratorLineIn.GetIndex()[0];
    if (linePixelValue != if2->GetPixel(testIndex))
      {
      passed = false;
      }
    }
  if (passed)
    {
      std::cout << "ExtractImageFilter case 3 passed." << std::endl;
    }
  else
    {
      std::cout << "ExtractImageFilter case 3 failed." << std::endl;
      return EXIT_FAILURE;
    }

  return ExtractImageInPlaceTest();
}
