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
#include "itkStreamingImageFilter.h"
#include "itkWrapPadImageFilter.h"
#include "itkVectorImage.h"
#include "itkMath.h"

//
// Check that val represents the correct pixel value.  This routine
// allows the pad region to extend to twice the size of the input.
//
int VerifyPixel(int row, int col, short val, float & expected)
{
  if (row < 0) row += 8;
  if (row < 0) row += 8;
  if (row > 7) row -= 8;
  if (row > 7) row -= 8;
  if (col < 0) col += 12;
  if (col < 0) col += 12;
  if (col > 11) col -= 12;
  if (col > 11) col -= 12;
  expected = 8*col+row;
  return ( itk::Math::AlmostEquals( val, expected) );
}


int itkWrapPadImageTest(int, char* [] )
{
  // typedefs to simplify the syntax
  typedef itk::Image< short, 2 >       ShortImage;
  typedef itk::Image< float, 2 >       FloatImage;
  typedef itk::VectorImage< short, 2 > VectorImage;

  // Test the creation of an image with native type
  ShortImage::Pointer    image = ShortImage::New();
  ShortImage::IndexType  index = {{0, 0}};
  ShortImage::SizeType   size = {{8, 12}};
  ShortImage::RegionType region( index, size );
  image->SetRegions( region );
  image->Allocate();

  VectorImage::Pointer vectorImage = VectorImage::New();
  vectorImage->SetRegions( region );
  vectorImage->SetNumberOfComponentsPerPixel( 3 );
  vectorImage->Allocate();

  itk::ImageRegionIterator< ShortImage > it( image, region );
  itk::ImageRegionIterator< VectorImage > vit( vectorImage, region );

  ShortImage::PixelType i = 0;
  for (; !it.IsAtEnd(); ++it, ++vit, ++i)
    {
    it.Set( i );
    VectorImage::PixelType vectorPixel( 3 );
    vectorPixel = i;
    vectorPixel[1] = i+1;
    vit.Set( vectorPixel );
    }

  // Create a filter
  typedef itk::WrapPadImageFilter< ShortImage, FloatImage > PadFilterType;
  PadFilterType::Pointer wrapPad = PadFilterType::New();
  wrapPad->SetInput( image );

  itk::WrapPadImageFilter< VectorImage, VectorImage >::Pointer vectorWrapPad;
  vectorWrapPad = itk::WrapPadImageFilter< VectorImage, VectorImage >::New();
  vectorWrapPad->SetInput( vectorImage );

  ShortImage::SizeValueType upperBound[2] = { 0, 0};
  ShortImage::SizeValueType lowerBound[2] = { 0, 0};

  wrapPad->SetPadLowerBound( lowerBound );
  wrapPad->SetPadUpperBound( upperBound );
  wrapPad->UpdateLargestPossibleRegion();

  vectorWrapPad->SetPadLowerBound( lowerBound );
  vectorWrapPad->SetPadUpperBound( upperBound );
  vectorWrapPad->UpdateLargestPossibleRegion();

  std::cout << wrapPad << std::endl;
  std::cout << vectorWrapPad << std::endl;

  std::cout << "Input spacing: " << image->GetSpacing()[0] << ", "
            << image->GetSpacing()[1] << std::endl;
  std::cout << "Output spacing: " << wrapPad->GetOutput()->GetSpacing()[0]
            << ", " << wrapPad->GetOutput()->GetSpacing()[1] << std::endl;

  ShortImage::RegionType requestedRegion;
  bool passed;

  // CASE 1
  lowerBound[0] = 1; lowerBound[1] = 3;
  upperBound[0] = 2; upperBound[1] = 4;
  wrapPad->SetPadLowerBound( lowerBound );
  wrapPad->SetPadUpperBound( upperBound );
  wrapPad->UpdateLargestPossibleRegion();

  vectorWrapPad->SetPadLowerBound( lowerBound );
  vectorWrapPad->SetPadUpperBound( upperBound );
  vectorWrapPad->UpdateLargestPossibleRegion();

  requestedRegion = wrapPad->GetOutput()->GetRequestedRegion();

  itk::ImageRegionIterator< FloatImage > itIn1( wrapPad->GetOutput(), requestedRegion );
  itk::ImageRegionIterator< VectorImage > vitIn1( vectorWrapPad->GetOutput(), requestedRegion );

  passed = true;
  size = requestedRegion.GetSize();
  index = requestedRegion.GetIndex();
  if ( ( index[0] != (0 - (long) lowerBound[0] ) )
      || ( index[1] != (0 - (long) lowerBound[1] ) )
      || ( size[0] != (8 + lowerBound[0] + upperBound[0] ) )
      || ( size[1] != (12 + lowerBound[1] + upperBound[1] ) ) )
    {
    passed = false;
    }
  else
    {
    for (; !itIn1.IsAtEnd(); ++itIn1, ++vitIn1)
      {
      int row = itIn1.GetIndex()[0];
      int column = itIn1.GetIndex()[1];
      FloatImage::PixelType expected = 0.0f;

      if ( !VerifyPixel( row, column, static_cast<short>(itIn1.Get()), expected ) )
        {
        std::cout << "Error in wrapPad: index (" << row << ", " << column
                  << "). Got " << itIn1.Get() << ", expected " << expected << std::endl;
        passed = false;
        }

      if ( !VerifyPixel( row, column, vitIn1.Get()[1] - 1, expected ) )
        {
        std::cout << "Error in vectorWrapPad: index (" << row << ", " << column
                  << "). Got " << vitIn1.Get()[1] - 1 << ", expected " << expected << std::endl;
        passed = false;
        }
      }
    }

  if (passed)
    {
    std::cout << "WrapPadImageFilter case 1 passed." << std::endl;
    }
  else
    {
    std::cout << "WrapPadImageFilter case 1 failed." << std::endl;
    return EXIT_FAILURE;
    }


  // CASE 2
  lowerBound[0] = 10;
  upperBound[1] = 15;
  wrapPad->SetPadLowerBound( lowerBound );
  wrapPad->SetPadUpperBound( upperBound );
  vectorWrapPad->SetPadLowerBound( lowerBound );
  vectorWrapPad->SetPadUpperBound( upperBound );

  if ((wrapPad->GetPadUpperBound()[0] != upperBound[0])
      || (wrapPad->GetPadUpperBound()[1] != upperBound[1])
      || (wrapPad->GetPadLowerBound()[0] != lowerBound[0])
      || (wrapPad->GetPadLowerBound()[1] != lowerBound[1]))
    {
    passed = false;
    }
  else
    {
    wrapPad->UpdateLargestPossibleRegion();
    vectorWrapPad->UpdateLargestPossibleRegion();
    requestedRegion = wrapPad->GetOutput()->GetRequestedRegion();

    itk::ImageRegionIterator< FloatImage > itIn2( wrapPad->GetOutput(), requestedRegion );
    itk::ImageRegionIterator< VectorImage > vitIn2( vectorWrapPad->GetOutput(), requestedRegion );

    passed = true;
    size = requestedRegion.GetSize();
    index = requestedRegion.GetIndex();
    if ( ( index[0] != (0 - (long) lowerBound[0] ) )
        || ( index[1] != (0 - (long) lowerBound[1] ) )
        || ( size[0] != (8 + lowerBound[0] + upperBound[0] ) )
        || ( size[1] != (12 + lowerBound[1] + upperBound[1] ) ) )
      {
      passed = false;
      }
    else
      {
      for (; !itIn2.IsAtEnd(); ++itIn2, ++vitIn2 )
        {
        int row = itIn2.GetIndex()[0];
        int column = itIn2.GetIndex()[1];
        FloatImage::PixelType expected = 0.0f;

        if ( !VerifyPixel( row, column, static_cast<short>(itIn2.Get()), expected ) )
          {
          std::cout << "Error in wrapPad: index (" << row << ", " << column
                    << "). Got " << itIn2.Get() << ", expected " << expected << std::endl;
          passed = false;
          }

        if ( !VerifyPixel( row, column, vitIn2.Get()[1] - 1, expected ) )
          {
          std::cout << "Error in vectorWrapPad: index (" << row << ", " << column
                    << "). Got " << vitIn2.Get()[1] - 1 << ", expected " << expected << std::endl;
          passed = false;
          }
        }
      }
    }

  if ( passed )
    {
    std::cout << "WrapPadImageFilter case 2 passed." << std::endl;
    }
  else
    {
    std::cout << "WrapPadImageFilter case 2 failed." << std::endl;
    return EXIT_FAILURE;
    }


  // CASE 3
  lowerBound[1] = 16;
  upperBound[0] = 9;
  wrapPad->SetPadLowerBound( lowerBound );
  wrapPad->SetPadUpperBound( upperBound );
  vectorWrapPad->SetPadLowerBound( lowerBound );
  vectorWrapPad->SetPadUpperBound( upperBound );

  // Create a stream
  typedef itk::StreamingImageFilter< FloatImage, FloatImage > StreamingFilter;
  StreamingFilter::Pointer stream = StreamingFilter::New();
  stream->SetInput( wrapPad->GetOutput() );
  stream->SetNumberOfStreamDivisions( 3 );

  itk::StreamingImageFilter< VectorImage, VectorImage >::Pointer vectorStream =
    itk::StreamingImageFilter< VectorImage, VectorImage >::New();
  vectorStream->SetInput( vectorWrapPad->GetOutput() );
  vectorStream->SetNumberOfStreamDivisions( 3 );

  if ( ( wrapPad->GetPadUpperBound()[0] != upperBound[0] )
      || ( wrapPad->GetPadUpperBound()[1] != upperBound[1] )
      || ( wrapPad->GetPadLowerBound()[0] != lowerBound[0] )
      || ( wrapPad->GetPadLowerBound()[1] != lowerBound[1] ) )
    {
    passed = false;
    }
  else
    {
    stream->UpdateLargestPossibleRegion();
    vectorStream->UpdateLargestPossibleRegion();
    requestedRegion = stream->GetOutput()->GetRequestedRegion();

    itk::ImageRegionIterator< FloatImage > itIn3(stream->GetOutput(), requestedRegion);
    itk::ImageRegionIterator< VectorImage > vitIn3(vectorStream->GetOutput(), requestedRegion);

    passed = true;
    size = requestedRegion.GetSize();
    index = requestedRegion.GetIndex();
    if ( ( index[0] != (0 - (long) lowerBound[0] ) )
        || ( index[1] != (0 - (long) lowerBound[1] ) )
        || ( size[0] != (8 + lowerBound[0] + upperBound[0] ) )
        || ( size[1] != (12 + lowerBound[1] + upperBound[1] ) ) )
      {
      passed = false;
      }
    else
      {
      for (; !itIn3.IsAtEnd(); ++itIn3, ++vitIn3 )
        {
        int row = itIn3.GetIndex()[0];
        int column = itIn3.GetIndex()[1];
        FloatImage::PixelType expected = 0.0f;

        if ( !VerifyPixel( row, column, static_cast<short>(itIn3.Get()), expected ) )
          {
          std::cout << "Error in wrapPad: index (" << row << ", " << column
                    << "). Got " << itIn3.Get() << ", expected " << expected << std::endl;
          passed = false;
          }

        if ( !VerifyPixel( row, column, vitIn3.Get()[1] - 1, expected ) )
          {
          std::cout << "Error in vectorWrapPad: index (" << row << ", " << column
                    << "). Got " << vitIn3.Get()[1] - 1 << ", expected " << expected << std::endl;
          passed = false;
          }
        }
      }
    }

  if ( passed )
    {
    std::cout << "WrapPadImageFilter case 3 passed." << std::endl;
    }
  else
    {
    std::cout << "WrapPadImageFilter case 3 failed." << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
