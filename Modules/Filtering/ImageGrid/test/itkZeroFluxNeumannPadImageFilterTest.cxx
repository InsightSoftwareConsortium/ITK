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
#include "itkMath.h"
#include "itkZeroFluxNeumannPadImageFilter.h"
#include "itkStreamingImageFilter.h"

typedef itk::Image< short, 2 >     ShortImage;
typedef itk::Image< float, 2 >     FloatImage;
typedef ShortImage::SizeValueType  SizeValueType;
typedef ShortImage::IndexValueType IndexValueType;

typedef itk::ZeroFluxNeumannPadImageFilter< ShortImage, FloatImage > FilterType;

static bool VerifyFilterOutput(const ShortImage * inputImage,
                               const FloatImage * outputImage)
{
  ShortImage::RegionType inputRegion = inputImage->GetLargestPossibleRegion();
  ShortImage::IndexType inputIndex = inputRegion.GetIndex();
  ShortImage::SizeType  inputSize  = inputRegion.GetSize();

  ShortImage::RegionType outputRegion = outputImage->GetLargestPossibleRegion();
  itk::ImageRegionConstIteratorWithIndex< FloatImage >
    outputIterator(outputImage, outputRegion);

  // Check pixel values
  for (; !outputIterator.IsAtEnd(); ++outputIterator)
    {
    ShortImage::IndexType idx = outputIterator.GetIndex();
    if ( inputRegion.IsInside( idx ) )
      {
      if ( itk::Math::NotAlmostEquals( outputIterator.Get(), inputImage->GetPixel( idx ) ) )
        {
        std::cerr << "Invalid output value at interior index "
                  << outputIterator.GetIndex() << ". Got "
                  << outputIterator.Get() << ", expected "
                  << inputImage->GetPixel( idx ) << std::endl;
        return false;
        }
      }
    else
      {
      ShortImage::IndexType borderIdx = idx;
      for ( unsigned int i = 0; i < ShortImage::ImageDimension; i++ )
        {
        if ( borderIdx[i] < inputIndex[i] )
          {
          borderIdx[i] = inputIndex[i];
          }
        else if ( borderIdx[i] > inputIndex[i] +
             static_cast<ShortImage::IndexValueType>( inputSize[i] ) - 1)
          {
          borderIdx[i] = inputIndex[i] + inputSize[i] - 1;
          }
        }

      if ( itk::Math::NotAlmostEquals( outputIterator.Get(), inputImage->GetPixel( borderIdx ) ) )
        {
        std::cerr << "Invalid output value at pad index "
                  << outputIterator.GetIndex() << ". Got "
                  << outputIterator.Get() << ", expected "
                  << inputImage->GetPixel( borderIdx ) << std::endl;
        return false;
        }
      }
    }

  return true;
}

static bool VerifyFilter(const ShortImage * inputImage,
                         FilterType * padFilter,
                         const SizeValueType * lowerBound,
                         const SizeValueType * upperBound)
{
  std::cout << "Verifying filter output metadata." << std::endl;

  padFilter->SetPadLowerBound(lowerBound);
  padFilter->SetPadUpperBound(upperBound);
  padFilter->UpdateLargestPossibleRegion();

  if ( (padFilter->GetPadLowerBound()[0] != lowerBound[0]) ||
       (padFilter->GetPadLowerBound()[1] != lowerBound[1]) )
    {
    std::cerr << "[FAILED]" << std::endl;
    std::cerr << "Incorrect lower bounds returned from the filter." << std::endl;
    return false;
    }

  if ( (padFilter->GetPadUpperBound()[0] != upperBound[0]) ||
       (padFilter->GetPadUpperBound()[1] != upperBound[1]) )
    {
    std::cerr << "[FAILED]" << std::endl;
    std::cerr << "Incorrect upper bounds returned from the filter." << std::endl;
    return false;
    }

  ShortImage::RegionType outputRegion =
    padFilter->GetOutput()->GetLargestPossibleRegion();
  ShortImage::IndexType outputIndex = outputRegion.GetIndex();
  ShortImage::SizeType  outputSize  = outputRegion.GetSize();

  ShortImage::RegionType inputRegion = inputImage->GetLargestPossibleRegion();
  ShortImage::IndexType  inputIndex  = inputRegion.GetIndex();
  ShortImage::SizeType   inputSize   = inputRegion.GetSize();

  ShortImage::IndexType expectedIndex;
  ShortImage::SizeType  expectedSize;

  for ( unsigned int i = 0; i < ShortImage::ImageDimension; ++i )
    {
    expectedIndex[i] = inputIndex[i] - lowerBound[i];
    expectedSize[i]  = inputSize[i] +
      static_cast<ShortImage::IndexValueType>( lowerBound[i] + upperBound[i] );
    }

  if ( outputIndex != expectedIndex )
    {
    std::cerr << "[FAILED]" << std::endl;
    std::cerr << "Expected output index to be " << expectedIndex << ", got "
              << outputIndex << std::endl;
    return false;
    }

  if ( outputSize != expectedSize )
    {
    std::cerr << "[FAILED]" << std::endl;
    std::cerr << "Expected output size to be " << expectedSize << ", got "
              << outputSize << std::endl;
    return false;
    }

  std::cout << "[PASSED]" << std::endl;

  padFilter->UpdateLargestPossibleRegion();

  std::cout << "Verifying filter output pixels." << std::endl;
  if ( !VerifyFilterOutput( inputImage, padFilter->GetOutput() ) )
    {
    std::cerr << "[FAILED]" << std::endl;
    return false;
    }
  std::cout << "[PASSED]" << std::endl;

  // Create a streaming filter
  typedef itk::StreamingImageFilter< FloatImage, FloatImage > StreamingFilter;
  StreamingFilter::Pointer stream = StreamingFilter::New();
  stream->SetInput( padFilter->GetOutput() );
  stream->SetNumberOfStreamDivisions( 3 );
  stream->UpdateLargestPossibleRegion();

  std::cout << "Verifying streaming filter output pixels." << std::endl;

  if ( !VerifyFilterOutput( inputImage, stream->GetOutput() ) )
    {
    std::cerr << "[FAILED]" << std::endl;
    return false;
    }
  std::cout << "[PASSED]" << std::endl;

  return true;
}


int itkZeroFluxNeumannPadImageFilterTest( int, char* [] )
{
  // Test the creation of an image with native type
  ShortImage::Pointer inputImage = ShortImage::New();

  // Fill in a test image
  ShortImage::IndexType  inputIndex = {{0, 0}};
  ShortImage::SizeType   inputSize  = {{8, 12}};
  ShortImage::RegionType inputRegion;
  inputRegion.SetSize( inputSize );
  inputRegion.SetIndex( inputIndex );
  inputImage->SetLargestPossibleRegion( inputRegion );
  inputImage->SetBufferedRegion( inputRegion );
  inputImage->Allocate();

  itk::ImageRegionIterator< ShortImage > iterator( inputImage, inputRegion );

  short i = 0;
  for (; !iterator.IsAtEnd(); ++iterator, ++i)
    {
      iterator.Set( i );
    }

  // Create a filter
  FilterType::Pointer padFilter = FilterType::New();
  padFilter->SetInput( inputImage );

  //FilterWatcher watcher( padFilter );

  // CASE 0
  // Verify that the input is the same as the output.
  std::cout << "Case 0" << std::endl;
  SizeValueType lowerBound[2] = { 0, 0 };
  SizeValueType upperBound[2] = { 0, 0 };
  if ( !VerifyFilter( inputImage, padFilter, lowerBound, upperBound ) )
    {
    return EXIT_FAILURE;
    }

  // CASE 1
  std::cout << "Case 1" << std::endl;
  lowerBound[0] = 3; lowerBound[1] = 7;
  upperBound[0] = 5; upperBound[1] = 9;
  if ( !VerifyFilter( inputImage, padFilter, lowerBound, upperBound ) )
    {
    return EXIT_FAILURE;
    }

  // CASE 2
  std::cout << "Case 2" << std::endl;
  lowerBound[0] = 10;
  upperBound[1] = 15;
  if ( !VerifyFilter( inputImage, padFilter, lowerBound, upperBound ) )
    {
    return EXIT_FAILURE;
    }

  // CASE 3
  std::cout << "Case 3" << std::endl;
  lowerBound[1] = 16;
  upperBound[0] = 9;
  if ( !VerifyFilter( inputImage, padFilter, lowerBound, upperBound ) )
    {
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
