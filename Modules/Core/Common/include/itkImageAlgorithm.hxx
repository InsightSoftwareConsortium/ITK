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
#ifndef __itkImageAlgorithm_hxx
#define __itkImageAlgorithm_hxx

#include "itkImageAlgorithm.h"

#include "itkImageRegionIterator.h"

namespace itk
{

template<typename InputImageType, typename OutputImageType >
void ImageAlgorithm::DispatchedCopy( const InputImageType *inImage, OutputImageType *outImage,
                                     const typename InputImageType::RegionType &inRegion,
                                     const typename OutputImageType::RegionType &outRegion,
                                     FalseType )
{
  itk::ImageRegionConstIterator<InputImageType> it( inImage, inRegion );
  itk::ImageRegionIterator<OutputImageType> ot( outImage, outRegion );

  while( !it.IsAtEnd() )
    {
    ot.Set( static_cast< typename OutputImageType::PixelType >( it.Get() ) );
    ++ot;
    ++it;
    }
}


template<typename TImageType >
void ImageAlgorithm::DispatchedCopy( const TImageType *inImage, TImageType *outImage,
                                     const typename TImageType::RegionType &inRegion,
                                     const typename TImageType::RegionType &outRegion,
                                     TrueType )
{
  typedef typename TImageType::RegionType _RegionType;
  typedef typename TImageType::IndexType  _IndexType;


  // We wish to copy whole lines, otherwise just use the basic implementation.
  if ( inRegion.GetSize()[0] != outRegion.GetSize()[0] )
    {
    ImageAlgorithm::DispatchedCopy<TImageType, TImageType>( inImage, outImage, inRegion, outRegion );
    return;
    }

  // Get the number of bytes of each pixel in the buffer.
  size_t PixelSize = ImageAlgorithm::PixelSize<TImageType>::Get( inImage );

  const void *in = inImage->GetBufferPointer();
  void *out = outImage->GetBufferPointer();

  const _RegionType &inBufferedRegion = inImage->GetBufferedRegion();
  const _RegionType &outBufferedRegion = outImage->GetBufferedRegion();

  ImageRegionConstIterator<TImageType> it( inImage, inRegion );

  // Compute the number of continuous pixel which can be copied.
  size_t numberOfPixel = 1;
  unsigned int    movingDirection = 0;
  do
    {
    numberOfPixel *= inRegion.GetSize(movingDirection );
    ++movingDirection;
    }
  // The copy regions must extend to the full buffered region, to
  // ensure continuity of pixels between dimensions.
  while ( movingDirection < _RegionType::ImageDimension
          && inRegion.GetSize( movingDirection - 1 ) == inBufferedRegion.GetSize( movingDirection - 1 )
          && outRegion.GetSize( movingDirection - 1 ) == outBufferedRegion.GetSize( movingDirection - 1 )
          && inBufferedRegion.GetSize(movingDirection - 1) == outBufferedRegion.GetSize(movingDirection - 1) );

  const size_t sizeOfChunk = numberOfPixel*PixelSize;

  _IndexType inCurrentIndex = inRegion.GetIndex();
  _IndexType outCurrentIndex = outRegion.GetIndex();

  while ( inRegion.IsInside( inCurrentIndex ) )
    {
    size_t inOffset = 0; // in bytes
    size_t outOffset = 0;

    size_t inSubDimensionQuantity = 1; // in pixels
    size_t outSubDimensionQuantity = 1;

    for (unsigned int i = 0; i < _RegionType::ImageDimension; ++i )
      {
      inOffset += inSubDimensionQuantity*PixelSize*static_cast<size_t>( inCurrentIndex[i] - inBufferedRegion.GetIndex(i) );
      inSubDimensionQuantity *= inBufferedRegion.GetSize(i);

      outOffset += outSubDimensionQuantity*PixelSize*static_cast<size_t>( outCurrentIndex[i] - outBufferedRegion.GetIndex(i) );
      outSubDimensionQuantity *= outBufferedRegion.GetSize(i);
      }

    const char *inBuffer = static_cast<const char*>(in) + inOffset;
    char* outBuffer = static_cast<char*>(out) + outOffset;

    std::copy(inBuffer,
              inBuffer+sizeOfChunk ,
              outBuffer);

    if ( movingDirection == _RegionType::ImageDimension )
      {
      break;
      }

    // increment index to next chunk
    ++inCurrentIndex[movingDirection];
    for ( unsigned int i = movingDirection; i + 1 < _RegionType::ImageDimension; ++i )
      {
      // When reaching the end of the moving index in the copy region
      // dimension, carry to higher dimensions.
      if ( static_cast<SizeValueType>(inCurrentIndex[i]  - inRegion.GetIndex(i)) >= inRegion.GetSize(i) )
        {
        inCurrentIndex[i] = inRegion.GetIndex(i);
        ++inCurrentIndex[i + 1];
        }
      }

    // increment index to next chunk
    ++outCurrentIndex[movingDirection];
    for ( unsigned int i = movingDirection; i + 1 < _RegionType::ImageDimension; ++i )
      {
      if ( static_cast<SizeValueType>(outCurrentIndex[i]  - outRegion.GetIndex(i)) >= outRegion.GetSize(i) )
        {
        outCurrentIndex[i] = outRegion.GetIndex(i);
        ++outCurrentIndex[i + 1];
        }
      }

    }
}


} // end namespace itk

#endif
