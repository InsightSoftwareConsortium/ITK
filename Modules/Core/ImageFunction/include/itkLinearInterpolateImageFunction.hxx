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
#ifndef itkLinearInterpolateImageFunction_hxx
#define itkLinearInterpolateImageFunction_hxx

#include "itkConceptChecking.h"
#include "itkLinearInterpolateImageFunction.h"

#include "itkMath.h"

namespace itk
{

// Define the number of neighbors
template< typename TInputImage, typename TCoordRep >
const unsigned long
LinearInterpolateImageFunction< TInputImage, TCoordRep >
::m_Neighbors = 1 << TInputImage::ImageDimension;


template< typename TInputImage, typename TCoordRep >
LinearInterpolateImageFunction< TInputImage, TCoordRep >
::LinearInterpolateImageFunction()
{}

template< typename TInputImage, typename TCoordRep >
LinearInterpolateImageFunction< TInputImage, TCoordRep >
::~LinearInterpolateImageFunction()
{}

template< typename TInputImage, typename TCoordRep >
typename LinearInterpolateImageFunction< TInputImage, TCoordRep >
::OutputType
LinearInterpolateImageFunction< TInputImage, TCoordRep >
::EvaluateUnoptimized(const ContinuousIndexType & index) const
{
  // Avoid the smartpointer de-reference in the loop for
  // "return m_InputImage.GetPointer()"
  const TInputImage * const inputImagePtr = this->GetInputImage();

  // Compute base index = closest index below point
  // Compute distance from point to base index

  IndexType baseIndex;
  InternalComputationType    distance[ImageDimension];
  for ( unsigned int dim = 0; dim < ImageDimension; ++dim )
    {
    baseIndex[dim] = Math::Floor< IndexValueType >(index[dim]);
    distance[dim] = index[dim] - static_cast< InternalComputationType >( baseIndex[dim] );
    }

  // The iInterpolated value is the weighted sum of each of the surrounding
  // neighbors. The weight for each neighbor is the fraction overlap
  // of the neighbor pixel with respect to a pixel centered on point.

  // When RealType is VariableLengthVector, 'value' will be resized properly
  // below when it's assigned again.
  Concept::Detail::UniqueType< typename NumericTraits< RealType >::ScalarRealType >();

  RealType value;
  // Initialize variable "value" with overloaded function so that
  // in the case of variable length vectors the "value" is initialized
  // to all zeros of length equal to the InputImagePtr first pixel length.
  this->MakeZeroInitializer( inputImagePtr, value );

  Concept::Detail::UniqueType< typename NumericTraits< InputPixelType >::ScalarRealType >();

  for ( unsigned int counter = 0; counter < m_Neighbors; ++counter )
    {
    InternalComputationType overlap = 1.0;    // Fraction overlap
    unsigned int upper = counter;  // Each bit indicates upper/lower neighbour
    IndexType    neighIndex( baseIndex );

    // Get neighbor index and overlap fraction
    for ( unsigned int dim = 0; dim < ImageDimension; ++dim )
      {
      if ( upper & 1 )
        {
        ++(neighIndex[dim]);
        // Take care of the case where the pixel is just
        // in the outer upper boundary of the image grid.
        if ( neighIndex[dim] > this->m_EndIndex[dim] )
          {
          neighIndex[dim] = this->m_EndIndex[dim];
          }
        overlap *= distance[dim];
        }
      else
        {
        // Take care of the case where the pixel is just
        // in the outer lower boundary of the image grid.
        if ( neighIndex[dim] < this->m_StartIndex[dim] )
          {
          neighIndex[dim] = this->m_StartIndex[dim];
          }
        overlap *= 1.0 - distance[dim];
        }

      upper >>= 1;
      }
      value += static_cast< RealType >( inputImagePtr->GetPixel(neighIndex) ) * overlap;
    }

  return ( static_cast< OutputType >( value ) );
}

template< typename TInputImage, typename TCoordRep >
void
LinearInterpolateImageFunction< TInputImage, TCoordRep >
::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);
}
} // end namespace itk

#endif
