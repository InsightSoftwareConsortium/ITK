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
#ifndef itkMedianImageFunction_hxx
#define itkMedianImageFunction_hxx

#include "itkMedianImageFunction.h"
#include "itkConstNeighborhoodIterator.h"

#include <vector>
#include <algorithm>

namespace itk
{
/**
 * Constructor
 */
template< typename TInputImage, typename TCoordRep >
MedianImageFunction< TInputImage, TCoordRep >
::MedianImageFunction() : m_NeighborhoodRadius(1)
{
}


/**
 *
 */
template< typename TInputImage, typename TCoordRep >
void
MedianImageFunction< TInputImage, TCoordRep >
::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);
  os << indent << "NeighborhoodRadius: "  << m_NeighborhoodRadius << std::endl;
}

/**
 *
 */
template< typename TInputImage, typename TCoordRep >
typename MedianImageFunction< TInputImage, TCoordRep >
::OutputType
MedianImageFunction< TInputImage, TCoordRep >
::EvaluateAtIndex(const IndexType & index) const
{
  unsigned int i;

  if ( !this->GetInputImage() )
    {
    return ( NumericTraits< OutputType >::max() );
    }

  if ( !this->IsInsideBuffer(index) )
    {
    return ( NumericTraits< OutputType >::max() );
    }

  // Create an N-d neighborhood kernel, using a zeroflux boundary condition
  typename InputImageType::SizeType kernelSize;
  kernelSize.Fill( m_NeighborhoodRadius );

  ConstNeighborhoodIterator< InputImageType >
  it( kernelSize, this->GetInputImage(), this->GetInputImage()->GetBufferedRegion() );

  // Set the iterator at the desired location
  it.SetLocation(index);

  // We have to copy the pixels so we can run std::nth_element.
  std::vector< InputPixelType > pixels;
  typename std::vector< InputPixelType >::iterator medianIterator;

  // Walk the neighborhood
  for ( i = 0; i < it.Size(); ++i )
    {
    pixels.push_back( it.GetPixel(i) );
    }

  // Get the median value
  unsigned int medianPosition = it.Size() / 2;
  medianIterator = pixels.begin() + medianPosition;
  std::nth_element( pixels.begin(), medianIterator, pixels.end() );

  return ( *medianIterator );
}
} // namespace itk

#endif
