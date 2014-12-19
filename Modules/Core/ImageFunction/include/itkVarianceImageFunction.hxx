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
#ifndef itkVarianceImageFunction_hxx
#define itkVarianceImageFunction_hxx

#include "itkVarianceImageFunction.h"

#include "itkConstNeighborhoodIterator.h"

namespace itk
{
/**
 * Constructor
 */
template< typename TInputImage, typename TCoordRep >
VarianceImageFunction< TInputImage, TCoordRep >
::VarianceImageFunction()
{
  m_NeighborhoodRadius = 1;
}

/**
 *
 */
template< typename TInputImage, typename TCoordRep >
void
VarianceImageFunction< TInputImage, TCoordRep >
::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);
  os << indent << "NeighborhoodRadius: "  << m_NeighborhoodRadius << std::endl;
}

/**
 *
 */
template< typename TInputImage, typename TCoordRep >
typename VarianceImageFunction< TInputImage, TCoordRep >
::RealType
VarianceImageFunction< TInputImage, TCoordRep >
::EvaluateAtIndex(const IndexType & index) const
{
  RealType sum;
  RealType sumOfSquares;
  RealType var;

  sum = NumericTraits< RealType >::ZeroValue();
  sumOfSquares = NumericTraits< RealType >::ZeroValue();

  if ( !this->GetInputImage() )
    {
    return ( NumericTraits< RealType >::max() );
    }

  if ( !this->IsInsideBuffer(index) )
    {
    return ( NumericTraits< RealType >::max() );
    }

  // Create an N-d neighborhood kernel, using a zeroflux boundary condition
  typename InputImageType::SizeType kernelSize;
  kernelSize.Fill(m_NeighborhoodRadius);

  ConstNeighborhoodIterator< InputImageType >
  it( kernelSize, this->GetInputImage(), this->GetInputImage()->GetBufferedRegion() );

  // Set the iterator at the desired location
  it.SetLocation(index);

  // Walk the neighborhood
  const unsigned int size = it.Size();
  for ( unsigned int i = 0; i < size; ++i )
    {
    const RealType value = static_cast< RealType >( it.GetPixel(i) );
    sum += value;
    sumOfSquares += value * value;
    }

  const double num = static_cast< double >( size );
  var = ( sumOfSquares - ( sum * sum / num ) ) / ( num - 1.0 );

  return ( var );
}
} // end namespace itk

#endif
