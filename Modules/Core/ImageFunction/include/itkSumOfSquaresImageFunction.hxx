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
#ifndef itkSumOfSquaresImageFunction_hxx
#define itkSumOfSquaresImageFunction_hxx

#include "itkSumOfSquaresImageFunction.h"

#include "itkConstNeighborhoodIterator.h"

namespace itk
{

template< typename TInputImage, typename TCoordRep >
SumOfSquaresImageFunction< TInputImage, TCoordRep >
::SumOfSquaresImageFunction()
{
  this->m_NeighborhoodRadius = 1;
  this->m_NeighborhoodSize = 1;
}

template< typename TInputImage, typename TCoordRep >
typename SumOfSquaresImageFunction< TInputImage, TCoordRep >
::RealType
SumOfSquaresImageFunction< TInputImage, TCoordRep >
::EvaluateAtIndex(const IndexType & index) const
{
  RealType sumOfSquares;

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
    sumOfSquares += value * value;
    }

  return ( sumOfSquares );
}

template< typename TInputImage, typename TCoordRep >
void
SumOfSquaresImageFunction< TInputImage, TCoordRep >
::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);

  os << indent << "NeighborhoodRadius: " << this->m_NeighborhoodRadius << std::endl;
  os << indent << "NeighborhoodSize: " << this->m_NeighborhoodSize << std::endl;
}

} // end namespace itk

#endif
