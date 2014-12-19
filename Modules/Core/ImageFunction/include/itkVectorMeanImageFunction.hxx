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
#ifndef itkVectorMeanImageFunction_hxx
#define itkVectorMeanImageFunction_hxx

#include "itkVectorMeanImageFunction.h"
#include "itkConstNeighborhoodIterator.h"

namespace itk
{
/**
 * Constructor
 */
template< typename TInputImage, typename TCoordRep >
VectorMeanImageFunction< TInputImage, TCoordRep >
::VectorMeanImageFunction()
{
  m_NeighborhoodRadius = 1;
}

/**
 *
 */
template< typename TInputImage, typename TCoordRep >
void
VectorMeanImageFunction< TInputImage, TCoordRep >
::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);
  os << indent << "NeighborhoodRadius: "  << m_NeighborhoodRadius << std::endl;
}

/**
 *
 */
template< typename TInputImage, typename TCoordRep >
typename VectorMeanImageFunction< TInputImage, TCoordRep >
::RealType
VectorMeanImageFunction< TInputImage, TCoordRep >
::EvaluateAtIndex(const IndexType & index) const
{

  typedef  typename TInputImage::PixelType                        PixelType;
  typedef  typename PixelType::ValueType                          PixelComponentType;
  typedef  typename NumericTraits< PixelComponentType >::RealType PixelComponentRealType;

  if ( !this->GetInputImage() || !this->IsInsideBuffer(index) )
    {
    RealType sum;
    sum.Fill( NumericTraits< PixelComponentRealType >::max() );
    return sum;
    }

  // Create an N-d neighborhood kernel, using a zeroflux boundary condition
  typename InputImageType::SizeType kernelSize;
  kernelSize.Fill(m_NeighborhoodRadius);

  ConstNeighborhoodIterator< InputImageType >
  it( kernelSize, this->GetInputImage(), this->GetInputImage()->GetBufferedRegion() );

  // Set the iterator at the desired location
  it.SetLocation(index);

  RealType sum;

  // Walk the neighborhood
  const unsigned int size = it.Size();
  for ( unsigned int i = 0; i < size; ++i )
    {
    PixelType p = it.GetPixel(i);
    const unsigned int VectorDimension = NumericTraits<PixelType>::GetLength( p );

    if ( i == 0 )
      {
      sum = static_cast< RealType >( NumericTraits<PixelType>::ZeroValue( p ) );
      }

    for ( unsigned int dim = 0; dim < VectorDimension; dim++ )
      {
      sum[dim] += static_cast< PixelComponentRealType >( p[dim] );
      }
    }

  const unsigned int VectorDimension = NumericTraits<RealType>::GetLength( sum );
  for ( unsigned int dim = 0; dim < VectorDimension; dim++ )
    {
    sum[dim] /= double( it.Size() );
    }

  return ( sum );
}
} // end namespace itk

#endif
