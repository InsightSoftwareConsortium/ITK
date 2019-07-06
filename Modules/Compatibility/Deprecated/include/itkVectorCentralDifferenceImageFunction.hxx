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
#ifndef itkVectorCentralDifferenceImageFunction_hxx
#define itkVectorCentralDifferenceImageFunction_hxx

#include "itkVectorCentralDifferenceImageFunction.h"

namespace itk
{
/**
 * Constructor
 */
template< typename TInputImage, typename TCoordRep >
VectorCentralDifferenceImageFunction< TInputImage, TCoordRep >
::VectorCentralDifferenceImageFunction()
{
  this->m_UseImageDirection = true;
}

/**
 *
 */
template< typename TInputImage, typename TCoordRep >
void
VectorCentralDifferenceImageFunction< TInputImage, TCoordRep >
::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);
  os << indent << "UseImageDirection = " << this->m_UseImageDirection << std::endl;
}

/**
 *
 */
template< typename TInputImage, typename TCoordRep >
typename VectorCentralDifferenceImageFunction< TInputImage, TCoordRep >::OutputType
VectorCentralDifferenceImageFunction< TInputImage, TCoordRep >
::EvaluateAtIndex(const IndexType & index) const
{
  OutputType derivative;

  derivative.Fill(0.0);

  IndexType neighIndex = index;

  const typename InputImageType::SizeType & size =
    this->GetInputImage()->GetBufferedRegion().GetSize();
  const typename InputImageType::IndexType & start =
    this->GetInputImage()->GetBufferedRegion().GetIndex();

  for ( unsigned int dim = 0; dim < TInputImage::ImageDimension; dim++ )
    {
    // bounds checking
    if ( index[dim] < start[dim] + 1
         || index[dim] > ( start[dim] + static_cast< OffsetValueType >( size[dim] ) - 2 ) )
      {
      for ( unsigned int vdim = 0; vdim < Dimension; ++vdim )
        {
        derivative(vdim, dim) = 0.0;
        }
      continue;
      }

    // compute derivative
    const double deriv_weight = 0.5 / this->GetInputImage()->GetSpacing()[dim];

    neighIndex[dim] += 1;
    const InputPixelType & pixf = this->GetInputImage()->GetPixel(neighIndex);
    for ( unsigned int vdim = 0; vdim < Dimension; ++vdim )
      {
      derivative(vdim, dim) = pixf[vdim];
      }

    neighIndex[dim] -= 2;
    const InputPixelType & pixb = this->GetInputImage()->GetPixel(neighIndex);
    for ( unsigned int vdim = 0; vdim < Dimension; ++vdim )
      {
      derivative(vdim, dim) -= pixb[vdim];
      derivative(vdim, dim) *= deriv_weight;
      }

    neighIndex[dim] += 1;
    }

  if ( this->m_UseImageDirection )
    {
    OutputType orientedderivative;
    const typename InputImageType::DirectionType & direction =
      this->GetInputImage()->GetDirection();
    for ( unsigned int i = 0; i < TInputImage::ImageDimension; ++i )
      {
      std::vector< double > sums(Dimension, 0.0);
      for ( unsigned int j = 0; j < TInputImage::ImageDimension; ++j )
        {
        for ( unsigned int vdim = 0; vdim < Dimension; ++vdim )
          {
          sums[vdim] += direction[i][j] * derivative(vdim, j);
          }
        }
      for ( unsigned int vdim = 0; vdim < Dimension; ++vdim )
        {
        orientedderivative(vdim, i) = static_cast< TCoordRep >( sums[vdim] );
        }
      }
    return orientedderivative;
    }

  return ( derivative );
}
} // end namespace itk

#endif
