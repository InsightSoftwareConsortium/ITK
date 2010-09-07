/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorCentralDifferenceImageFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkVectorCentralDifferenceImageFunction_txx
#define __itkVectorCentralDifferenceImageFunction_txx

#include "itkVectorCentralDifferenceImageFunction.h"

namespace itk
{
/**
 * Constructor
 */
template< class TInputImage, class TCoordRep >
VectorCentralDifferenceImageFunction< TInputImage, TCoordRep >
::VectorCentralDifferenceImageFunction()
{
  this->m_UseImageDirection = true;
}

/**
 *
 */
template< class TInputImage, class TCoordRep >
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
template< class TInputImage, class TCoordRep >
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
    if ( index[dim] < static_cast< long >( start[dim] ) + 1
         || index[dim] > ( start[dim] + static_cast< long >( size[dim] ) - 2 ) )
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
