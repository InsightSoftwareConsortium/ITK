/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCentralDifferenceImageFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkCentralDifferenceImageFunction_txx
#define __itkCentralDifferenceImageFunction_txx

#include "itkCentralDifferenceImageFunction.h"

namespace itk
{
/**
 * Constructor
 */
template< class TInputImage, class TCoordRep >
CentralDifferenceImageFunction< TInputImage, TCoordRep >
::CentralDifferenceImageFunction()
{
  this->m_UseImageDirection = true;
}

/**
 *
 */
template< class TInputImage, class TCoordRep >
void
CentralDifferenceImageFunction< TInputImage, TCoordRep >
::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);
  os << indent << "UseImageDirection = " << this->m_UseImageDirection << std::endl;
}

/**
 *
 */
template< class TInputImage, class TCoordRep >
typename CentralDifferenceImageFunction< TInputImage, TCoordRep >::OutputType
CentralDifferenceImageFunction< TInputImage, TCoordRep >
::EvaluateAtIndex(const IndexType & index) const
{
  OutputType derivative;

  derivative.Fill(0.0);

  IndexType neighIndex = index;

  const InputImageType *inputImage = this->GetInputImage();

  const typename InputImageType::RegionType region =
    inputImage->GetBufferedRegion();

  const typename InputImageType::SizeType & size   = region.GetSize();
  const typename InputImageType::IndexType & start = region.GetIndex();

  const unsigned int MaxDims = Self::ImageDimension;
  for ( unsigned int dim = 0; dim < MaxDims; dim++ )
    {
    // bounds checking
    if ( index[dim] < static_cast< long >( start[dim] ) + 1
         || index[dim] > ( start[dim] + static_cast< long >( size[dim] ) - 2 ) )
      {
      derivative[dim] = 0.0;
      continue;
      }

    // compute derivative
    neighIndex[dim] += 1;
    derivative[dim] = inputImage->GetPixel(neighIndex);

    neighIndex[dim] -= 2;
    derivative[dim] -= inputImage->GetPixel(neighIndex);

    derivative[dim] *= 0.5 / inputImage->GetSpacing()[dim];
    neighIndex[dim] += 1;
    }

  if ( this->m_UseImageDirection )
    {
    OutputType orientedDerivative;
    inputImage->TransformLocalVectorToPhysicalVector(derivative, orientedDerivative);
    return orientedDerivative;
    }

  return ( derivative );
}
} // end namespace itk

#endif
