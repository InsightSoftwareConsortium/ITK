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
template <class TInputImage, class TCoordRep>
CentralDifferenceImageFunction<TInputImage,TCoordRep>
::CentralDifferenceImageFunction()
{
}


/**
 *
 */
template <class TInputImage, class TCoordRep>
void
CentralDifferenceImageFunction<TInputImage,TCoordRep>
::PrintSelf(std::ostream& os, Indent indent) const
{
  this->Superclass::PrintSelf(os,indent);
}


/**
 *
 */
template <class TInputImage, class TCoordRep>
typename CentralDifferenceImageFunction<TInputImage,TCoordRep>::OutputType
CentralDifferenceImageFunction<TInputImage,TCoordRep>
::EvaluateAtIndex( const IndexType& index ) const
{
  
  OutputType derivative;
  derivative.Fill( 0.0 );
  
  IndexType neighIndex = index;

  const typename InputImageType::SizeType& size =
    this->GetInputImage()->GetBufferedRegion().GetSize();
  const typename InputImageType::IndexType& start =
    this->GetInputImage()->GetBufferedRegion().GetIndex();

  for ( unsigned int dim = 0; dim < TInputImage::ImageDimension; dim++ )
    {
    // bounds checking
    if( index[dim] < static_cast<long>(start[dim]) + 1 ||
        index[dim] > (start[dim] + static_cast<long>(size[dim]) - 2 ) )
      {
      derivative[dim] = 0.0;
      continue;
      }
    
    // compute derivative
    neighIndex[dim] += 1;
    derivative[dim] = this->GetInputImage()->GetPixel( neighIndex );

    neighIndex[dim] -= 2;
    derivative[dim] -= this->GetInputImage()->GetPixel( neighIndex );

    derivative[dim] *= 0.5 / this->GetInputImage()->GetSpacing()[dim];
    neighIndex[dim] += 1;
  }

  return ( derivative );

}


} // end namespace itk

#endif
