/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCentralDifferenceImageFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkCentralDifferenceImageFunction_txx
#define _itkCentralDifferenceImageFunction_txx
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
double
CentralDifferenceImageFunction<TInputImage,TCoordRep>
::EvaluateAtIndex(
  const IndexType& index,
  unsigned int dim ) const
{
  
  double derivative = 0.0;

  if( dim > ImageDimension - 1 )
    {
    return ( derivative );
    }
  
  IndexType neighIndex = index;

  const typename InputImageType::SizeType& size =
    m_Image->GetBufferedRegion().GetSize();
  const typename InputImageType::IndexType& start =
    m_Image->GetBufferedRegion().GetIndex();

  // bounds checking
  if( index[dim] < static_cast<long>(start[dim]) + 1 ||
      index[dim] > (start[dim] + static_cast<long>(size[dim]) - 2 ) )
    {
    return ( derivative );
    }
  
  // compute derivative
  neighIndex[dim] += 1;
  derivative = m_Image->GetPixel( neighIndex );

  neighIndex[dim] -= 2;
  derivative -= m_Image->GetPixel( neighIndex );

  derivative *= 0.5 / m_Image->GetSpacing()[dim];


  return ( derivative );

}


} // namespace itk

#endif
