/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkUpwindDerivativeImageFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkUpwindDerivativeImageFunction_txx
#define _itkUpwindDerivativeImageFunction_txx
#include "itkUpwindDerivativeImageFunction.h"

namespace itk
{

/**
 *
 */
template <class TInputImage, class TCoordRep>
void
UpwindDerivativeImageFunction<TInputImage,TCoordRep>
::SetInputImage( const InputImageType * ptr )
{
  this->Superclass::SetInputImage( ptr );

  typename TInputImage::SizeType size =
    this->GetInputImage()->GetLargestPossibleRegion().GetSize();

  for( unsigned int j = 0; j < ImageDimension; j++ )
    {
    m_ImageSize[j] = (signed long) size[j];
    }  
  m_Speed = 1.0;

}


/**
 *
 */
template <class TInputImage, class TCoordRep>
void
UpwindDerivativeImageFunction<TInputImage,TCoordRep>
::PrintSelf(std::ostream& os, Indent indent) const
{
  this->Superclass::PrintSelf(os,indent);
  os << indent << "Speed: " << m_Speed << std::endl;
}


/**
 *
 */
template <class TInputImage, class TCoordRep>
double
UpwindDerivativeImageFunction<TInputImage,TCoordRep>
::EvaluateNthDerivativeAtIndex(
const IndexType& index,
unsigned int dim ) const
{
  
  if( !m_Image || dim > ImageDimension - 1 )
    {
    return 0.0;
    }

  m_Derivative = 0.0;
  m_NeighIndex = index ;

  m_CenterValue = (double) m_Image->GetPixel( index );
  
  // calculate backward difference
  if( m_Speed > 0 && index[dim] > 0 )
    {
    m_NeighIndex[dim] = index[dim] - 1;
    m_Derivative = m_CenterValue - (double) m_Image->GetPixel( m_NeighIndex );
    }

  // calculate forward difference
  if( m_Speed <= 0 && index[dim] < m_ImageSize[dim] - 1 )
    {
    m_NeighIndex[dim] = index[dim] + 1;
    m_Derivative = (double) m_Image->GetPixel( m_NeighIndex ) - m_CenterValue;
    }

  return ( m_Derivative );

}


} // namespace itk

#endif
