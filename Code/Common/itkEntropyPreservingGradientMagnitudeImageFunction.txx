/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkEntropyPreservingGradientMagnitudeImageFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkEntropyPreservingGradientMagnitudeImageFunction_txx
#define _itkEntropyPreservingGradientMagnitudeImageFunction_txx

#include "itkEntropyPreservingGradientMagnitudeImageFunction.h"
#include "vnl/vnl_math.h"

namespace itk
{

/**
 *
 */
template <class TInputImage, class TCoordRep>
void
EntropyPreservingGradientMagnitudeImageFunction<TInputImage,TCoordRep>
::SetInputImage( const InputImageType * ptr )
{
  if( !ptr ) return;

  this->Superclass::SetInputImage( ptr );

  typename TInputImage::SizeType size =
    this->GetInputImage()->GetLargestPossibleRegion().GetSize();
  
  m_ImageSizeOK = true;
  for( unsigned int j = 0; j < ImageDimension; j++ )
    {
    m_ImageSize[j] = (signed long) size[j];
    if( m_ImageSize[j] < 3 )
      {
      itkWarningMacro ( << " EntropyPreservingGradientMagnitudeImageFunction requires all dimensions have size greater than 3" );
      m_ImageSizeOK = false;
      }
    }

  m_Speed = 1.0;

}


/**
 *
 */
template <class TInputImage, class TCoordRep>
void
EntropyPreservingGradientMagnitudeImageFunction<TInputImage,TCoordRep>
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
EntropyPreservingGradientMagnitudeImageFunction<TInputImage,TCoordRep>
::EvaluateAtIndex(
const IndexType& index ) const
{

  if( !m_Image )
    {
    throw ExceptionObject(__FILE__, __LINE__);
    }

  if( !m_ImageSizeOK )
    {
    return 0.0;
    }

  m_Magnitude = 0.0;
  m_NeighIndex = index ;

  InputImageConstPointer image = this->GetInputImage();

  m_CenterValue = (double) image->GetPixel( index );
  
  for( unsigned int j = 0; j < ImageDimension; j++ )
    {
    // calculate backward difference
    if( index[j] > 0 )
      {
      m_NeighIndex[j] = index[j] - 1;
      m_DiffValue = m_CenterValue - (double) image->GetPixel( m_NeighIndex );

      if( ( m_Speed > 0 && m_DiffValue > 0 ) ||
          ( m_Speed < 0 && m_DiffValue < 0 ) )
        {
        m_Magnitude += m_DiffValue * m_DiffValue;
        }

      }

    // calculate forward difference
    if( index[j] < m_ImageSize[j] - 1 )
      {
      m_NeighIndex[j] = index[j] + 1;
      m_DiffValue = (double) image->GetPixel( m_NeighIndex ) - m_CenterValue;

      if( ( m_Speed > 0 && m_DiffValue < 0 ) ||
          ( m_Speed < 0 && m_DiffValue > 0 ) )
        {
        m_Magnitude += m_DiffValue * m_DiffValue;
        }

     }

    // reset neigh index
    m_NeighIndex[j] = index[j];

    }

  m_Magnitude = vnl_math_sqrt( m_Magnitude );

  return ( m_Magnitude );

}


} // namespace itk

#endif
