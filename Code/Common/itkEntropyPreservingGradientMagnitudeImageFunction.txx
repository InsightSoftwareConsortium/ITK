/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkEntropyPreservingGradientMagnitudeImageFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/
#ifndef _itkEntropyPreservingGradientMagnitudeImageFunction_txx
#define _itkEntropyPreservingGradientMagnitudeImageFunction_txx
#include "vnl/vnl_math.h"

namespace itk
{

/**
 *
 */
template <class TInputImage>
void
EntropyPreservingGradientMagnitudeImageFunction<TInputImage>
::SetInputImage( InputImageType * ptr )
{
  if( !ptr ) return;

  this->Superclass::SetInputImage( ptr );

  m_ImageSize = 
    this->GetInputImage()->GetLargestPossibleRegion().GetSize();
  
  m_ImageSizeOK = true;
  for( int j = 0; j < ImageDimension; j++ )
    {
    if( m_ImageSize[j] < 3 )
      {
      m_ImageSizeOK = false;
      }
    }

  m_Speed = 1.0;

}


/**
 *
 */
template<class TInputImage>
void
EntropyPreservingGradientMagnitudeImageFunction<TInputImage>
::PrintSelf(std::ostream& os, Indent indent)
{
  this->Superclass::PrintSelf(os,indent);
  os << indent << "calculate entropy satisfying derivative:" << std::endl;
  os << indent << "speed: " << m_Speed << std::endl;
}


/**
 *
 */
template <class TInputImage>
double
EntropyPreservingGradientMagnitudeImageFunction<TInputImage>
::Evaluate(
const IndexType& index )
{

  if( !m_Image )
    {
    throw ExceptionObject();
    }

  if( !m_ImageSizeOK )
    {
    return 0.0;
    }

  m_Magnitude = 0.0;
  m_NeighIndex = index ;

  typename InputImageType::Pointer image = this->GetInputImage();

  m_CenterValue = (double) 
    ScalarTraits<PixelType>::GetScalar( image->GetPixel( index ) );
  
  for( int j = 0; j < ImageDimension; j++ )
    {
    // calculate backward difference
    if( index[j] > 0 )
      {
      m_NeighIndex[j] = index[j] - 1;
      m_DiffValue = m_CenterValue - (double) 
        ScalarTraits<PixelType>::GetScalar( image->GetPixel( m_NeighIndex ) );

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
      m_DiffValue = (double) 
        ScalarTraits<PixelType>::GetScalar(image->GetPixel( m_NeighIndex )) 
          - m_CenterValue;

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
