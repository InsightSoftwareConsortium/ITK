/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLinearInterpolateFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

==========================================================================*/
#include "itkPixelTraits.h"
#include "itkIndex.h"
#include "vnl/vnl_math.h"

namespace itk
{

/**
 *
 */
template<class TInputImage>
void
LinearInterpolateFunction<TInputImage>
::SetInputImage( InputImageType *ptr )
{
  this->Superclass::SetInputImage( ptr );

  m_ImageSize =
    this->GetInputImage()->GetLargestPossibleRegion().GetSize();

  m_Value = 0.0;

  m_Neighbors = vnl_math_rnd( ldexp(1.0,ImageDimension) );

}


/**
 *
 */
template<class TInputImage>
void
LinearInterpolateFunction<TInputImage>
::PrintSelf(std::ostream& os, Indent indent)
{
  this->Superclass::PrintSelf(os,indent);
  os << indent << "linearly interpolate image." << std::endl;
}


/**
 *
 */
template<class TInputImage>
double
LinearInterpolateFunction<TInputImage>
::Evaluate(
const IndexType& index )
{
  m_Value = 0.0;
  double dblIndex[ImageDimension];
  bool inRange = true;

  for( int j = 0; j < ImageDimension; j++ )
    {
    if( index[j] < 0 || index[j] >= m_ImageSize[j] - 1 )
      {
      dblIndex[j] = (double) index[j];
      inRange = false;
      }
    }

  if( inRange )
    {
    m_Value = (double) ScalarTraits<PixelType>::
      GetScalar( this->GetInputImage()->GetPixel( index ) );
    return ( m_Value );
    }
  else
    {
    return ( this->Evaluate( dblIndex ) );
    }

}


/**
 *
 */
template<class TInputImage>
double
LinearInterpolateFunction<TInputImage>
::Evaluate(
double * dblIndex )
{

  for( int j = 0; j < ImageDimension; j++ )
    {
    m_Lower[j] = floor( dblIndex[j] );
    if( m_Lower[j] < 0 || m_Lower[j] >= m_ImageSize[j] )
      {
      m_Lower[j] = -1.0;
      }

    m_Upper[j] = ceil( dblIndex[j] );
    if( m_Upper[j] < 0 || m_Upper[j] >= m_ImageSize[j] )
      {
      m_Upper[j] = -1.0;
      }

    m_Distance[j] = dblIndex[j] - floor( dblIndex[j] );

    }

  IndexType neighIndex;

  m_Value = 0.0;

  typename InputImageType::Pointer image = this->GetInputImage();

  for( int counter = 0; counter < m_Neighbors; counter++)
    {
    int useUpper = counter;
    double neighValue = 1.0;

    // use bitfield in counter to determine which neighbor
    // to use
    for( int j = 0; j < ImageDimension; j++ )
      {
      if( useUpper & 1 )
        {
        if( m_Upper[j] < 0 )
          {
          neighValue = 0.0;
          break;
          }

        neighIndex[j] = (long) m_Upper[j];
        neighValue *= m_Distance[j];
        }
      else
        {
        if( m_Lower[j] < 0 )
          {
          neighValue = 0.0;
          }

        neighIndex[j] = (long) m_Lower[j];
        neighValue *= 1.0 - m_Distance[j];

        }

      if ( !neighValue )
        {
        break;
        }

      useUpper >>= 1;

      }

    if( neighValue )
      {
      neighValue *= (double) ScalarTraits<PixelType>::
        GetScalar( image->GetPixel( neighIndex ) );
      m_Value += neighValue;

      }

    }

  return ( m_Value );

}


} // namespace itk