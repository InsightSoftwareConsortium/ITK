/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageFunction_txx
#define _itkImageFunction_txx

#include "itkImageFunction.h"
#include "itkOffset.h"
#include "vnl/vnl_math.h"

namespace itk
{

/**
 * Constructor
 */
template <class TInputImage, class TOutput, class TCoordRep>
ImageFunction<TInputImage, TOutput, TCoordRep>
::ImageFunction()
{
  m_Image = NULL;
}


/**
 * Standard "PrintSelf" method
 */
template <class TInputImage, class TOutput, class TCoordRep>
void
ImageFunction<TInputImage, TOutput, TCoordRep>
::PrintSelf(
std::ostream& os, 
Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "InputImage: " << m_Image.GetPointer() << std::endl;

}


/**
 * Initialize by setting the input image
 */
template <class TInputImage, class TOutput, class TCoordRep>
void
ImageFunction<TInputImage, TOutput, TCoordRep>
::SetInputImage(
const InputImageType * ptr )
{

  // set the input image
  m_Image = ptr;

  if( m_Image )
    {
    m_Origin = m_Image->GetOrigin();
    m_Spacing = m_Image->GetSpacing();

    const typename InputImageType::SizeType & size =
      m_Image->GetBufferedRegion().GetSize();

    Offset<ImageDimension> offset;
    offset.Fill( -1 );
   
    m_BufferStart = m_Image->GetBufferedRegion().GetIndex();
    m_BufferEnd = m_BufferStart + size + offset;

    this->ConvertIndexToPoint( m_BufferStart, m_GeometricStart );
    this->ConvertIndexToPoint( m_BufferEnd, m_GeometricEnd );

    }
    
}


/**
 * Check if an index is inside the image buffer
 */
template <class TInputImage, class TOutput, class TCoordRep>
bool
ImageFunction<TInputImage, TOutput, TCoordRep>
::IsInsideBuffer(
const IndexType& index ) const
{
  for( unsigned int j = 0; j < ImageDimension; j++ )
    {
    if( index[j] < m_BufferStart[j] ||
        index[j] > m_BufferEnd[j] )
      {
      return false;
      }
    }
  return true;
}


/**
 * Check if a continuous index is inside the image buffer
 */
template <class TInputImage, class TOutput, class TCoordRep>
bool
ImageFunction<TInputImage, TOutput, TCoordRep>
::IsInsideBuffer(
const ContinuousIndexType& index ) const
{
  for( unsigned int j = 0; j < ImageDimension; j++ )
    {
    if( index[j] < (double) m_BufferStart[j] ||
        index[j] > (double) m_BufferEnd[j] )
      {
      return false;
      }
    }
  return true;
}


/**
 * Check if a point is inside the image buffer
 */
template <class TInputImage, class TOutput, class TCoordRep>
bool
ImageFunction<TInputImage, TOutput, TCoordRep>
::IsInsideBuffer(
const PointType& point ) const
{
  for( unsigned int j = 0; j < ImageDimension; j++ )
    {
    if( point[j] < m_GeometricStart[j] ||
        point[j] > m_GeometricEnd[j] )
      {
      return false;
      }
    }
  return true;
}


/**
 * Convert a point to a continuous index
 */
template <class TInputImage, class TOutput, class TCoordRep>
void
ImageFunction<TInputImage, TOutput, TCoordRep>
::ConvertPointToContinuousIndex(
const PointType& point,
ContinuousIndexType& index ) const
{
  for( unsigned int j = 0; j < ImageDimension; j++ )
    {
    index[j] = ( point[j] - m_Origin[j] ) / m_Spacing[j];
    }
}


/**
 * Convert a continuous index to a point
 */
template <class TInputImage, class TOutput, class TCoordRep>
void
ImageFunction<TInputImage, TOutput, TCoordRep>
::ConvertContinuousIndexToPoint(
const ContinuousIndexType& index,
PointType& point ) const
{
  for( unsigned int j = 0; j < ImageDimension; j++ )
    {
    point[j] = index[j] * m_Spacing[j] + m_Origin[j];
    }
}


/**
 * Convert a index to a point
 */
template <class TInputImage, class TOutput, class TCoordRep>
void
ImageFunction<TInputImage, TOutput, TCoordRep>
::ConvertIndexToPoint(
const IndexType& index,
PointType& point ) const
{
  for( unsigned int j = 0; j < ImageDimension; j++ )
    {
    point[j] = (double) index[j] * m_Spacing[j] + m_Origin[j];
    }
}


/**
 * Convert a point to a nearest index
 */
template <class TInputImage, class TOutput, class TCoordRep>
void
ImageFunction<TInputImage, TOutput, TCoordRep>
::ConvertPointToNearestIndex(
const PointType& point,
IndexType& index ) const
{
  for( unsigned int j = 0; j < ImageDimension; j++ )
    {
    index[j] = vnl_math_rnd(( point[j] - m_Origin[j] ) / m_Spacing[j]);
    }
}


/**
 * Convert a continuous index to a nearest index
 */
template <class TInputImage, class TOutput, class TCoordRep>
void
ImageFunction<TInputImage, TOutput, TCoordRep>
::ConvertContinuousIndexToNearestIndex(
const ContinuousIndexType& cindex,
IndexType& index ) const
{
  for( unsigned int j = 0; j < ImageDimension; j++ )
    {
    index[j] = vnl_math_rnd( cindex[j] );
    }
}


} // end namespace itk

#endif

