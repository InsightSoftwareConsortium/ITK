/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkReflectiveImageRegionConstIterator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkReflectiveImageRegionConstIterator_txx
#define _itkReflectiveImageRegionConstIterator_txx


#include "itkReflectiveImageRegionConstIterator.h"

namespace itk
{

template<class TImage>
ReflectiveImageRegionConstIterator<TImage>
::ReflectiveImageRegionConstIterator() : ImageConstIteratorWithIndex<TImage>()
{
  int i;
  m_BeginOffset.Fill( 0 );
  m_EndOffset.Fill( 0 );
  this->GoToBegin();
}



template<class TImage>
ReflectiveImageRegionConstIterator<TImage>
::ReflectiveImageRegionConstIterator(TImage *ptr, const RegionType& region) :
  ImageConstIteratorWithIndex<TImage>(ptr, region)
{
  m_BeginOffset.Fill( 0 );
  m_EndOffset.Fill( 0 );
  this->GoToBegin();
}

template<class TImage>
ReflectiveImageRegionConstIterator<TImage>
::ReflectiveImageRegionConstIterator( const Self& it )
{
  this->Operator=( it );
  this->GoToBegin();
}


template<class TImage>
ReflectiveImageRegionConstIterator<TImage>
::ReflectiveImageRegionConstIterator(
  const ImageConstIteratorWithIndex<TImage> &it)
{
  this->ImageConstIteratorWithIndex<TImage>::operator=(it); 
  m_BeginOffset.Fill(0);
  m_EndOffset.Fill(0);
}

template<class TImage>
ReflectiveImageRegionConstIterator<TImage> &
ReflectiveImageRegionConstIterator<TImage>
::operator=(const Self& it)
{
  this->ImageConstIteratorWithIndex<TImage>::operator=(it);
  m_BeginOffset = it.m_BeginOffset;
  m_EndOffset = it.m_EndOffset;
  return *this;
}

template<class TImage>
void
ReflectiveImageRegionConstIterator<TImage>
::GoToBegin( void ) 
{
  m_PositionIndex = m_BeginIndex + m_BeginOffset;
  m_Position = m_Image->GetBufferPointer() +
               m_Image->ComputeOffset( m_PositionIndex );

  m_Remaining = false;
  for (unsigned int i=0; i < ImageDimension; ++i)
    {
    unsigned long size = m_Region.GetSize()[i];
    if( size > 0 ) { m_Remaining = true; }
    }

  for(unsigned int i=0;i<TImage::ImageDimension;i++) 
    {
    m_IsFirstPass[i] = true;
    }

}



template<class TImage>
bool
ReflectiveImageRegionConstIterator<TImage>
::IsReflected(unsigned int dim) const
{
  return !m_IsFirstPass[ dim ];
}


template<class TImage>
void
ReflectiveImageRegionConstIterator<TImage>
::FillOffsets(const OffsetValueType &value)
{
  m_BeginOffset.Fill( value );
  m_EndOffset.Fill( value );
}


//----------------------------------------------------------------------
//  Advance along the line
//----------------------------------------------------------------------
template<class TImage>
ReflectiveImageRegionConstIterator<TImage> &
ReflectiveImageRegionConstIterator<TImage>
::operator++()
{
  
  m_Remaining = false;
  for( unsigned int in=0; in<TImage::ImageDimension; in++ )
    {    
    if( m_IsFirstPass[ in ] ) 
      {
      m_PositionIndex[ in  ]++;
      if( m_PositionIndex[ in ] < m_EndIndex[ in ] )
        {
        m_Position += m_OffsetTable[in];
        m_Remaining = true;
        break;
        }
      else 
        {
        m_PositionIndex[ in ] = m_EndIndex[ in ]-m_EndOffset[ in ]-1; 
        m_IsFirstPass[ in ] = false;
        m_Remaining = true;
        break;
        }
      }
    else 
      {
      m_PositionIndex[ in  ]--;
      if( m_PositionIndex[ in ] >= m_BeginIndex[ in ] )
        {
        m_Position -= m_OffsetTable[in];
        m_Remaining = true;
        break;
        }
      else 
        {
        m_PositionIndex[ in ] = m_BeginIndex[ in ]+m_BeginOffset[ in ]; 
        m_IsFirstPass[ in ] = true;
        }
      }
    }

  if( !m_Remaining ) // It will not advance here otherwise
    {
    m_Position = m_End;
    }

  return *this;
}

} // end namespace itk

#endif
