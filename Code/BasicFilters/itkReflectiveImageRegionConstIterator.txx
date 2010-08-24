/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkReflectiveImageRegionConstIterator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkReflectiveImageRegionConstIterator_txx
#define __itkReflectiveImageRegionConstIterator_txx

#include "itkReflectiveImageRegionConstIterator.h"

namespace itk
{
template< class TImage >
ReflectiveImageRegionConstIterator< TImage >
::ReflectiveImageRegionConstIterator():ImageConstIteratorWithIndex< TImage >()
{
  int i;

  m_BeginOffset.Fill(0);
  m_EndOffset.Fill(0);
  this->GoToBegin();
}

template< class TImage >
ReflectiveImageRegionConstIterator< TImage >
::ReflectiveImageRegionConstIterator(TImage *ptr, const RegionType & region):
  ImageConstIteratorWithIndex< TImage >(ptr, region)
{
  m_BeginOffset.Fill(0);
  m_EndOffset.Fill(0);
  this->GoToBegin();
}

template< class TImage >
ReflectiveImageRegionConstIterator< TImage >
::ReflectiveImageRegionConstIterator(const Self & it)
{
  this->Operator = ( it );
  this->GoToBegin();
}

template< class TImage >
ReflectiveImageRegionConstIterator< TImage >
::ReflectiveImageRegionConstIterator(
  const ImageConstIteratorWithIndex< TImage > & it)
{
  this->ImageConstIteratorWithIndex< TImage >::operator=(it);

  m_BeginOffset.Fill(0);
  m_EndOffset.Fill(0);
}

template< class TImage >
ReflectiveImageRegionConstIterator< TImage > &
ReflectiveImageRegionConstIterator< TImage >
::operator=(const Self & it)
{
  this->ImageConstIteratorWithIndex< TImage >::operator=(it);
  m_BeginOffset = it.m_BeginOffset;
  m_EndOffset = it.m_EndOffset;
  return *this;
}

template< class TImage >
void
ReflectiveImageRegionConstIterator< TImage >
::GoToBegin(void)
{
  this->m_PositionIndex = this->m_BeginIndex + this->m_BeginOffset;
  this->m_Position = this->m_Image->GetBufferPointer()
                     + this->m_Image->ComputeOffset(this->m_PositionIndex);

  this->m_Remaining = false;
  for ( unsigned int i = 0; i < TImage::ImageDimension; ++i )
    {
    unsigned long size = this->m_Region.GetSize()[i];
    if ( size > 0 ) { this->m_Remaining = true; }
    }

  for ( unsigned int i = 0; i < TImage::ImageDimension; i++ )
    {
    m_IsFirstPass[i] = true;
    }
}

template< class TImage >
bool
ReflectiveImageRegionConstIterator< TImage >
::IsReflected(unsigned int dim) const
{
  return !m_IsFirstPass[dim];
}

template< class TImage >
void
ReflectiveImageRegionConstIterator< TImage >
::FillOffsets(const OffsetValueType & value)
{
  m_BeginOffset.Fill(value);
  m_EndOffset.Fill(value);
}

//----------------------------------------------------------------------
//  Advance along the line
//----------------------------------------------------------------------
template< class TImage >
ReflectiveImageRegionConstIterator< TImage > &
ReflectiveImageRegionConstIterator< TImage >
::operator++()
{
  this->m_Remaining = false;
  for ( unsigned int in = 0; in < TImage::ImageDimension; in++ )
    {
    if ( m_IsFirstPass[in] )
      {
      this->m_PositionIndex[in]++;
      if ( this->m_PositionIndex[in] < this->m_EndIndex[in] )
        {
        this->m_Position += this->m_OffsetTable[in];
        this->m_Remaining = true;
        break;
        }
      else
        {
        this->m_PositionIndex[in] = this->m_EndIndex[in] - m_EndOffset[in] - 1;
        m_IsFirstPass[in] = false;
        this->m_Remaining = true;
        break;
        }
      }
    else
      {
      this->m_PositionIndex[in]--;
      if ( this->m_PositionIndex[in] >= this->m_BeginIndex[in] )
        {
        this->m_Position -= this->m_OffsetTable[in];
        this->m_Remaining = true;
        break;
        }
      else
        {
        this->m_PositionIndex[in] = this->m_BeginIndex[in] + m_BeginOffset[in];
        m_IsFirstPass[in] = true;
        }
      }
    }

  if ( !this->m_Remaining ) // It will not advance here otherwise
    {
    this->m_Position = this->m_End;
    }

  return *this;
}
} // end namespace itk

#endif
