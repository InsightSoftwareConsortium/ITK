/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkReflectiveImageRegionConstIterator_hxx
#define itkReflectiveImageRegionConstIterator_hxx

#include "itkReflectiveImageRegionConstIterator.h"

namespace itk
{
template< typename TImage >
ReflectiveImageRegionConstIterator< TImage >
::ReflectiveImageRegionConstIterator():ImageConstIteratorWithIndex< TImage >()
{
  for( unsigned int dim = 0; dim < TImage::ImageDimension; dim++ )
    {
    m_BeginOffset[dim] = 0;
    m_EndOffset[dim] = 0;
    }
  this->GoToBegin();
}

template< typename TImage >
ReflectiveImageRegionConstIterator< TImage >
::ReflectiveImageRegionConstIterator(TImage *ptr, const RegionType & region):
  ImageConstIteratorWithIndex< TImage >(ptr, region)
{
  for( unsigned int dim = 0; dim < TImage::ImageDimension; dim++ )
    {
    m_BeginOffset[dim] = 0;
    m_EndOffset[dim] = 0;
    }

  this->GoToBegin();
}

template< typename TImage >
ReflectiveImageRegionConstIterator< TImage >
::ReflectiveImageRegionConstIterator(const Self & it)
{
  this->operator= ( it );
  this->GoToBegin();
}

template< typename TImage >
ReflectiveImageRegionConstIterator< TImage >
::ReflectiveImageRegionConstIterator(
  const ImageConstIteratorWithIndex< TImage > & it)
{
  this->ImageConstIteratorWithIndex< TImage >::operator=(it);

  for( unsigned int dim = 0; dim < TImage::ImageDimension; dim++ )
    {
    m_BeginOffset[dim] = 0;
    m_EndOffset[dim] = 0;
    }
}

template< typename TImage >
ReflectiveImageRegionConstIterator< TImage > &
ReflectiveImageRegionConstIterator< TImage >
::operator=(const Self & it)
{
  if(this != &it)
    {
    this->ImageConstIteratorWithIndex< TImage >::operator=(it);

    for( unsigned int dim = 0; dim < TImage::ImageDimension; dim++ )
      {
      m_BeginOffset[dim] = it.m_BeginOffset[dim];
      m_EndOffset[dim] = it.m_EndOffset[dim];
      }
    }
  return *this;
}

template< typename TImage >
void
ReflectiveImageRegionConstIterator< TImage >
::GoToBegin(void)
{
  this->m_PositionIndex = this->m_BeginIndex + this->m_BeginOffset;
  this->m_Position = this->m_Image->GetBufferPointer()
                     + this->m_Image->ComputeOffset(this->m_PositionIndex);

  this->m_Remaining = false;
  SizeType size = this->m_Region.GetSize();
  for ( unsigned int i = 0; i < TImage::ImageDimension; ++i )
    {
    m_IsFirstPass[i] = true;

    SizeValueType tempSize = size[i];
    if ( tempSize > 0 )
      {
      this->m_Remaining = true;
      }
    }
}

template< typename TImage >
bool
ReflectiveImageRegionConstIterator< TImage >
::IsReflected(unsigned int dim) const
{
  return !m_IsFirstPass[dim];
}

template< typename TImage >
void
ReflectiveImageRegionConstIterator< TImage >
::FillOffsets(const OffsetValueType & value)
{
  for( unsigned int dim = 0; dim < TImage::ImageDimension; dim++ )
    {
    m_BeginOffset[dim] = value;
    m_EndOffset[dim] = value;
    }
}

//----------------------------------------------------------------------
//  Advance along the line
//----------------------------------------------------------------------
template< typename TImage >
ReflectiveImageRegionConstIterator< TImage > &
ReflectiveImageRegionConstIterator< TImage >
::operator++()
{
  this->m_Remaining = false;
  for ( unsigned int in = 0; in < TImage::ImageDimension; in++ )
    {
    if ( m_IsFirstPass[in] )
      {
      if ( this->m_PositionIndex[in]+1 < this->m_EndIndex[in] )
        {
        this->m_PositionIndex[in]++;
        this->m_Position += this->m_OffsetTable[in];
        this->m_Remaining = true;
        break;
        }
      else
        {
        this->m_PositionIndex[in] = this->m_EndIndex[in] - m_EndOffset[in] - 1;
        this->m_Position -= (this->m_EndOffset[in]) * (this->m_OffsetTable[in]);
        m_IsFirstPass[in] = false;
        this->m_Remaining = true;
        break;
        }
      }
    else
      {
      if ( this->m_PositionIndex[in]-1 >= this->m_BeginIndex[in] )
        {
        this->m_PositionIndex[in]--;
        this->m_Position -= this->m_OffsetTable[in];
        this->m_Remaining = true;
        break;
        }
      else
        {
        this->m_PositionIndex[in] = this->m_BeginIndex[in] + m_BeginOffset[in];
        this->m_Position += (this->m_BeginOffset[in]) * (this->m_OffsetTable[in]);
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
