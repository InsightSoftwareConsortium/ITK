/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageConstIteratorWithIndex.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageConstIteratorWithIndex_txx
#define _itkImageConstIteratorWithIndex_txx

#include "itkImageConstIteratorWithIndex.h"

namespace itk
{



//----------------------------------------------------------------------
//  Constructor
//----------------------------------------------------------------------
template<class TImage>
ImageConstIteratorWithIndex<TImage>
::ImageConstIteratorWithIndex()
{
  m_Position  = 0;
  m_Begin     = 0;
  m_End       = 0;
  m_Remaining = false;
}



//----------------------------------------------------------------------
//  Constructor
//----------------------------------------------------------------------
template<class TImage>
ImageConstIteratorWithIndex<TImage>
::ImageConstIteratorWithIndex(const Self& it)
{
  m_Image = it.m_Image;     // copy the smart pointer

  m_PositionIndex     = it.m_PositionIndex;
  m_BeginIndex        = it.m_BeginIndex;
  m_EndIndex          = it.m_EndIndex;
  m_Region            = it.m_Region;

  memcpy(m_OffsetTable, it.m_OffsetTable, 
          (ImageDimension+1)*sizeof(unsigned long));
  
  m_Position    = it.m_Position;
  m_Begin       = it.m_Begin;
  m_End         = it.m_End;
  m_Remaining   = it.m_Remaining;

  m_PixelAccessor = it.m_PixelAccessor;
}



//----------------------------------------------------------------------
//  Constructor
//----------------------------------------------------------------------
template<class TImage>
ImageConstIteratorWithIndex<TImage>
::ImageConstIteratorWithIndex( const TImage *ptr,
                         const RegionType & region )
{
  m_Image = ptr;

  const InternalPixelType * buffer   = m_Image->GetBufferPointer();

  m_BeginIndex        = region.GetIndex();
  m_PositionIndex     = m_BeginIndex;
  m_Region            = region;

  memcpy(m_OffsetTable, m_Image->GetOffsetTable(), 
        (ImageDimension+1)*sizeof(unsigned long));

  // Compute the start position
  long offs =  m_Image->ComputeOffset( m_BeginIndex );
  m_Begin = buffer + offs;
  m_Position = m_Begin;
  
  // Compute the end offset
  m_Remaining = false;
  IndexType pastEnd;
  for (unsigned int i=0; i < ImageDimension; ++i)
    {
      unsigned long size = region.GetSize()[i];
      if( size > 0 )
      {
        m_Remaining = true;
      }
      m_EndIndex[i] = m_BeginIndex[i] + static_cast<long>(size);
      pastEnd[i]    = m_BeginIndex[i] + static_cast<long>(size)-1;
    }
  m_End = buffer + m_Image->ComputeOffset( pastEnd );

  m_PixelAccessor = m_Image->GetPixelAccessor();

  GoToBegin();

}
 

//----------------------------------------------------------------------
//    Assignment Operator
//----------------------------------------------------------------------
template<class TImage>
ImageConstIteratorWithIndex<TImage> &
ImageConstIteratorWithIndex<TImage>
::operator=(const Self& it)
{
  m_Image = it.m_Image;     // copy the smart pointer

  m_BeginIndex        = it.m_BeginIndex;
  m_EndIndex          = it.m_EndIndex;
  m_PositionIndex     = it.m_PositionIndex;
  m_Region            = it.m_Region;

  memcpy(m_OffsetTable, it.m_OffsetTable, 
        (ImageDimension+1)*sizeof(unsigned long));
  
  m_Position    = it.m_Position;
  m_Begin       = it.m_Begin;
  m_End         = it.m_End;
  m_Remaining   = it.m_Remaining;

  m_PixelAccessor = it.m_PixelAccessor;

  return *this;
} 
  


//----------------------------------------------------------------------------
// Begin() is the first pixel in the region.
//----------------------------------------------------------------------------
template<class TImage>
ImageConstIteratorWithIndex<TImage> 
ImageConstIteratorWithIndex<TImage>
::Begin() const
{
  Self it( *this );
  it.GoToBegin();
  return it;
}


//----------------------------------------------------------------------------
// GoToBegin() is the first pixel in the region.
//----------------------------------------------------------------------------
template<class TImage>
void
ImageConstIteratorWithIndex<TImage>
::GoToBegin()
{
  // Set the position at begin

  m_Position       = m_Begin;
  m_PositionIndex  = m_BeginIndex;
 
  m_Remaining = false;
  for (unsigned int i=0; i < ImageDimension; ++i)
  {
    unsigned long size = m_Region.GetSize()[i];
    if( size > 0 )
    {
      m_Remaining = true;
    }

  }


}


//----------------------------------------------------------------------------
// End() is the last pixel in the region.  DEPRECATED
//----------------------------------------------------------------------------
template<class TImage>
ImageConstIteratorWithIndex<TImage> 
ImageConstIteratorWithIndex<TImage>
::End() const
{
  Self it( *this );
  it.GoToReverseBegin();
  return it;
}


//----------------------------------------------------------------------------
// GoToReverseBegin() is the last pixel in the region.
//----------------------------------------------------------------------------
template<class TImage>
void
ImageConstIteratorWithIndex<TImage>
::GoToReverseBegin()
{

  m_Remaining = false;
  for (unsigned int i=0; i < ImageDimension; ++i)
  {
    m_PositionIndex[i]  = m_EndIndex[i]-1;
    unsigned long size = m_Region.GetSize()[i];
    if( size > 0 )
    {
      m_Remaining = true;
    }

  }

  // Set the position at the end
  const InternalPixelType * buffer   = m_Image->GetBufferPointer();
  const unsigned long       offset   = m_Image->ComputeOffset( m_PositionIndex );
  m_Position = buffer + offset;

}


} // end namespace itk




#endif
