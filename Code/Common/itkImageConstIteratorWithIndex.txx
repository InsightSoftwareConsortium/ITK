/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageConstIteratorWithIndex.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
  
  // Compute the end offset
  m_Remaining = false;
  IndexType pastEnd;
  for (unsigned int i=0; i < ImageDimension; ++i)
    {
      unsigned int size = region.GetSize()[i];
      if( size > 0 )
      {
        m_Remaining = true;
      }
      m_EndIndex[i] = m_BeginIndex[i] + size;
      pastEnd[i]    = m_BeginIndex[i] + size-1;
    }
  m_End = buffer + m_Image->ComputeOffset( pastEnd );

  m_PixelAccessor = m_Image->GetPixelAccessor();

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
void
ImageConstIteratorWithIndex<TImage>
::Begin()
{
  GoToBegin();
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
    unsigned int size = m_Region.GetSize()[i];
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
void
ImageConstIteratorWithIndex<TImage>
::End()
{
  GoToEnd();
}


//----------------------------------------------------------------------------
// GoToEnd() is the last pixel in the region.
//----------------------------------------------------------------------------
template<class TImage>
void
ImageConstIteratorWithIndex<TImage>
::GoToEnd()
{

  m_Remaining = false;
  for (unsigned int i=0; i < ImageDimension; ++i)
  {
    m_PositionIndex[i]  = m_EndIndex[i]-1;
    unsigned int size = m_Region.GetSize()[i];
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
