/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageIteratorWithIndex.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

// #include "itkImageIteratorWithIndex.h"

namespace itk
{



//----------------------------------------------------------------------
//  Constructor
//----------------------------------------------------------------------
template<class TImage>
ImageIteratorWithIndex<TImage>
::ImageIteratorWithIndex()
{
  m_Position  = 0;
  m_Begin     = 0;
  m_End       = 0;
  m_Remaining = true;
}



//----------------------------------------------------------------------
//  Constructor
//----------------------------------------------------------------------
template<class TImage>
ImageIteratorWithIndex<TImage>
::ImageIteratorWithIndex(const Self& it)
{
  m_Image = it.m_Image;     // copy the smart pointer

  m_PositionIndex     = it.m_PositionIndex;
  m_BeginIndex        = it.m_BeginIndex;
  m_EndIndex          = it.m_EndIndex;
  m_Region            = it.m_Region;

  memcpy(m_OffsetTable, it.m_OffsetTable, 
          (ImageIteratorDimension+1)*sizeof(unsigned long));
  
  m_Position    = it.m_Position;
  m_Begin       = it.m_Begin;
  m_End         = it.m_End;
  m_Remaining   = it.m_Remaining;
}



//----------------------------------------------------------------------
//  Constructor
//----------------------------------------------------------------------
template<class TImage>
ImageIteratorWithIndex<TImage>
::ImageIteratorWithIndex(const TImage *ptr,
                         const Region & region )
{
  m_Image = ptr;

  InternalPixelType * m_Buffer   = m_Image->GetBufferPointer();

  m_BeginIndex        = region.GetIndex();
  m_PositionIndex     = m_BeginIndex;
  m_Region            = region;

  memcpy(m_OffsetTable, m_Image->GetOffsetTable(), 
        (ImageIteratorDimension+1)*sizeof(unsigned long));

  // Compute the start position
  long offs =  m_Image->ComputeOffset( m_BeginIndex );
  m_Begin = m_Buffer + offs;
  
  // Compute the end offset
  Index pastEnd;
  for (unsigned int i=0; i < ImageIteratorDimension; ++i)
    {
    m_EndIndex[i] = m_BeginIndex[i] + region.GetSize()[i];
    pastEnd[i]    = m_BeginIndex[i] + region.GetSize()[i]-1;
    }
  m_End = m_Buffer + m_Image->ComputeOffset( pastEnd );
  m_End++;
  m_Remaining = true;

}
 

//----------------------------------------------------------------------
//    Assignment Operator
//----------------------------------------------------------------------
template<class TImage>
ImageIteratorWithIndex<TImage> &
ImageIteratorWithIndex<TImage>
::operator=(const Self& it)
{
  m_Image = it.m_Image;     // copy the smart pointer

  m_BeginIndex        = it.m_BeginIndex;
  m_EndIndex          = it.m_EndIndex;
  m_PositionIndex     = it.m_PositionIndex;

  memcpy(m_OffsetTable, it.m_OffsetTable, 
        (ImageIteratorDimension+1)*sizeof(unsigned long));
  
  m_Position    = it.m_Position;
  m_Begin       = it.m_Begin;
  m_End         = it.m_End;
  m_Remaining   = it.m_Remaining;

  return *this;
} 
  



//----------------------------------------------------------------------------
// Begin() is the first pixel in the region.
//----------------------------------------------------------------------------
template<class TImage>
void
ImageIteratorWithIndex<TImage>
::Begin()
{
  // Set the position at begin

  m_Position       = m_Begin;
  m_PositionIndex  = m_BeginIndex;
  m_Remaining      = true;
}


} // end namespace itk



