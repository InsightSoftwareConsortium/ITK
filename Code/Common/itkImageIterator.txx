/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageIterator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkImageIterator_txx
#define _itkImageIterator_txx

// #include "itkImageIterator.h"

namespace itk
{

//----------------------------------------------------------------------------
// Begin() is the first pixel in the region.
template<class TImage>
ImageIterator<TImage>
ImageIterator<TImage>
::Begin()
{
  // Copy the current iterator
  Self it( *this );

  // Set the offset to the m_BeginOffset. 
  it.m_Offset = m_BeginOffset;
  
  return it;
}

//----------------------------------------------------------------------------
// End() is one pixel past the last pixel in the current region.
// The index of this pixel is
//          [m_StartIndex[0] + m_Size[0],
//           m_StartIndex[1] + m_Size[1]-1, ...,
//           m_StartIndex[VImageDimension-2] + m_Size[VImageDimension-2]-1,
//           m_StartIndex[VImageDimension-1] + m_Size[VImageDimension-1]-1]
//
template<class TImage>
ImageIterator<TImage>
ImageIterator<TImage>
::End()
{
  // Copy the current iterator
  Self it( *this );

  // Set the offset to the m_EndOffset. 
  it.m_Offset = m_EndOffset;
  
  return it;
}

} // end namespace itk

#endif
