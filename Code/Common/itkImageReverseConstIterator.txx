/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageReverseConstIterator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageReverseConstIterator_txx
#define _itkImageReverseConstIterator_txx

#include "itkImageReverseConstIterator.h"

namespace itk
{

//----------------------------------------------------------------------------
// Begin() is the last pixel in the region.
template<class TImage>
ImageReverseConstIterator<TImage>
ImageReverseConstIterator<TImage>
::Begin() const
{
  // Copy the current iterator
  Self it( *this );

  // Set the offset to the m_BeginOffset. 
  it.m_Offset = m_BeginOffset;
  
  return it;
}

//----------------------------------------------------------------------------
// End() is one pixel before the first pixel in the current region.
// The index of this pixel is
//          [m_StartIndex[0] - 1,
//           m_StartIndex[1], ...,
//           m_StartIndex[VImageDimension-2],
//           m_StartIndex[VImageDimension-1]]
//
template<class TImage>
ImageReverseConstIterator<TImage>
ImageReverseConstIterator<TImage>
::End() const
{
  // Copy the current iterator
  Self it( *this );

  // Set the offset to the m_EndOffset. 
  it.m_Offset = m_EndOffset;
  
  return it;
}

} // end namespace itk

#endif
