/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageConstIterator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageConstIterator_txx
#define __itkImageConstIterator_txx

#include "itkImageConstIterator.h"

namespace itk
{
//----------------------------------------------------------------------------
// Begin() is the first pixel in the region.
template< class TImage >
ImageConstIterator< TImage >
ImageConstIterator< TImage >
::Begin() const
{
  // Copy the current iterator
  Self it(*this);

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
template< class TImage >
ImageConstIterator< TImage >
ImageConstIterator< TImage >
::End() const
{
  // Copy the current iterator
  Self it(*this);

  // Set the offset to the m_EndOffset.
  it.m_Offset = m_EndOffset;

  return it;
}
} // end namespace itk

#endif
