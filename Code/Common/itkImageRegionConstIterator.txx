/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageRegionConstIterator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageRegionConstIterator_txx
#define _itkImageRegionConstIterator_txx

#include "itkImageRegionConstIterator.h"

namespace itk
{

//----------------------------------------------------------------------------
// Begin() is the first pixel in the region.
template<class TImage>
ImageRegionConstIterator<TImage>
ImageRegionConstIterator<TImage>
::Begin() const
{
  return this->Superclass::Begin();
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
ImageRegionConstIterator<TImage>
ImageRegionConstIterator<TImage>
::End() const
{
  return this->Superclass::End();
}

} // end namespace itk

#endif
