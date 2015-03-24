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
#ifndef itkImageReverseConstIterator_hxx
#define itkImageReverseConstIterator_hxx

#include "itkImageReverseConstIterator.h"

namespace itk
{
#if !defined(ITK_LEGACY_REMOVE)
//----------------------------------------------------------------------------
// Begin() is the last pixel in the region.
template< typename TImage >
ImageReverseConstIterator< TImage >
ImageReverseConstIterator< TImage >
::Begin() const
{
  // Copy the current iterator
  Self it(*this);

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
template< typename TImage >
ImageReverseConstIterator< TImage >
ImageReverseConstIterator< TImage >
::End() const
{
  // Copy the current iterator
  Self it(*this);

  // Set the offset to the m_EndOffset.
  it.m_Offset = m_EndOffset;

  return it;
}
#endif
} // end namespace itk

#endif
