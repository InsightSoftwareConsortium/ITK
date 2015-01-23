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
#ifndef itkImageIterator_hxx
#define itkImageIterator_hxx

#include "itkImageIterator.h"

namespace itk
{
//----------------------------------------------------------------------
//  Constructor
//----------------------------------------------------------------------
template< typename TImage >
ImageIterator< TImage >
::ImageIterator()
{}

//----------------------------------------------------------------------
//  Constructor
//----------------------------------------------------------------------
template< typename TImage >
ImageIterator< TImage >
::ImageIterator(const Self & it):
  ImageConstIterator< TImage >(it)
{}

//----------------------------------------------------------------------
//  Constructor
//----------------------------------------------------------------------
template< typename TImage >
ImageIterator< TImage >
::ImageIterator(TImage *ptr, const RegionType & region):
  ImageConstIterator< TImage >(ptr, region)
{}

//----------------------------------------------------------------------
//  Constructor
//----------------------------------------------------------------------
template< typename TImage >
ImageIterator< TImage >
::ImageIterator(const ImageConstIterator< TImage > & it):
  ImageConstIterator< TImage >(it)
{}

//----------------------------------------------------------------------
//    Assignment Operator
//----------------------------------------------------------------------
template< typename TImage >
ImageIterator< TImage > &
ImageIterator< TImage >
::operator=(const ImageConstIterator< TImage > & it)
{
  this->ImageConstIterator< TImage >::operator=(it);
  return *this;
}

//----------------------------------------------------------------------
//    Assignment Operator
//----------------------------------------------------------------------
template< typename TImage >
ImageIterator< TImage > &
ImageIterator< TImage >
::operator=(const Self & it)
{
  this->ImageConstIterator< TImage >::operator=(it);
  return *this;
}

#if !defined(ITK_LEGACY_REMOVE)
//----------------------------------------------------------------------------
// Begin() is the first pixel in the region.
template< typename TImage >
ImageIterator< TImage >
ImageIterator< TImage >
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
template< typename TImage >
ImageIterator< TImage >
ImageIterator< TImage >
::End() const
{
  return this->Superclass::End();
}
#endif
} // end namespace itk

#endif
