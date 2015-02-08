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
#ifndef itkImageRegionReverseIterator_hxx
#define itkImageRegionReverseIterator_hxx

#include "itkImageRegionReverseIterator.h"

namespace itk
{
template< typename TImage >
ImageRegionReverseIterator< TImage >
::ImageRegionReverseIterator():
  ImageRegionReverseConstIterator< TImage >()
{}

template< typename TImage >
ImageRegionReverseIterator< TImage >
::ImageRegionReverseIterator(ImageType *ptr, const RegionType & region):
  ImageRegionReverseConstIterator< TImage >(ptr, region)
{}

template< typename TImage >
ImageRegionReverseIterator< TImage >
::ImageRegionReverseIterator(const ImageConstIterator< TImage > & it):Superclass(it)
{}

template< typename TImage >
ImageRegionReverseIterator< TImage >
::ImageRegionReverseIterator(const ImageRegionReverseConstIterator< TImage > & it):
  Superclass(it)
{}

template< typename TImage >
ImageRegionReverseIterator< TImage > &
ImageRegionReverseIterator< TImage >
::operator=(const ImageRegionReverseConstIterator< TImage > & it)
{
  this->Superclass::operator=(it);
  return *this;
}

#if !defined(ITK_LEGACY_REMOVE)
template< typename TImage >
ImageRegionReverseIterator< TImage >
ImageRegionReverseIterator< TImage >
::Begin() const
{
  return this->Superclass::Begin();
}

template< typename TImage >
ImageRegionReverseIterator< TImage >
ImageRegionReverseIterator< TImage >
::End() const
{
  return this->Superclass::End();
}
#endif
} // end namespace itk

#endif
