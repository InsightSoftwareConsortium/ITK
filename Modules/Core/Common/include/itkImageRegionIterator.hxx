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
#ifndef itkImageRegionIterator_hxx
#define itkImageRegionIterator_hxx

#include "itkImageRegionIterator.h"

namespace itk
{
template< typename TImage >
ImageRegionIterator< TImage >
::ImageRegionIterator():
  ImageRegionConstIterator< TImage >()
{}

template< typename TImage >
ImageRegionIterator< TImage >
::ImageRegionIterator(ImageType *ptr, const RegionType & region):
  ImageRegionConstIterator< TImage >(ptr, region)
{}

template< typename TImage >
ImageRegionIterator< TImage >
::ImageRegionIterator(const ImageIterator< TImage > & it):
  ImageRegionConstIterator< TImage >(it)
{}

template< typename TImage >
ImageRegionIterator< TImage >
::ImageRegionIterator(const ImageRegionConstIterator< TImage > & it):
  ImageRegionConstIterator< TImage >(it)
{}

template< typename TImage >
ImageRegionIterator< TImage > &
ImageRegionIterator< TImage >
::operator=(const ImageRegionConstIterator< TImage > & it)
{
  this->ImageRegionConstIterator< TImage >::operator=(it);
  return *this;
}

#if !defined(ITK_LEGACY_REMOVE)
template< typename TImage >
ImageRegionIterator< TImage >
ImageRegionIterator< TImage >
::Begin() const
{
  return this->Superclass::Begin();
}

template< typename TImage >
ImageRegionIterator< TImage >
ImageRegionIterator< TImage >
::End() const
{
  return this->Superclass::End();
}
#endif
} // end namespace itk

#endif
