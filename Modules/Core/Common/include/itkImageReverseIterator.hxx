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
#ifndef itkImageReverseIterator_hxx
#define itkImageReverseIterator_hxx

#include "itkImageReverseIterator.h"

namespace itk
{
template< typename TImage >
ImageReverseIterator< TImage >
::ImageReverseIterator():
  ImageRegionReverseConstIterator< TImage >()
{}

template< typename TImage >
ImageReverseIterator< TImage >
::ImageReverseIterator(ImageType *ptr, const RegionType & region):
  ImageRegionReverseConstIterator< TImage >(ptr, region)
{}

template< typename TImage >
ImageReverseIterator< TImage >
::ImageReverseIterator(const ImageIteratorWithIndex< TImage > & it):
  ImageRegionReverseConstIterator< TImage >(it)
{}

template< typename TImage >
ImageReverseIterator< TImage >
::ImageReverseIterator(const ImageRegionReverseConstIterator< TImage > & it):
  ImageRegionReverseConstIterator< TImage >(it)
{}

template< typename TImage >
ImageReverseIterator< TImage > &
ImageReverseIterator< TImage >
::operator=(const ImageRegionReverseConstIterator< TImage > & it)
{
  this->ImageRegionReverseConstIterator< TImage >::operator=(it);
  return *this;
}
} // end namespace itk

#endif
