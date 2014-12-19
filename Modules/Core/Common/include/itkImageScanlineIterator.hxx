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
#ifndef itkImageScanlineIterator_hxx
#define itkImageScanlineIterator_hxx

#include "itkImageScanlineIterator.h"

namespace itk
{
template< typename TImage >
ImageScanlineIterator< TImage >
::ImageScanlineIterator():
  ImageScanlineConstIterator< TImage >()
{}

template< typename TImage >
ImageScanlineIterator< TImage >
::ImageScanlineIterator(ImageType *ptr, const RegionType & region):
  ImageScanlineConstIterator< TImage >(ptr, region)
{}

template< typename TImage >
ImageScanlineIterator< TImage >
::ImageScanlineIterator(const ImageIterator< TImage > & it):
  ImageScanlineConstIterator< TImage >(it)
{}

template< typename TImage >
ImageScanlineIterator< TImage >
::ImageScanlineIterator(const ImageScanlineConstIterator< TImage > & it):
  ImageScanlineConstIterator< TImage >(it)
{}

template< typename TImage >
ImageScanlineIterator< TImage > &
ImageScanlineIterator< TImage >
::operator=(const ImageScanlineConstIterator< TImage > & it)
{
  this->ImageScanlineConstIterator< TImage >::operator=(it);
  return *this;
}

} // end namespace itk

#endif
