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
#ifndef itkImageRandomIteratorWithIndex_hxx
#define itkImageRandomIteratorWithIndex_hxx

#include "itkImageRandomIteratorWithIndex.h"

namespace itk
{
template< typename TImage >
ImageRandomIteratorWithIndex< TImage >
::ImageRandomIteratorWithIndex():
  ImageRandomConstIteratorWithIndex< TImage >()
{}

template< typename TImage >
ImageRandomIteratorWithIndex< TImage >
::ImageRandomIteratorWithIndex(ImageType *ptr, const RegionType & region):
  ImageRandomConstIteratorWithIndex< TImage >(ptr, region)
{}

template< typename TImage >
ImageRandomIteratorWithIndex< TImage >
::ImageRandomIteratorWithIndex(const ImageIteratorWithIndex< TImage > & it):
  ImageRandomConstIteratorWithIndex< TImage >(it)
{}

template< typename TImage >
ImageRandomIteratorWithIndex< TImage >
::ImageRandomIteratorWithIndex(const ImageRandomConstIteratorWithIndex< TImage > & it):
  ImageRandomConstIteratorWithIndex< TImage >(it)
{}

template< typename TImage >
ImageRandomIteratorWithIndex< TImage > &
ImageRandomIteratorWithIndex< TImage >
::operator=(const ImageRandomConstIteratorWithIndex< TImage > & it)
{
  this->ImageRandomConstIteratorWithIndex< TImage >::operator=(it);
  return *this;
}
} // end namespace itk

#endif
