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
#ifndef itkImageLinearIteratorWithIndex_hxx
#define itkImageLinearIteratorWithIndex_hxx

#include "itkImageLinearIteratorWithIndex.h"

namespace itk
{
template< typename TImage >
ImageLinearIteratorWithIndex< TImage >
::ImageLinearIteratorWithIndex():
  ImageLinearConstIteratorWithIndex< TImage >()
{}

template< typename TImage >
ImageLinearIteratorWithIndex< TImage >
::ImageLinearIteratorWithIndex(ImageType *ptr, const RegionType & region):
  ImageLinearConstIteratorWithIndex< TImage >(ptr, region)
{}

template< typename TImage >
ImageLinearIteratorWithIndex< TImage >
::ImageLinearIteratorWithIndex(const ImageIteratorWithIndex< TImage > & it):
  ImageLinearConstIteratorWithIndex< TImage >(it)
{}

template< typename TImage >
ImageLinearIteratorWithIndex< TImage >
::ImageLinearIteratorWithIndex(const ImageLinearConstIteratorWithIndex< TImage > & it):
  ImageLinearConstIteratorWithIndex< TImage >(it)
{}

template< typename TImage >
ImageLinearIteratorWithIndex< TImage > &
ImageLinearIteratorWithIndex< TImage >
::operator=(const ImageLinearConstIteratorWithIndex< TImage > & it)
{
  this->ImageLinearConstIteratorWithIndex< TImage >::operator=(it);
  return *this;
}
} // end namespace itk

#endif
