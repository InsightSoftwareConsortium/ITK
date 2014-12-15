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
#ifndef itkImageRegionIteratorWithIndex_hxx
#define itkImageRegionIteratorWithIndex_hxx

#include "itkImageRegionIteratorWithIndex.h"

namespace itk
{
template< typename TImage >
ImageRegionIteratorWithIndex< TImage >
::ImageRegionIteratorWithIndex():
  ImageRegionConstIteratorWithIndex< TImage >()
{}

template< typename TImage >
ImageRegionIteratorWithIndex< TImage >
::ImageRegionIteratorWithIndex(TImage *ptr, const RegionType & region):
  ImageRegionConstIteratorWithIndex< TImage >(ptr, region)
{}

template< typename TImage >
ImageRegionIteratorWithIndex< TImage >
::ImageRegionIteratorWithIndex(const ImageIteratorWithIndex< TImage > & it):
  ImageRegionConstIteratorWithIndex< TImage >(it)
{}

template< typename TImage >
ImageRegionIteratorWithIndex< TImage >
::ImageRegionIteratorWithIndex(const ImageRegionConstIteratorWithIndex< TImage > & it):
  ImageRegionConstIteratorWithIndex< TImage >(it)
{}

template< typename TImage >
ImageRegionIteratorWithIndex< TImage > &
ImageRegionIteratorWithIndex< TImage >
::operator=(const ImageRegionConstIteratorWithIndex< TImage > & it)
{
  this->ImageRegionConstIteratorWithIndex< TImage >::operator=(it);
  return *this;
}
} // end namespace itk

#endif
