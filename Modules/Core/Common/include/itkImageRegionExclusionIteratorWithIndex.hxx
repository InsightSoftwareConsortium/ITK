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
#ifndef itkImageRegionExclusionIteratorWithIndex_hxx
#define itkImageRegionExclusionIteratorWithIndex_hxx

#include "itkImageRegionExclusionIteratorWithIndex.h"

namespace itk
{
template< typename TImage >
ImageRegionExclusionIteratorWithIndex< TImage >
::ImageRegionExclusionIteratorWithIndex():
  ImageRegionExclusionConstIteratorWithIndex< TImage >()
{}

template< typename TImage >
ImageRegionExclusionIteratorWithIndex< TImage >
::ImageRegionExclusionIteratorWithIndex(ImageType *ptr, const RegionType & region):
  ImageRegionExclusionConstIteratorWithIndex< TImage >(ptr, region)
{}

template< typename TImage >
ImageRegionExclusionIteratorWithIndex< TImage >
::ImageRegionExclusionIteratorWithIndex(const ImageIteratorWithIndex< TImage > & it):
  ImageRegionExclusionConstIteratorWithIndex< TImage >(it)
{}

template< typename TImage >
ImageRegionExclusionIteratorWithIndex< TImage >
::ImageRegionExclusionIteratorWithIndex(const ImageRegionExclusionConstIteratorWithIndex< TImage > & it):
  ImageRegionExclusionConstIteratorWithIndex< TImage >(it)
{}

template< typename TImage >
ImageRegionExclusionIteratorWithIndex< TImage > &
ImageRegionExclusionIteratorWithIndex< TImage >
::operator=(const ImageRegionExclusionConstIteratorWithIndex< TImage > & it)
{
  this->ImageRegionExclusionConstIteratorWithIndex< TImage >::operator=(it);
  return *this;
}
} // end namespace itk

#endif
