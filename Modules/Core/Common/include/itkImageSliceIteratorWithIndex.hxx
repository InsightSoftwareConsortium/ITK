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
#ifndef itkImageSliceIteratorWithIndex_hxx
#define itkImageSliceIteratorWithIndex_hxx

#include "itkImageSliceIteratorWithIndex.h"

namespace itk
{
template< typename TImage >
ImageSliceIteratorWithIndex< TImage >
::ImageSliceIteratorWithIndex():
  ImageSliceConstIteratorWithIndex< TImage >()
{}

template< typename TImage >
ImageSliceIteratorWithIndex< TImage >
::ImageSliceIteratorWithIndex(ImageType *ptr, const RegionType & region):
  ImageSliceConstIteratorWithIndex< TImage >(ptr, region)
{}

template< typename TImage >
ImageSliceIteratorWithIndex< TImage >
::ImageSliceIteratorWithIndex(const ImageIteratorWithIndex< TImage > & it):
  ImageSliceConstIteratorWithIndex< TImage >(it)
{}

template< typename TImage >
ImageSliceIteratorWithIndex< TImage >
::ImageSliceIteratorWithIndex(const ImageSliceConstIteratorWithIndex< TImage > & it):
  ImageSliceConstIteratorWithIndex< TImage >(it)
{}

template< typename TImage >
ImageSliceIteratorWithIndex< TImage > &
ImageSliceIteratorWithIndex< TImage >
::operator=(const ImageSliceConstIteratorWithIndex< TImage > & it)
{
  this->ImageSliceConstIteratorWithIndex< TImage >::operator=(it);
  return *this;
}
} // end namespace itk

#endif
