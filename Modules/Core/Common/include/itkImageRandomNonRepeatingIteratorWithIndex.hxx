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
#ifndef itkImageRandomNonRepeatingIteratorWithIndex_hxx
#define itkImageRandomNonRepeatingIteratorWithIndex_hxx

#include "itkImageRandomNonRepeatingIteratorWithIndex.h"

namespace itk
{
template< typename TImage >
ImageRandomNonRepeatingIteratorWithIndex< TImage >
::ImageRandomNonRepeatingIteratorWithIndex():
  ImageRandomNonRepeatingConstIteratorWithIndex< TImage >()
{}

template< typename TImage >
ImageRandomNonRepeatingIteratorWithIndex< TImage >
::ImageRandomNonRepeatingIteratorWithIndex(ImageType *ptr, const RegionType & region):
  ImageRandomNonRepeatingConstIteratorWithIndex< TImage >(ptr, region)
{}

template< typename TImage >
ImageRandomNonRepeatingIteratorWithIndex< TImage >
::ImageRandomNonRepeatingIteratorWithIndex(const ImageIteratorWithIndex< TImage > & it):
  ImageRandomNonRepeatingConstIteratorWithIndex< TImage >(it)
{}

template< typename TImage >
ImageRandomNonRepeatingIteratorWithIndex< TImage >
::ImageRandomNonRepeatingIteratorWithIndex(const ImageRandomNonRepeatingConstIteratorWithIndex< TImage > & it):
  ImageRandomNonRepeatingConstIteratorWithIndex< TImage >(it)
{}

template< typename TImage >
ImageRandomNonRepeatingIteratorWithIndex< TImage > &
ImageRandomNonRepeatingIteratorWithIndex< TImage >
::operator=(const ImageRandomNonRepeatingConstIteratorWithIndex< TImage > & it)
{
  this->ImageRandomNonRepeatingConstIteratorWithIndex< TImage >::operator=(it);
  return *this;
}
} // end namespace itk

#endif
