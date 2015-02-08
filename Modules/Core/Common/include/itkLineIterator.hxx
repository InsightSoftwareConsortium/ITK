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
#ifndef itkLineIterator_hxx
#define itkLineIterator_hxx

#include "itkLineIterator.h"

namespace itk
{
template< typename TImage >
LineIterator< TImage >
::LineIterator(ImageType *imagePtr, const IndexType & firstIndex, const IndexType & lastIndex):
  LineConstIterator< TImage >(imagePtr, firstIndex, lastIndex)
{}

template< typename TImage >
LineIterator< TImage > &
LineIterator< TImage >
::operator=(const Self & it)
{
  this->LineConstIterator< TImage >::operator=(it);
  return *this;
}
} // end namespace itk

#endif
