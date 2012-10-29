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
#ifndef __itkConditionalConstIterator_hxx
#define __itkConditionalConstIterator_hxx

#include "itkConditionalConstIterator.h"

namespace itk
{

/** operator= is provided to make sure the handle to the image is properly
* reference counted. */
template< typename TImageType >
ConditionalConstIterator< TImageType > &
ConditionalConstIterator< TImageType >
::operator=(const Self & it)
{
  m_IsAtEnd = it.m_IsAtEnd; // copy the end flag
  m_Image = it.m_Image;     // copy the smart pointer
  m_Region = it.m_Region;   // copy the region
  return *this;
}

/** Get the dimension (size) of the index. */
template< typename TImageType >
unsigned int
ConditionalConstIterator< TImageType >
::GetIteratorDimension(void)
{
  return Self::NDimension;
}

template< typename TImageType >
ConditionalConstIterator< TImageType >
::ConditionalConstIterator()
{

}

template< typename TImageType >
ConditionalConstIterator< TImageType >
::~ConditionalConstIterator()
{

}
} // end namespace itk

#endif
