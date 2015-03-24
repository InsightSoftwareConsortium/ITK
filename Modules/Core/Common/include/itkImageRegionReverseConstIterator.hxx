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
#ifndef itkImageRegionReverseConstIterator_hxx
#define itkImageRegionReverseConstIterator_hxx

#include "itkImageRegionReverseConstIterator.h"

namespace itk
{
#if !defined(ITK_LEGACY_REMOVE)
template< typename TImage >
ImageRegionReverseConstIterator< TImage >
ImageRegionReverseConstIterator< TImage >
::Begin() const
{
  // Copy the current iterator
  Self it(*this);

  // Set the iterator to the beginning of the region
  it.GoToBegin();

  return it;
}

template< typename TImage >
ImageRegionReverseConstIterator< TImage >
ImageRegionReverseConstIterator< TImage >
::End() const
{
  // Copy the current iterator
  Self it(*this);

  // Set the iterator to the end of the region
  it.GoToEnd();

  return it;
}
#endif
} // end namespace itk

#endif
