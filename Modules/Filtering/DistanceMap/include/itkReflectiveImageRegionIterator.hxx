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
#ifndef itkReflectiveImageRegionIterator_hxx
#define itkReflectiveImageRegionIterator_hxx

#include "itkReflectiveImageRegionIterator.h"

namespace itk
{
template< typename TImage >
ReflectiveImageRegionIterator< TImage >
::ReflectiveImageRegionIterator():
  ReflectiveImageRegionConstIterator< TImage >()
{}

template< typename TImage >
ReflectiveImageRegionIterator< TImage >
::ReflectiveImageRegionIterator(ImageType *ptr, const RegionType & region):
  ReflectiveImageRegionConstIterator< TImage >(ptr, region)
{}

template< typename TImage >
ReflectiveImageRegionIterator< TImage >
::ReflectiveImageRegionIterator(const ImageIteratorWithIndex< TImage > & it):
  ReflectiveImageRegionConstIterator< TImage >(it)
{}

template< typename TImage >
ReflectiveImageRegionIterator< TImage >
::ReflectiveImageRegionIterator(const ReflectiveImageRegionConstIterator< TImage > & it):
  ReflectiveImageRegionConstIterator< TImage >(it)
{}

template< typename TImage >
ReflectiveImageRegionIterator< TImage > &
ReflectiveImageRegionIterator< TImage >
::operator=(const ReflectiveImageRegionConstIterator< TImage > & it)
{
  this->ReflectiveImageRegionConstIterator< TImage >::operator=(it);
  return *this;
}
} // end namespace itk

#endif
