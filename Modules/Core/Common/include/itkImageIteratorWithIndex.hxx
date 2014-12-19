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
#ifndef itkImageIteratorWithIndex_hxx
#define itkImageIteratorWithIndex_hxx

#include "itkImageIteratorWithIndex.h"

namespace itk
{
//----------------------------------------------------------------------
//  Constructor
//----------------------------------------------------------------------
template< typename TImage >
ImageIteratorWithIndex< TImage >
::ImageIteratorWithIndex()
{}

//----------------------------------------------------------------------
//  Constructor
//----------------------------------------------------------------------
template< typename TImage >
ImageIteratorWithIndex< TImage >
::ImageIteratorWithIndex(const Self & it):
  ImageConstIteratorWithIndex< TImage >(it)
{}

//----------------------------------------------------------------------
//  Constructor
//----------------------------------------------------------------------
template< typename TImage >
ImageIteratorWithIndex< TImage >
::ImageIteratorWithIndex(TImage *ptr, const RegionType & region):
  ImageConstIteratorWithIndex< TImage >(ptr, region)
{}

//----------------------------------------------------------------------
//  Constructor
//----------------------------------------------------------------------
template< typename TImage >
ImageIteratorWithIndex< TImage >
::ImageIteratorWithIndex(const ImageConstIteratorWithIndex< TImage > & it):
  ImageConstIteratorWithIndex< TImage >(it)
{}

//----------------------------------------------------------------------
//    Assignment Operator
//----------------------------------------------------------------------
template< typename TImage >
ImageIteratorWithIndex< TImage > &
ImageIteratorWithIndex< TImage >
::operator=(const ImageConstIteratorWithIndex< TImage > & it)
{
  this->ImageConstIteratorWithIndex< TImage >::operator=(it);
  return *this;
}

//----------------------------------------------------------------------
//    Assignment Operator
//----------------------------------------------------------------------
template< typename TImage >
ImageIteratorWithIndex< TImage > &
ImageIteratorWithIndex< TImage >
::operator=(const Self & it)
{
  this->ImageConstIteratorWithIndex< TImage >::operator=(it);
  return *this;
}

} // end namespace itk

#endif
