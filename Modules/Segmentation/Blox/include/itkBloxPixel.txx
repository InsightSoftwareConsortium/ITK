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
#ifndef __itkBloxPixel_txx
#define __itkBloxPixel_txx

#include "itkBloxPixel.h"

namespace itk
{
template< typename TItemType >
BloxPixel< TItemType >
::BloxPixel()
{}

template< typename TItemType >
BloxPixel< TItemType >
::~BloxPixel()
{
  // We need to clean up memory used by linked list entries
  // Walk through all of the elements at the pixel and delete what we find
  this->DeleteListEntries();
}

template< typename TItemType >
void
BloxPixel< TItemType >
::DeleteListEntries()
{
  // Delete all entries in the linked list and clear the list
  // if the list contains existing entries
  if ( !( this->empty() ) )
    {
    typename BloxPixel::iterator bpiterator;

    for ( bpiterator = this->begin(); bpiterator != this->end(); ++bpiterator )
      {
      delete ( *bpiterator );
      }

    // Empty the linked list
    this->clear();
    }
}
} // end namespace itk

#endif
