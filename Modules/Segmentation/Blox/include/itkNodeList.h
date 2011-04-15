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
#ifndef __itkNodeList_h
#define __itkNodeList_h

#include <list>

namespace itk
{
/**
 * \class NodeList
 * \brief Stores secondary lists of nodes with pointers to
 * the contained items.
 *
 * \ingroup ImageObjects
 * \ingroup ITK-Blox
 */

template< typename TItemType >
class NodeList:public std::list< TItemType >
{
public:
  typedef typename std::list< TItemType>::size_type NodeListSizeType;

  /** Pointer to the item. */
  TItemType *ItemPointer;

  /** Store a pointer to the iteme in the list. */
  void SetItemPointer(TItemType *itemPointer) { ItemPointer = itemPointer; }

  /** Get the number of items stored in the list. */
  NodeListSizeType GetSize() const
  { return this->size(); }

  NodeList();
  ~NodeList();
private:
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNodeList.txx"
#endif

#endif
