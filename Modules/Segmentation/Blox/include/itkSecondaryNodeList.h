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
#ifndef __itkSecondaryNodeList_h
#define __itkSecondaryNodeList_h

#include <list>

namespace itk
{
/**
 * \class SecondaryNodeList
 * \brief Stores corresponding lists of nodes with pointers to
 * the contained items.
 *
 *
 * \ingroup ITK-Blox
 */

template< typename TItemType, int VCliqueSize >
class SecondaryNodeList:public std::list< TItemType >
{
public:
  /** Pointer to the medial node. */
  TItemType *ItemPointer;

  /** Set the medial node pointer for the corresponding node. */
  void SetNodePointer(TItemType *itemPointer) { ItemPointer = itemPointer; }

  /** Get the number of items stored in the list. */
  SizeValueType GetSize() { return this->size(); }

  //return the index of the base node in the clique
  int GetIndex() { return m_Index; }
  void SetIndex(int value) { m_Index = value; }

  SecondaryNodeList();
  ~SecondaryNodeList();
private:

  int m_Index;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSecondaryNodeList.txx"
#endif

#endif
