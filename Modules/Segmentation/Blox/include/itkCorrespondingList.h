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
#ifndef __itkCorrespondingList_h
#define __itkCorrespondingList_h

#include <list>

namespace itk
{
/**
 * \class CorrespondingList
 * \brief Part of the itkCorrespondenceDataStructure.
 *
 *
 *
 *
 * \ingroup ITK-Blox
 */

template< typename TItemType, int VCliqueSize >
class CorrespondingList:public std::list< TItemType >
{
public:

  typedef typename std::list< TItemType >   SuperClass;
  typedef typename SuperClass::size_type    SizeType;

  /** Get the number of items stored in the blox. */
  SizeType GetSize()
  { return this->size(); }

  /** Set and Get functions for thesecondary indices in the base node clique. */
  int GetIndex(int i){ return m_Index[i]; }
  void SetIndex(int i, int value){ m_Index[i] = value; }

  /** Constructor */
  CorrespondingList();

  /** Destructor */
  ~CorrespondingList();
private:

  /** Array to hold secondary indices of the base node clique */
  int m_Index[VCliqueSize];
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCorrespondingList.txx"
#endif

#endif
