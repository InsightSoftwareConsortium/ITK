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
#ifndef itkCorrespondenceDataStructureIterator_h
#define itkCorrespondenceDataStructureIterator_h

#include "itkMacro.h"

namespace itk
{
/** \class CorrespondenceDataStructureIterator
 * \brief An iterator designed to easily traverse an
 *        CorrespondenceDataStructure.
 *
 * \ingroup ITKCommon
 */
template< typename TStructureType >
class ITK_TEMPLATE_EXPORT CorrespondenceDataStructureIterator
{
public:
  /** Standard class typedefs. */
  typedef CorrespondenceDataStructureIterator Self;

  /** Get the dimension (size) of the index. */
  static unsigned int GetIteratorDimension()
  {
    return TStructureType::dim;
  }

  /** Is the iterator at the end of the region? */
  bool IsAtEnd();

  /** Walk forward one index. (prefix) */
  void operator++(){ GoToNext(); }

  /** Walk forward one index. (postfix) */
  void operator++(int){ GoToNext(); }

  /** Goes to the next corresponding node clique in the structure,
   *  moving on to the next base node clique if necessary. */
  void GoToNext();

  /** Goes to the next base node clique. */
  void GoToNextBaseGroup();

  /** Resets the iterator. */
  void Reset();

  /** Constructor */
  CorrespondenceDataStructureIterator(TStructureType *StructurePtr);

  /** Destructor */
  virtual ~CorrespondenceDataStructureIterator();

  typedef typename TStructureType::CorrespondingListType CorrespondingListType;
  typedef typename TStructureType::ItemType              ItemType;
  typedef typename TStructureType::SecondaryNodeListType SecondaryNodeListType;
  typedef typename TStructureType::NodeListType          NodeListType;

  typedef typename CorrespondingListType::iterator CorrespondingListIterator;
  typedef typename SecondaryNodeListType::iterator SecondaryNodeListIterator;
  typedef typename NodeListType::iterator          NodeListIterator;

  /** Get m_CorrespondingListPointer.  */
  CorrespondingListType * GetCorrespondingListPointer()
  {
    return m_CorrespondingListPointer;
  }

  CorrespondingListIterator m_CorrespondingListIterator;
  SecondaryNodeListIterator m_SecondaryListIterator;

  typename TStructureType::NodeListType::iterator m_NodeListIterator;

protected:

  /** Is the iterator at the end of its walk? */
  bool                   m_IsAtEnd;
  TStructureType *       m_Structure;
  ItemType *             m_CorrespondingNodePointer;
  CorrespondingListType *m_CorrespondingListPointer;
  SecondaryNodeListType *m_SecondaryListPointer;
  NodeListType *         m_NodeListPointer;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCorrespondenceDataStructureIterator.hxx"
#endif

#endif
