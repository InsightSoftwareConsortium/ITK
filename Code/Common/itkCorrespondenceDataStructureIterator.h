/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCorrespondenceDataStructureIterator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkCorrespondenceDataStructureIterator_h
#define __itkCorrespondenceDataStructureIterator_h

namespace itk
{

/** \class CorrespondenceDataStructureIterator
 * \brief An iterator designed to easily traverse an itkCorrespondenceDataStructure.
 * 
 * \ingroup 
 */
template<class TStructureType>
class CorrespondenceDataStructureIterator {
public:
  /** Standard class typedefs. */
  typedef CorrespondenceDataStructureIterator Self;

  /** Get the dimension (size) of the index. */
  static unsigned int GetIteratorDimension() 
  {return TStructureType::dim;}

  /** Is the iterator at the end of the region? */
  bool IsAtEnd();

  /** Walk forward one index. (prefix) */
  void operator++(){GoToNext();}

  /** Walk forward one index. (postfix) */
  void operator++(int){GoToNext();}

  /** Goes to the next corresponding node clique in the structure, moving on to the next base node clique if necessary. */
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
  typedef typename TStructureType::ItemType ItemType;
  typedef typename TStructureType::SecondaryNodeListType SecondaryNodeListType;
  typedef typename TStructureType::NodeListType NodeListType;

  typedef typename CorrespondingListType::iterator CorrespondingListIterator;
  typedef typename SecondaryNodeListType::iterator SecondaryNodeListIterator;
  typedef typename NodeListType::iterator NodeListIterator;

  /** Get m_CorrespondingListPointer.  */
  CorrespondingListType * GetCorrespondingListPointer(){return m_CorrespondingListPointer;}

  CorrespondingListIterator m_CorrespondingListIterator;
  SecondaryNodeListIterator m_SecondaryListIterator;
  typename  TStructureType::NodeListType::iterator m_NodeListIterator;

protected:

  /** Is the iterator at the end of its walk? */
  bool m_IsAtEnd;
  TStructureType * m_Structure;
  ItemType * m_CorrespondingNodePointer;
  CorrespondingListType * m_CorrespondingListPointer;
  SecondaryNodeListType * m_SecondaryListPointer;
  NodeListType * m_NodeListPointer;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCorrespondenceDataStructureIterator.txx"
#endif

#endif 
