/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSecondaryNodeList.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
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
 * */

template <typename TItemType, int VCliqueSize>
class SecondaryNodeList : public std::list<TItemType>
{
public:

  /** Pointer to the medial node. */
  TItemType * ItemPointer;

  /** Set the medial node pointer for the corresponding node. */
  void SetNodePointer(TItemType* itemPointer) {ItemPointer = itemPointer;}

  /** Get the number of items stored in the list. */
  unsigned long int GetSize() {return this->size();}

  //return the index of the base node in the clique
  int GetIndex() {return m_Index;}
  void SetIndex(int value) {m_Index = value;}

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
