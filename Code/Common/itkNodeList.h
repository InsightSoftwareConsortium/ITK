/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNodeList.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
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
 **/

template <typename TItemType>
class NodeList : public std::list< TItemType >
{
public:

  /** Pointer to the item. */
  TItemType * ItemPointer;
  
  /** Store a pointer to the iteme in the list. */
  void SetItemPointer(TItemType* itemPointer) {ItemPointer = itemPointer;}

  /** Get the number of items stored in the list. */
  unsigned long int GetSize()
    {return this->size(); }

  NodeList();
  ~NodeList();

private:

};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNodeList.txx"
#endif

#endif
