/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCorrespondenceDataStructureIterator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkCorrespondenceDataStructureIterator_txx
#define _itkCorrespondenceDataStructureIterator_txx

#include "itkCorrespondenceDataStructureIterator.h"

namespace itk
{
/**
 * Constructor.  Initializes iterators, pointers, and m_IsAtEnd.
 */
template <typename TStructureType>
CorrespondenceDataStructureIterator<TStructureType>
::CorrespondenceDataStructureIterator(TStructureType *StructurePtr)
{
  m_Structure = StructurePtr;
  m_NodeListPointer = StructurePtr->m_NodeList;
  m_NodeListIterator = m_NodeListPointer->begin();
  m_SecondaryListPointer = &(*m_NodeListIterator);
  m_SecondaryListIterator = m_SecondaryListPointer->begin();
  m_CorrespondingListPointer = &(*m_SecondaryListIterator);
  m_CorrespondingListIterator = m_CorrespondingListPointer->begin();
  
  m_IsAtEnd = false;
}

/**
 * Destructor.
 */
template <typename TStructureType>
CorrespondenceDataStructureIterator<TStructureType>
::~CorrespondenceDataStructureIterator()
{
}

/**
 * Used to verify that the iterator is at the end of the data structure.
 */
template <typename TStructureType>
bool
CorrespondenceDataStructureIterator<TStructureType>
::IsAtEnd()
{
  return m_IsAtEnd;
}

/**
 * Goes to the next corresponding node clique in the structure, moving on to the next base node clique if necessary. 
 */
template <typename TStructureType>
void
CorrespondenceDataStructureIterator<TStructureType>
::GoToNext()
{
  m_CorrespondingListIterator++;
  
  if(m_CorrespondingListIterator == m_CorrespondingListPointer->end())
    {
    m_SecondaryListIterator++;
    if(m_SecondaryListIterator != m_SecondaryListPointer->end())
      {
      m_CorrespondingListPointer = &(*m_SecondaryListIterator);
      m_CorrespondingListIterator = m_CorrespondingListPointer->begin();
      }
    else if(m_SecondaryListIterator == m_SecondaryListPointer->end())
      {
      m_NodeListIterator++;

      if(m_NodeListIterator != m_NodeListPointer->end())
        {
        m_SecondaryListPointer = &(*m_NodeListIterator);
        m_SecondaryListIterator = m_SecondaryListPointer->begin();
    
        m_CorrespondingListPointer = &(*m_SecondaryListIterator);
        m_CorrespondingListIterator = m_CorrespondingListPointer->begin();
        }
      else if(m_NodeListIterator == m_NodeListPointer->end())
        {
        m_IsAtEnd = true;
        }
      }
    }
}

/**
 * Goes to the next base node clique. 
 */
template <typename TStructureType>
void
CorrespondenceDataStructureIterator<TStructureType>
::GoToNextBaseGroup()
{
  m_SecondaryListIterator++;
  if(m_SecondaryListIterator != m_SecondaryListPointer->end())
    {
    m_CorrespondingListPointer = &(*m_SecondaryListIterator);
    m_CorrespondingListIterator = m_CorrespondingListPointer->begin();
    }
  else if(m_SecondaryListIterator == m_SecondaryListPointer->end())
    {
    m_NodeListIterator++;

    if(m_NodeListIterator != m_NodeListPointer->end())
      {
      m_SecondaryListPointer = &(*m_NodeListIterator);
      m_SecondaryListIterator = m_SecondaryListPointer->begin();
    
      m_CorrespondingListPointer = &(*m_SecondaryListIterator);
      m_CorrespondingListIterator = m_CorrespondingListPointer->begin();
      }
    else if(m_NodeListIterator == m_NodeListPointer->end())
      {
      m_IsAtEnd = true;
      }
    }
}

/**
 * Resets the iterator to the default settings/placement.
 */
template <typename TStructureType>
void
CorrespondenceDataStructureIterator<TStructureType>
::Reset()
{
  m_IsAtEnd = false;

  m_NodeListPointer = m_Structure->m_NodeList;
  m_NodeListIterator = m_NodeListPointer->begin();

  m_SecondaryListPointer = &(*m_NodeListIterator);
  m_SecondaryListIterator = m_SecondaryListPointer->begin();

  m_CorrespondingListPointer = &(*m_SecondaryListIterator);
  m_CorrespondingListIterator = m_CorrespondingListPointer->begin();
}

} // end namespace itk

#endif
