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
#ifndef itkCorrespondenceDataStructureIterator_hxx
#define itkCorrespondenceDataStructureIterator_hxx

#include "itkCorrespondenceDataStructureIterator.h"

namespace itk
{
/** Constructor.  Initializes iterators, pointers, and m_IsAtEnd. */
template< typename TStructureType >
CorrespondenceDataStructureIterator< TStructureType >
::CorrespondenceDataStructureIterator(TStructureType *StructurePtr)
{
  m_Structure = StructurePtr;
  m_NodeListPointer = StructurePtr->m_NodeList;
  m_NodeListIterator = m_NodeListPointer->begin();
  m_SecondaryListPointer = &( *m_NodeListIterator );
  m_SecondaryListIterator = m_SecondaryListPointer->begin();
  m_CorrespondingListPointer = &( *m_SecondaryListIterator );
  m_CorrespondingListIterator = m_CorrespondingListPointer->begin();

  m_IsAtEnd = false;
}

/** Destructor. */
template< typename TStructureType >
CorrespondenceDataStructureIterator< TStructureType >
::~CorrespondenceDataStructureIterator()
{}

/** Used to verify that the iterator is at the end of the data structure. */
template< typename TStructureType >
bool
CorrespondenceDataStructureIterator< TStructureType >
::IsAtEnd()
{
  return m_IsAtEnd;
}

/** Goes to the next corresponding node clique in the structure,
 *  moving on to the next base node clique if necessary. */
template< typename TStructureType >
void
CorrespondenceDataStructureIterator< TStructureType >
::GoToNext()
{
  m_CorrespondingListIterator++;

  if ( m_CorrespondingListIterator == m_CorrespondingListPointer->end() )
    {
      this->GoToNextBaseGroup();
    }
}

/** Goes to the next base node clique. */
template< typename TStructureType >
void
CorrespondenceDataStructureIterator< TStructureType >
::GoToNextBaseGroup()
{
  m_SecondaryListIterator++;
  if ( m_SecondaryListIterator != m_SecondaryListPointer->end() )
    {
    m_CorrespondingListPointer = &( *m_SecondaryListIterator );
    m_CorrespondingListIterator = m_CorrespondingListPointer->begin();
    }
  else if ( m_SecondaryListIterator == m_SecondaryListPointer->end() )
    {
    m_NodeListIterator++;

    if ( m_NodeListIterator != m_NodeListPointer->end() )
      {
      m_SecondaryListPointer = &( *m_NodeListIterator );
      m_SecondaryListIterator = m_SecondaryListPointer->begin();

      m_CorrespondingListPointer = &( *m_SecondaryListIterator );
      m_CorrespondingListIterator = m_CorrespondingListPointer->begin();
      }
    else if ( m_NodeListIterator == m_NodeListPointer->end() )
      {
      m_IsAtEnd = true;
      }
    }
}

/** Resets the iterator to the default settings/placement.*/
template< typename TStructureType >
void
CorrespondenceDataStructureIterator< TStructureType >
::Reset()
{
  m_IsAtEnd = false;

  m_NodeListPointer = m_Structure->m_NodeList;
  m_NodeListIterator = m_NodeListPointer->begin();

  m_SecondaryListPointer = &( *m_NodeListIterator );
  m_SecondaryListIterator = m_SecondaryListPointer->begin();

  m_CorrespondingListPointer = &( *m_SecondaryListIterator );
  m_CorrespondingListIterator = m_CorrespondingListPointer->begin();
}
} // end namespace itk

#endif
