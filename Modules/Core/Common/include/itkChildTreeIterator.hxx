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
#ifndef itkChildTreeIterator_hxx
#define itkChildTreeIterator_hxx

#include "itkChildTreeIterator.h"

namespace itk
{

/** Constructor */
template< typename TTreeType >
ChildTreeIterator< TTreeType >::ChildTreeIterator(TTreeType *tree,
                                                  const TreeNodeType *start):
  TreeIteratorBase< TTreeType >(tree, start)
{
  m_ListPosition = 0;
  m_ParentNode = this->m_Position;
  this->m_Position = m_ParentNode->GetChild(m_ListPosition);
  this->m_Begin = this->m_Position;
}

template< typename TTreeType >
ChildTreeIterator< TTreeType >::ChildTreeIterator(
  const TreeIteratorBase< TTreeType > & iterator):
  TreeIteratorBase< TTreeType >( iterator.GetTree(), iterator.GetNode() )
{
  m_ListPosition = 0;
  m_ParentNode = this->m_Position;
  this->m_Position = m_ParentNode->GetChild(m_ListPosition);
}

/** Go to a specific child */
template< typename TTreeType >
bool
ChildTreeIterator< TTreeType >::GoToChild(ChildIdentifier number)
{
  if ( m_ParentNode->GetChild(number) == ITK_NULLPTR )
    {
    return false;
    }

  m_ListPosition = 0;
  m_ParentNode = m_ParentNode->GetChild(number);
  this->m_Position = m_ParentNode->GetChild(m_ListPosition);
  this->m_Begin = this->m_Position;
  return true;
}

/** Go to the parent node */
template< typename TTreeType >
bool
ChildTreeIterator< TTreeType >::GoToParent()
{
  TreeNodeType *parent =  m_ParentNode->GetParent();

  if ( parent == ITK_NULLPTR )
    {
    return false;
    }

  m_ListPosition = 0;
  m_ParentNode = parent;
  this->m_Position = m_ParentNode->GetChild(m_ListPosition);
  this->m_Begin = this->m_Position;
  return true;
}

/** Return the type of the iterator */
template< typename TTreeType >
typename ChildTreeIterator< TTreeType >::NodeType
ChildTreeIterator< TTreeType >::GetType() const
{
  return TreeIteratorBase< TTreeType >::CHILD;
}

/** Return true if the next node exists */
template< typename TTreeType >
bool
ChildTreeIterator< TTreeType >::HasNext() const
{
  if ( m_ListPosition < m_ParentNode->CountChildren() - 1 )
    {
    return true;
    }
  else
    {
    return false;
    }
}

/** Return the next node */
template< typename TTreeType >
const typename ChildTreeIterator< TTreeType >::ValueType &
ChildTreeIterator< TTreeType >::Next()
{
  m_ListPosition++;
  this->m_Position = m_ParentNode->GetChild(m_ListPosition);
  return this->m_Position->Get();
}

/** Clone function */
template< typename TTreeType >
TreeIteratorBase< TTreeType > *ChildTreeIterator< TTreeType >::Clone()
{
  ChildTreeIterator< TTreeType > *clone = new ChildTreeIterator< TTreeType >(
    const_cast< TTreeType * >( this->m_Tree ), this->m_Position);
  *clone = *this;
  return clone;
}
} // namespace

#endif
