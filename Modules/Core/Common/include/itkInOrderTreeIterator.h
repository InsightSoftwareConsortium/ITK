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
#ifndef itkInOrderTreeIterator_h
#define itkInOrderTreeIterator_h

#include "itkTreeIteratorBase.h"

namespace itk
{
template< typename TTreeType >
class InOrderTreeIterator:public TreeIteratorBase< TTreeType >
{
public:

  /** Typedefs */
  typedef InOrderTreeIterator               Self;
  typedef TreeIteratorBase< TTreeType >     Superclass;
  typedef TTreeType                         TreeType;
  typedef typename TTreeType::ValueType     ValueType;
  typedef typename Superclass::TreeNodeType TreeNodeType;
  typedef typename Superclass::NodeType     NodeType;

  /** Constructors */
  InOrderTreeIterator(TreeType & start);
  InOrderTreeIterator(TreeType *tree, TreeNodeType *start = ITK_NULLPTR);

  /** Get the type of iterator */
  NodeType GetType() const;

  /** Clone function */
  TreeIteratorBase< TTreeType > * Clone();

protected:

  /** Return the next node */
  const ValueType & Next();

  /** Return true if the next node exists */
  bool HasNext() const;

private:

  /** Find the next node */
  const TreeNodeType * FindNextNode() const;
};

/** Constructor */
template< typename TTreeType >
InOrderTreeIterator< TTreeType >::InOrderTreeIterator(TTreeType & start):
  TreeIteratorBase< TTreeType >(start)
{}

/** Constructor */
template< typename TTreeType >
InOrderTreeIterator< TTreeType >::InOrderTreeIterator(TTreeType *tree, TreeNodeType *start):
  TreeIteratorBase< TTreeType >(tree, start)
{}

/** Get the type of the iterator */
template< typename TTreeType >
typename InOrderTreeIterator< TTreeType >::NodeType
InOrderTreeIterator< TTreeType >::GetType() const
{
  return TreeIteratorBase< TTreeType >::INORDER;
}

/** Return true if the next node exists */
template< typename TTreeType >
bool InOrderTreeIterator< TTreeType >::HasNext() const
{
  if ( const_cast< TreeNodeType * >( FindNextNode() ) != ITK_NULLPTR )
    {
    return true;
    }
  return false;
}

/** Return the next node */
template< typename TTreeType >
const typename InOrderTreeIterator< TTreeType >::ValueType &
InOrderTreeIterator< TTreeType >::Next()
{
  this->m_Position =  const_cast< TreeNodeType * >( FindNextNode() );
  return this->m_Position->Get();
}

/** Find the next node */
template< typename TTreeType >
const typename InOrderTreeIterator< TTreeType >::TreeNodeType *
InOrderTreeIterator< TTreeType >::FindNextNode() const
{
  if ( this->m_Position == ITK_NULLPTR )
    {
    return ITK_NULLPTR;
    }

  if ( this->m_Position->HasChildren() )
    {
    return this->m_Position->GetChild(0);
    }

  if ( !this->m_Position->HasParent() )
    {
    return ITK_NULLPTR;
    }

  TreeNodeType *child = this->m_Position;
  TreeNodeType *parent = this->m_Position->GetParent();

  int childPosition = parent->ChildPosition(child);
  int lastChildPosition = parent->CountChildren() - 1;

  while ( childPosition < lastChildPosition )
    {
    TreeNodeType *help = parent->GetChild(childPosition + 1);
    if ( help != ITK_NULLPTR )
      {
      return help;
      }
    childPosition++;
    }

  while ( parent->HasParent() )
    {
    child = parent;
    parent = parent->GetParent();

    // Subtree
    if ( parent->ChildPosition(this->m_Root) >= 0 )
      {
      return ITK_NULLPTR;
      }
    childPosition = parent->ChildPosition(child);
    lastChildPosition = parent->CountChildren() - 1;

    while ( childPosition < lastChildPosition )
      {
      TreeNodeType *help = parent->GetChild(childPosition + 1);
      if ( help != ITK_NULLPTR )
        {
        return help;
        }
      }
    }
  return ITK_NULLPTR;
}

/** Clone function */
template< typename TTreeType >
TreeIteratorBase< TTreeType > *InOrderTreeIterator< TTreeType >::Clone()
{
  InOrderTreeIterator *clone = new InOrderTreeIterator( const_cast< TTreeType * >( this->m_Tree ) );

  *clone = *this;
  return clone;
}
} // end namespace itk

#endif
