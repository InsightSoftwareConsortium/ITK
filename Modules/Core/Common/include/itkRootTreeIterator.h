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
#ifndef itkRootTreeIterator_h
#define itkRootTreeIterator_h

#include "itkTreeIteratorBase.h"

namespace itk
{
template< typename TTreeType >
class RootTreeIterator:public TreeIteratorBase< TTreeType >
{
public:

  /** Typedefs */
  typedef TreeIteratorBase< TTreeType >     Superclass;
  typedef TTreeType                         TreeType;
  typedef typename TTreeType::ValueType     ValueType;
  typedef typename Superclass::TreeNodeType TreeNodeType;
  typedef typename Superclass::NodeType     NodeType;

  /** Constructor */
  RootTreeIterator(TreeType *tree, const TreeNodeType *start = ITK_NULLPTR);

  /** Return the type of the iterator */
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
RootTreeIterator< TTreeType >::RootTreeIterator(TTreeType *tree, const TreeNodeType *start):
  TreeIteratorBase< TTreeType >(tree, start)
{
  if ( start )
    {
    this->m_Begin = const_cast< TreeNode< ValueType > * >( start );
    }
  this->m_Root = tree->GetRoot();
  this->m_Position = this->m_Begin;
}

/** Return the type of the iterator */
template< typename TTreeType >
typename RootTreeIterator< TTreeType >::NodeType
RootTreeIterator< TTreeType >::GetType() const
{
  return TreeIteratorBase< TTreeType >::ROOT;
}

/** Return true if the next node exists */
template< typename TTreeType >
bool
RootTreeIterator< TTreeType >::HasNext() const
{
  if ( const_cast< TreeNodeType * >( FindNextNode() ) != ITK_NULLPTR )
    {
    return true;
    }
  return false;
}

/** Go to the next node */
template< typename TTreeType >
const typename RootTreeIterator< TTreeType >::ValueType &
RootTreeIterator< TTreeType >::Next()
{
  this->m_Position = const_cast< TreeNodeType * >( FindNextNode() );
  return this->m_Position->Get();
}

/** Find the next node */
template< typename TTreeType >
const typename RootTreeIterator< TTreeType >::TreeNodeType *
RootTreeIterator< TTreeType >::FindNextNode() const
{
  if ( this->m_Position == ITK_NULLPTR )
    {
    return ITK_NULLPTR;
    }
  if ( this->m_Position == this->m_Root )
    {
    return ITK_NULLPTR;
    }
  return this->m_Position->GetParent();
}

/** Clone function */
template< typename TTreeType >
TreeIteratorBase< TTreeType > *RootTreeIterator< TTreeType >::Clone()
{
  RootTreeIterator< TTreeType > *clone = new RootTreeIterator< TTreeType >(const_cast< TTreeType * >( this->m_Tree ),
                                                                           this->m_Position);
  *clone = *this;
  return clone;
}
} // end namespace itk

#endif
