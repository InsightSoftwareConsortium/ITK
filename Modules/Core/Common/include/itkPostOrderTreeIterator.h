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
#ifndef itkPostOrderTreeIterator_h
#define itkPostOrderTreeIterator_h

#include "itkTreeIteratorBase.h"

namespace itk
{
template< typename TTreeType >
class PostOrderTreeIterator:public TreeIteratorBase< TTreeType >
{
public:

  /** Typedefs */
  using Self = PostOrderTreeIterator;
  using Superclass = TreeIteratorBase< TTreeType >;
  using TreeType = TTreeType;
  using ValueType = typename TTreeType::ValueType;
  using TreeNodeType = typename Superclass::TreeNodeType;
  using NodeType = typename Superclass::NodeType;

  /** Constructor */
  PostOrderTreeIterator(TreeType *tree);

  /** Get the type of the iterator */
  NodeType GetType() const override;

  /** Clone function */
  TreeIteratorBase< TTreeType > * Clone() override;

protected:
  /** Return the next node */
  const ValueType & Next() override;

  /** Return true if the next node exists */
  bool HasNext() const override;

protected:

  const TreeNodeType * FindNextNode() const;

  const TreeNodeType * FindMostRightLeaf(TreeNodeType *node) const;

  const TreeNodeType * FindSister(TreeNodeType *node) const;
};

/** Constructor */
template< typename TTreeType >
PostOrderTreeIterator< TTreeType >::PostOrderTreeIterator(TTreeType *tree):
  TreeIteratorBase< TTreeType >(tree, nullptr)
{
  if ( tree->GetRoot() == nullptr )
    {
    this->m_Begin = nullptr;
    }
  else
    {
    const auto * root = dynamic_cast<const TreeNodeType *>(tree->GetRoot());
    if(root == nullptr)
      {
      itkGenericExceptionMacro(<< "Can't downcast root node to TreeNodeType *");
      }
    this->m_Position = const_cast<TreeNodeType *>(root);
    this->m_Position = const_cast< TreeNodeType * >( FindMostRightLeaf(this->m_Position) );
    this->m_Begin = this->m_Position;
    }
}

/** Return the type of the iterator */
template< typename TTreeType >
typename PostOrderTreeIterator< TTreeType >::NodeType
PostOrderTreeIterator< TTreeType >::GetType() const
{
  return TreeIteratorBase< TTreeType >::POSTORDER;
}

/** Return true if the next node exists */
template< typename TTreeType >
bool
PostOrderTreeIterator< TTreeType >::HasNext() const
{
  if ( const_cast< TreeNodeType * >( FindNextNode() ) != nullptr )
    {
    return true;
    }
  return false;
}

/** Go to the next node */
template< typename TTreeType >
const typename PostOrderTreeIterator< TTreeType >::ValueType &
PostOrderTreeIterator< TTreeType >::Next()
{
  this->m_Position = const_cast< TreeNodeType * >( FindNextNode() );
  return this->m_Position->Get();
}

/** Find the next node */
template< typename TTreeType >
const typename PostOrderTreeIterator< TTreeType >::TreeNodeType *
PostOrderTreeIterator< TTreeType >::FindNextNode() const
{
  if ( this->m_Position == nullptr || this->m_Position == this->m_Root )
    {
    return nullptr;
    }
  auto * sister = const_cast< TreeNodeType * >( FindSister(this->m_Position) );

  if ( sister != nullptr )
    {
    return FindMostRightLeaf(sister);
    }
  if(this->m_Position->GetParent() == nullptr)
    {
    return nullptr;
    }
  auto * rval = dynamic_cast<TreeNodeType *>(this->m_Position->GetParent());
  if(rval == nullptr)
    {
      itkGenericExceptionMacro(<< "Can't downcast to TreeNodeType *");
    }
  return rval;
}

/** Find the sister node */
template< typename TTreeType >
const typename PostOrderTreeIterator< TTreeType >::TreeNodeType *
PostOrderTreeIterator< TTreeType >::FindSister(TreeNodeType *node) const
{
  if ( !node->HasParent() )
    {
    return nullptr;
    }

  auto * parent = dynamic_cast<TreeNodeType *>(node->GetParent());
  if(parent == nullptr)
    {
    itkGenericExceptionMacro(<< "Can't downcast to TreeNodeType *");
    }

  int childPosition = parent->ChildPosition(node);
  int lastChildPosition = parent->CountChildren() - 1;

  while ( childPosition < lastChildPosition )
    {
    if(parent->GetChild(childPosition + 1) == nullptr)
      {
      childPosition++;
      }
    else
      {
      auto * sister = dynamic_cast<TreeNodeType *>(parent->GetChild(childPosition + 1));
      if ( sister == nullptr)
      {
      itkGenericExceptionMacro(<< "Can't downcast to TreeNodeType *");
      }
      return sister;
      }
    }
  return nullptr;
}

/** Find the most right leaf */
template< typename TTreeType >
const typename PostOrderTreeIterator< TTreeType >::TreeNodeType *
PostOrderTreeIterator< TTreeType >::FindMostRightLeaf(TreeNodeType *node) const
{
  while ( node->HasChildren() )
    {
    TreeNodeType *helpNode;
    int           childCount = node->CountChildren();
    int           i = 0;

    do
      {
      if(node->GetChild(i) == nullptr)
        {
        helpNode = nullptr;
        }
      else
        {
        helpNode = dynamic_cast<TreeNodeType *>(node->GetChild(i));
       if(helpNode == nullptr)
          {
          itkGenericExceptionMacro(<< "Can't downcast to TreeNodeType *");
          }
        }
      i++;
      }
    while ( helpNode == nullptr && i < childCount );

    if ( helpNode == nullptr )
      {
      return node;
      }
    node = helpNode;
    }
  return node;
}

/** Clone function */
template< typename TTreeType >
TreeIteratorBase< TTreeType > *PostOrderTreeIterator< TTreeType >::Clone()
{
  auto * clone = new PostOrderTreeIterator< TTreeType >( const_cast< TTreeType * >( this->m_Tree ) );
  *clone = *this;
  return clone;
}
} // end namespace itk

#endif
