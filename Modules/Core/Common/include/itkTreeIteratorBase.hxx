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
#ifndef itkTreeIteratorBase_hxx
#define itkTreeIteratorBase_hxx

#include "itkTreeChangeEvent.h"

/** There are some weird circular #include dependencies between TreeChangeEvent
 * and TreeIteratorBase that cause the HeaderTest to fail without these forward
 * declarations. */
template< typename TTreeType >
class ITK_TEMPLATE_EXPORT TreeNodeChangeEvent;

template< typename TTreeType >
class ITK_TEMPLATE_EXPORT TreeAddEvent;

template< typename TTreeType >
class ITK_TEMPLATE_EXPORT TreePruneEvent;

template< typename TTreeType >
class ITK_TEMPLATE_EXPORT TreeRemoveEvent;

namespace itk
{
/** Constructor */
template< typename TTreeType >
TreeIteratorBase< TTreeType >::TreeIteratorBase(TTreeType *tree, const TreeNodeType *start)
{
  if ( start )
    {
    m_Root = start;
    }
  else
    {
    m_Root = dynamic_cast< const TreeNodeType * >( tree->GetRoot() );
    }

  m_Position = const_cast< TreeNodeType * >( m_Root );
  m_Tree = tree;
  m_Begin = m_Position;
  m_End = ITK_NULLPTR;
}

/** Constructor */
template< typename TTreeType >
TreeIteratorBase< TTreeType >::TreeIteratorBase(const TTreeType *tree, const TreeNodeType *start)
{
  if ( start )
    {
    m_Root = start;
    }
  else
    {
    m_Root = const_cast< TreeNodeType * >( dynamic_cast< const TreeNodeType * >( tree->GetRoot() ) );
    }
  m_Position = const_cast< TreeNodeType * >( m_Root );
  m_Tree = const_cast< TTreeType * >( tree );
  m_Begin = m_Position;
  m_End = ITK_NULLPTR;
}

/** Return the current value of the node */
template< typename TTreeType >
const typename TreeIteratorBase< TTreeType >::ValueType &
TreeIteratorBase< TTreeType >::Get() const
{
  return m_Position->Get();
}

/** Set the current value of the node */
template< typename TTreeType >
void
TreeIteratorBase< TTreeType >::Set(ValueType element)
{
//  itkAssertInDebugAndIgnoreInReleaseMacro(m_Position);
  m_Position->Set(element);
  m_Tree->Modified();
  m_Tree->InvokeEvent( TreeNodeChangeEvent< TTreeType >(*this) );
}

/** Add a value to the node. This creates a new child node */
template< typename TTreeType >
bool
TreeIteratorBase< TTreeType >::Add(ValueType element)
{
  if ( m_Position == ITK_NULLPTR && m_Root == ITK_NULLPTR )
    {
    bool returnValue = const_cast< TTreeType * >( m_Tree )->SetRoot(element);
    // signal AddEvent for self
    m_Root = dynamic_cast< const TreeNodeType * >( const_cast< TTreeType * >( m_Tree )->GetRoot() );
    m_Position =  const_cast< TreeNodeType * >( m_Root );
    m_Tree->Modified();
    m_Tree->InvokeEvent( TreeAddEvent< TTreeType >(*this) );
    return returnValue;
    }
  else if ( m_Position == ITK_NULLPTR )
    {
    return false;
    }

  typename TreeNodeType::Pointer node = TreeNodeType::New();
  node->Set(element);
  m_Position->AddChild(node);
  m_Tree->Modified();

  // signal AddEvent for new child
  TreeIteratorBase< TTreeType > *childIterator = Clone();
  childIterator->m_Position = dynamic_cast< TreeNodeType * >( m_Position->GetChild( m_Position->ChildPosition(node) ) );
  // signal "child has been added deleted"
  m_Tree->InvokeEvent( TreeAddEvent< TTreeType >(*childIterator) );
  delete childIterator;

  return true;
}

/** Add a new element at a given position */
template< typename TTreeType >
bool
TreeIteratorBase< TTreeType >::Add(int itkNotUsed(childPosition), ValueType element)
{
  if ( m_Position )
    {
    typename TreeNodeType::Pointer node = TreeNodeType::New();
    node->Set(element);
    m_Position->AddChild(node);
    m_Tree->Modified();

    // signal AddEvent
    TreeIteratorBase< TTreeType > *childIterator = Clone();
    childIterator->m_Position = dynamic_cast< TreeNodeType * >( m_Position->GetChild( m_Position->ChildPosition(node) ) );
    // signal "child has been added deleted"
    m_Tree->InvokeEvent( TreeAddEvent< TTreeType >(*childIterator) );
    delete childIterator;

    return true;
    }
  return false;
}

/** Return true if the current pointed node is a leaf */
template< typename TTreeType >
bool
TreeIteratorBase< TTreeType >::IsLeaf() const
{
  return !( m_Position->HasChildren() );
}

/** Return true if the current pointed node is a root */
template< typename TTreeType >
bool
TreeIteratorBase< TTreeType >::IsRoot() const
{
  if ( m_Root == ITK_NULLPTR )
    {
    return false;
    }

  if ( m_Position == m_Root )
    {
    return true;
    }
  return false;
}

/** Add a subtree  */
template< typename TTreeType >
bool
TreeIteratorBase< TTreeType >::Add(TTreeType & subTree)
{
  if ( subTree.Count() == 0 )
    {
    return false;
    }

  if ( !subTree.GetRoot() )
    {
    return false;
    }

  if ( m_Root == ITK_NULLPTR )
    {
    m_Root = static_cast< const TreeNodeType * >( subTree.GetRoot() );
    }
  else
    {
    if ( m_Position == ITK_NULLPTR )
      {
      return false;
      }
    m_Position->AddChild( const_cast< TreeNodeType * >( static_cast< const TreeNodeType * >( subTree.GetRoot() ) ) );
    }
  return true;
}

/** Return the subtree */
template< typename TTreeType >
TTreeType *
TreeIteratorBase< TTreeType >::GetSubTree() const
{
  typename TTreeType::Pointer tree = TTreeType::New();
  tree->SetRoot(m_Position);
  tree->SetSubtree(true);
  return tree;
}

/** Return true of the current node has a child */
template< typename TTreeType >
bool
TreeIteratorBase< TTreeType >::HasChild(int number) const
{
  if ( m_Position == ITK_NULLPTR )
    {
    return false;
    }
  if ( m_Position->GetChild(number) != ITK_NULLPTR )
    {
    return true;
    }
  return false;
}

/** Return the current position of the child */
template< typename TTreeType >
int
TreeIteratorBase< TTreeType >::ChildPosition(ValueType element) const
{
  if ( !m_Position )
    {
    return -1;
    }
  return m_Position->ChildPosition(element);
}

/** Remove a child */
template< typename TTreeType >
bool
TreeIteratorBase< TTreeType >::RemoveChild(int number)
{
  if ( !HasChild(number) )
    {
    return false;
    }
  TreeNodeType *child = dynamic_cast< TreeNodeType * >( m_Position->GetChild(number) );

  if ( child != ITK_NULLPTR )
    {
    // signal PruneEvent (node plus all children are removed)
    TreeIteratorBase< TTreeType > *childIterator = Clone();
    childIterator->m_Position = child;
    // signal "child has been added deleted"
    m_Tree->InvokeEvent( TreePruneEvent< TTreeType >(*childIterator) );
    delete childIterator;

    // and really remove child (and subitems)
    const_cast< TreeNodeType * >( m_Position )->Remove(child);
    m_Tree->Modified();
    return true;
    }
  return false;
}

/** Count the number of children */
template< typename TTreeType >
int
TreeIteratorBase< TTreeType >::CountChildren() const
{
  if ( m_Position == ITK_NULLPTR )
    {
    return -1;
    }
  return m_Position->CountChildren();
}

/** Return true of the pointed node has a parent */
template< typename TTreeType >
bool
TreeIteratorBase< TTreeType >::HasParent() const
{
  return ( m_Position != ITK_NULLPTR && m_Position->GetParent() != ITK_NULLPTR );
}

/** Disconnect the tree */
template< typename TTreeType >
bool
TreeIteratorBase< TTreeType >::Disconnect()
{
  if ( m_Position == ITK_NULLPTR )
    {
    return false;
    }

  if ( m_Position->HasParent() == false )
    {
    return false;
    }

  //keep node alive just a bit longer
  typename TreeNodeType::Pointer position = m_Position;

  TreeNodeType *parent = dynamic_cast< TreeNodeType * >( m_Position->GetParent() );
  parent->Remove( const_cast< TreeNodeType * >( m_Position ) );
  m_Tree->Modified();

  while ( m_Position->CountChildren() > 0 )
    {
    // always add first child in list, because AddChild() removes the added node
    // from
    // its former parent (== m_position)
    TreeNodeType *child = dynamic_cast< TreeNodeType * >( m_Position->GetChild(0) );
    parent->AddChild(child);
    }

  m_Tree->InvokeEvent( TreeRemoveEvent< TTreeType >(*this) );

  m_Position = ITK_NULLPTR;
  return true;
}

/** Return the children list */
template< typename TTreeType >
TreeIteratorBase< TTreeType > *
TreeIteratorBase< TTreeType >::Children()
{
  itkGenericOutputMacro("Not implemented yet");
  ::itk::ExceptionObject e_(__FILE__, __LINE__, "Not implemented yet", ITK_LOCATION);
  throw e_; /* Explicit naming to work around Intel compiler bug.  */
  return ITK_NULLPTR;
}

/** Return the first parent found */
template< typename TTreeType >
const typename TreeIteratorBase< TTreeType >::TreeNodeType *
TreeIteratorBase< TTreeType >::GetParent() const
{
  if ( m_Position == ITK_NULLPTR )
    {
    return ITK_NULLPTR;
    }

  return m_Position->GetParent();
}

/** Return the list of parents */
template< typename TTreeType >
TreeIteratorBase< TTreeType > *TreeIteratorBase< TTreeType >::Parents()
{
  itkGenericOutputMacro("Not implemented yet");
  ::itk::ExceptionObject e_(__FILE__, __LINE__, "Not implemented yet", ITK_LOCATION);
  throw e_; /* Explicit naming to work around Intel compiler bug.  */
  return ITK_NULLPTR;
}

/** Go to a child */
template< typename TTreeType >
bool TreeIteratorBase< TTreeType >::GoToChild(ChildIdentifier number)
{
  if ( m_Position == ITK_NULLPTR )
    {
    return false;
    }

  TreeNodeType *next = dynamic_cast< TreeNodeType * >( m_Position->GetChild(number) );

  if ( next == ITK_NULLPTR )
    {
    return false;
    }
  m_Position = next;
  return true;
}

/** Go to a parent */
template< typename TTreeType >
bool TreeIteratorBase< TTreeType >::GoToParent()
{
  if ( m_Position == ITK_NULLPTR )
    {
    return false;
    }

  if ( !m_Position->HasParent() )
    {
    return false;
    }

  m_Position = dynamic_cast< TreeNodeType * >( m_Position->GetParent() );
  return true;
}

/** Get a child given a number */
template< typename TTreeType >
TreeIteratorBase< TTreeType > *TreeIteratorBase< TTreeType >::GetChild(int number) const
{
  if ( !m_Position )
    {
    return ITK_NULLPTR;
    }

  TreeNodeType *child = dynamic_cast< TreeNodeType * >( m_Position->GetChild(number) );

  if ( !child )
    {
    return ITK_NULLPTR;
    }
//    return new WalkTreeIterator<ValueType,P>( child, m_Root, m_Tree, getType()
// );
  return ITK_NULLPTR;
}

/** Count the number of nodes from the beginning */
template< typename TTreeType >
int TreeIteratorBase< TTreeType >::Count()
{
  int size = 0;

  this->GoToBegin();
  if ( !m_Position->HasChildren() )
    {
    return 0;
    }
  while ( this->Next() )
    {
    size++;
    }
  return size;
}

/** Get the node pointed by the iterator */
template< typename TTreeType >
typename TreeIteratorBase< TTreeType >::TreeNodeType *
TreeIteratorBase< TTreeType >::GetNode()
{
  return const_cast< TreeNodeType * >( m_Position );
}

/** Get the node pointed by the iterator */
template< typename TTreeType >
const typename TreeIteratorBase< TTreeType >::TreeNodeType *
TreeIteratorBase< TTreeType >::GetNode() const
{
  return m_Position;
}

/** Get the root */
template< typename TTreeType >
typename TreeIteratorBase< TTreeType >::TreeNodeType *
TreeIteratorBase< TTreeType >::GetRoot()
{
  return const_cast< TreeNodeType * >( m_Root );
}

/** Get the root (const) */
template< typename TTreeType >
const typename TreeIteratorBase< TTreeType >::TreeNodeType *
TreeIteratorBase< TTreeType >::GetRoot() const
{
  return m_Root;
}

/** Remove a specific node (and its child nodes!) */
template< typename TTreeType >
bool
TreeIteratorBase< TTreeType >::Remove()
{
  if ( m_Position == ITK_NULLPTR )
    {
    return false;
    }

  //keep node alive just a bit longer (for the notification)
  typename TreeNodeType::Pointer position = m_Position;

  if ( m_Position->HasParent() )
    {
    TreeNodeType *parent = m_Position->GetParent();
    // removes this node (and implicitly all children, too)
    parent->Remove(m_Position);
    }
  else if ( m_Root == m_Position )
    {
    m_Root = ITK_NULLPTR;
    m_Tree->SetRoot( (TreeNodeType *)ITK_NULLPTR );
    // this won't do anything if root is already != ITK_NULLPTR  ==> root cannot be
    // removed
    }

  m_Position->SetParent(ITK_NULLPTR); // we don't have a parent anymore
  m_Tree->InvokeEvent( TreePruneEvent< TTreeType >(*this) );
  while ( m_Position->CountChildren() > 0 )  // remove all children
    {
    //always remove first child (id 0)
    TreeNodeType *child = dynamic_cast< TreeNodeType * >( m_Position->GetChild(0) );
    m_Position->Remove(child);
    }

  position = ITK_NULLPTR;
  m_Position = ITK_NULLPTR;  // Smart pointer, deletes *m_Position

  m_Tree->Modified();

  return true;
}

/** Return the tree */
template< typename TTreeType >
TTreeType *
TreeIteratorBase< TTreeType >::GetTree() const
{
  return m_Tree;
}
} // namespace itk

#endif
