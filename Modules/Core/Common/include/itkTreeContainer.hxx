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
#ifndef __itkTreeContainer_hxx
#define __itkTreeContainer_hxx

#include "itkTreeContainer.h"

namespace itk
{
/** Constructor */
template< typename TValueType >
TreeContainer< TValueType >::TreeContainer()
{
  m_Root = NULL;
  this->SetSubtree(false);
  m_DefaultChildrenCount = 2;
}

/** Constructor with default children count */
template< typename TValueType >
TreeContainer< TValueType >::TreeContainer(int dcc)
{
  m_Root = NULL;
  this->SetSubtree(false);
  m_DefaultChildrenCount = dcc;
}

/** Constructor by adding a tree */
template< typename TValueType >
TreeContainer< TValueType >::TreeContainer(TreeContainer< TValueType > & )
{
  m_Root = NULL;
  this->SetSubtree(false);
  m_DefaultChildrenCount = 3;
}

/** Destructor */
template< typename TValueType >
TreeContainer< TValueType >::~TreeContainer()
{}

/** Set the root of the tree */
template< typename TValueType >
bool
TreeContainer< TValueType >::SetRoot(const TValueType element)
{
  m_Root = TreeNodeType::New();
  m_Root->Set(element);
  m_Root->SetParent(NULL);
  return true;
}

/** Set the root of the tree */
template< typename TValueType >
bool
TreeContainer< TValueType >::SetRoot(TreeNode< TValueType > *node)
{
  m_Root = node;
  return true;
}

/** Count the number of nodes in the tree */
template< typename TValueType >
int
TreeContainer< TValueType >::Count() const
{
  if ( !m_Root )
    {
    return 0;
    }
  int                          size = 0;
  PreOrderTreeIterator< Self > it(this, this->m_Root);
  it.GoToBegin();
  while ( !it.IsAtEnd() )
    {
    size++;
    ++it;
    }
  return size;
}

/** Swap the iterators */
template< typename TValueType >
bool
TreeContainer< TValueType >::Swap(IteratorType & v, IteratorType & w)
{
  TreeNode< TValueType > *nv = v.GetNode();
  TreeNode< TValueType > *nw = w.GetNode();

  if ( nv == NULL || nw == NULL )
    {
    return false;
    }
  TreeNode< TValueType > *pv = nv->GetParent();
  TreeNode< TValueType > *pw = nw->GetParent();

  if ( pv == NULL && pw == NULL )
    {
    return false;
    }
  else if ( pv == NULL )
    {
    pw->ReplaceChild(nw, nv);
    m_Root = nw;
    }
  else if ( pw == NULL )
    {
    pv->ReplaceChild(nv, nw);
    m_Root = nv;
    }
  else
    {
    pv->ReplaceChild(nv, nw);
    pw->ReplaceChild(nw, nv);
    }

  nv->SetParent(pw);
  nw->SetParent(pv);

  return true;
}

/** Return true if the tree contains this element */
template< typename TValueType >
bool
TreeContainer< TValueType >::Contains(const TValueType element)
{
  PreOrderTreeIterator< Self > it(this, m_Root);
  it.GoToBegin();
  while ( !it.IsAtEnd() )
    {
    if ( it.Get() == element )
      {
      return true;
      }
    ++it;
    }
  return false;
}

/** Equal operator */
template< typename TValueType >
bool
TreeContainer< TValueType >::operator==(TreeContainer< TValueType > & tree)
{
  PreOrderTreeIterator< Self > it(this, m_Root);
  it.GoToBegin();
  PreOrderTreeIterator< Self > it2( &tree, tree.GetRoot() );
  it2.GoToBegin();

  while ( ( !it.IsAtEnd() ) && ( !it2.IsAtEnd() ) )
    {
    if ( it.Get() != it2.Get() )
      {
      return false;
      }
    ++it;
    ++it2;
    }

  return true;
}

/** Return true if the given element is a leaf of the tree */
template< typename TValueType >
bool
TreeContainer< TValueType >::IsLeaf(TValueType element)
{
  PreOrderTreeIterator< Self > it(this, m_Root);
  it.GoToBegin();
  while ( !it.IsAtEnd() )
    {
    if ( it.Get() == element )
      {
      if ( it.IsLeaf() )
        {
        return true;
        }
      else
        {
        return false;
        }
      }
    }
  return false;
}

/** Return true of the node containing the element is the root */
template< typename TValueType >
bool
TreeContainer< TValueType >::IsRoot(TValueType element)
{
  PreOrderTreeIterator< Self > it(this, m_Root);
  it.GoToBegin();
  while ( !it.IsAtEnd() )
    {
    if ( it.Get() == element )
      {
      if ( !it.HasParent() )
        {
        return true;
        }
      else
        {
        return false;
        }
      }
    ++it;
    }
  return false;
}

/** Clear the tree */
template< typename TValueType >
bool TreeContainer< TValueType >::Clear()
{
  PreOrderTreeIterator< Self > it(this, m_Root);
  bool                         success = it.Remove();
  m_Root = NULL;
  return success;
}

/** Get node given a value */
template< typename TValueType >
const TreeNode< TValueType > *
TreeContainer< TValueType >::GetNode(TValueType val) const
{
  PreOrderTreeIterator< Self > it(this, m_Root);
  it.GoToBegin();
  while ( !it.IsAtEnd() )
    {
    if ( it.Get() == val )
      {
      return it.GetNode();
      }
    ++it;
    }
  return NULL;
}

/** Set the root of the tree from the iterator position */
template< typename TValueType >
bool
TreeContainer< TValueType >::SetRoot(IteratorType & pos)
{
  if ( this->m_SubTree )
    {
    return false;
    }
  TreeNode< TValueType > *node = pos.GetNode();
  if ( node == NULL )
    {
    return false;
    }

  TreeNode< TValueType > *parent = node->GetParent();
  TreeNode< TValueType > *help = NULL;

  if ( parent == NULL )
    {
    return false;
    }

  m_Root = node;
  node->AddChild(parent);
  parent->Remove(node);
  node->SetParent(NULL);
  help = parent->GetParent();
  parent->SetParent(node);
  node = parent;

  while ( help != NULL )
    {
    parent = help;
    help = help->GetParent();
    node->AddChild(parent);
    parent->Remove(node);
    parent->SetParent(node);
    node = parent;
    }
  return true;
}

/** Add a child to a given parent */
template< typename TValueType >
bool
TreeContainer< TValueType >::Add(const TValueType child, const TValueType parent)
{
  if ( !m_Root )
    {
    std::cout << "TreeContainer<TValueType>::Add() : The tree is empty" << std::endl;
    return false;
    }
  // Find the first node in the tree that has the parent value
  PreOrderTreeIterator< Self > it(this, m_Root);
  it.GoToBegin();
  while ( !it.IsAtEnd() )
    {
    if ( it.Get() == parent )
      {
      it.Add(child);
      return true;
      }
    ++it;
    }
  return false;
}

/** Print self */
template< typename TValueType >
void
TreeContainer< TValueType >::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Number of objects = " << this->Count() << std::endl;

  if ( this->Count() > 0 )
    {
    os << indent << "Tree:" << std::endl;
    // Now prints the tree
    PreOrderTreeIterator< Self > it(this, m_Root);
    it.GoToBegin();
    while ( !it.IsAtEnd() )
      {
      if ( it.GetParent() )
        {
        std::cout << it.GetParent()->Get() << " <- ";
        }
      std::cout << it.Get() << std::endl;
      ++it;
      }
    }
}
} // namespace itk

#endif
