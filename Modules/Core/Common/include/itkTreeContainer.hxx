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
#ifndef itkTreeContainer_hxx
#define itkTreeContainer_hxx

#include "itkTreeContainer.h"

namespace itk
{
/** Constructor */
template< typename TValue >
TreeContainer< TValue >::TreeContainer()
{
  m_Root = ITK_NULLPTR;
  this->SetSubtree(false);
  m_DefaultChildrenCount = 2;
}

/** Constructor with default children count */
template< typename TValue >
TreeContainer< TValue >::TreeContainer(int dcc)
{
  m_Root = ITK_NULLPTR;
  this->SetSubtree(false);
  m_DefaultChildrenCount = dcc;
}

/** Constructor by adding a tree */
template< typename TValue >
TreeContainer< TValue >::TreeContainer(TreeContainer< TValue > & )
{
  m_Root = ITK_NULLPTR;
  this->SetSubtree(false);
  m_DefaultChildrenCount = 3;
}

/** Destructor */
template< typename TValue >
TreeContainer< TValue >::~TreeContainer()
{}

/** Set the root of the tree */
template< typename TValue >
bool
TreeContainer< TValue >::SetRoot(const TValue element)
{
  m_Root = TreeNodeType::New();
  m_Root->Set(element);
  m_Root->SetParent(ITK_NULLPTR);
  return true;
}

/** Set the root of the tree */
template< typename TValue >
bool
TreeContainer< TValue >::SetRoot(TreeNode< TValue > *node)
{
  m_Root = node;
  return true;
}

/** Count the number of nodes in the tree */
template< typename TValue >
int
TreeContainer< TValue >::Count() const
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
template< typename TValue >
bool
TreeContainer< TValue >::Swap(IteratorType & v, IteratorType & w)
{
  TreeNode< TValue > *nv = v.GetNode();
  TreeNode< TValue > *nw = w.GetNode();

  if ( nv == ITK_NULLPTR || nw == ITK_NULLPTR )
    {
    return false;
    }
  TreeNode< TValue > *pv = nv->GetParent();
  TreeNode< TValue > *pw = nw->GetParent();

  if ( pv == ITK_NULLPTR && pw == ITK_NULLPTR )
    {
    return false;
    }
  else if ( pv == ITK_NULLPTR )
    {
    pw->ReplaceChild(nw, nv);
    m_Root = nw;
    }
  else if ( pw == ITK_NULLPTR )
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
template< typename TValue >
bool
TreeContainer< TValue >::Contains(const TValue element)
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
template< typename TValue >
bool
TreeContainer< TValue >::operator==(TreeContainer< TValue > & tree)
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
template< typename TValue >
bool
TreeContainer< TValue >::IsLeaf(TValue element)
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
template< typename TValue >
bool
TreeContainer< TValue >::IsRoot(TValue element)
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
template< typename TValue >
bool TreeContainer< TValue >::Clear()
{
  PreOrderTreeIterator< Self > it(this, m_Root);
  bool                         success = it.Remove();
  m_Root = ITK_NULLPTR;
  return success;
}

/** Get node given a value */
template< typename TValue >
const TreeNode< TValue > *
TreeContainer< TValue >::GetNode(TValue val) const
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
  return ITK_NULLPTR;
}

/** Set the root of the tree from the iterator position */
template< typename TValue >
bool
TreeContainer< TValue >::SetRoot(IteratorType & pos)
{
  if ( this->m_SubTree )
    {
    return false;
    }
  TreeNode< TValue > *node = pos.GetNode();
  if ( node == ITK_NULLPTR )
    {
    return false;
    }

  TreeNode< TValue > *parent = node->GetParent();
  TreeNode< TValue > *help = ITK_NULLPTR;

  if ( parent == ITK_NULLPTR )
    {
    return false;
    }

  m_Root = node;
  node->AddChild(parent);
  parent->Remove(node);
  node->SetParent(ITK_NULLPTR);
  help = parent->GetParent();
  parent->SetParent(node);
  node = parent;

  while ( help != ITK_NULLPTR )
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
template< typename TValue >
bool
TreeContainer< TValue >::Add(const TValue child, const TValue parent)
{
  if ( !m_Root )
    {
    std::cout << "TreeContainer<TValue>::Add() : The tree is empty" << std::endl;
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
template< typename TValue >
void
TreeContainer< TValue >::PrintSelf(std::ostream & os, Indent indent) const
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
