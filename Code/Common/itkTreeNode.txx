/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTreeNode.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkTreeNode_txx
#define _itkTreeNode_txx

#include "itkTreeNode.h"

namespace itk
{

/** Constructor */
template <class TValueType>
TreeNode<TValueType>::TreeNode()
{
  m_Parent = NULL;
}

/** Destructor */
template <class TValueType>
TreeNode<TValueType>::~TreeNode() 
{
  if ( m_Parent )
    {
    m_Parent->Remove(this);
    }
    
 for ( size_t i=m_Children.size() ; i > 0; i-- )
   {
   m_Children[i-1]->SetParent(NULL);
   m_Children[i-1] = 0;
   }
  m_Children.clear();
  m_Parent = NULL;
  m_Data = 0;
}

/** Return the parent node */
template <class TValueType>
TreeNode<TValueType>* 
TreeNode<TValueType>
::GetParent( ) const 
{
  return m_Parent;
}

/** Get a child */
template <class TValueType>
TreeNode<TValueType>* 
TreeNode<TValueType>
::GetChild( int number ) const 
{
  if ( (unsigned int)number < m_Children.size() )
    {
    return m_Children[number];
    }
  else
    {
    return NULL;
    }
}

/** Set the value of a node */
template <class TValueType>
TValueType TreeNode<TValueType>::Set(const TValueType data)
{
  TValueType help = m_Data;
  m_Data = data;
  return help;
}

/** Get the data of node */
template <class TValueType>
const TValueType& TreeNode<TValueType>::Get() const 
{
  return m_Data;
}

/** Return true if has a parent */
template <class TValueType>
bool 
TreeNode<TValueType>::HasParent() const 
{
  return (m_Parent)?true:false;
}

/** Set the parent node */
template <class TValueType>
void 
TreeNode<TValueType>::SetParent( TreeNode<TValueType>* node) 
{
  m_Parent = node;
}

/** Return true if the node has children */
template <class TValueType>
bool TreeNode<TValueType>::HasChildren() const 
{
  return (m_Children.size()>0)?true:false;
}

/** Return the number of children */
template <class TValueType>
int 
TreeNode<TValueType>
::CountChildren( ) const 
{
  return static_cast<int>(m_Children.size());
}

/** Remove a child node from the current node */
template <class TValueType>
bool 
TreeNode<TValueType>
::Remove( TreeNode<TValueType> *n ) 
{
  typename std::vector<Pointer>::iterator pos;
  pos = std::find(m_Children.begin(), m_Children.end(), n );
  if ( pos != m_Children.end() ) 
    {
    m_Children.erase(pos);
    n->SetParent(NULL);
    return true;
    }
  return false;
}

/** Replace a child by a new one */
template <class TValueType>
bool TreeNode<TValueType>::ReplaceChild( TreeNode<TValueType> *oldChild, TreeNode<TValueType> *newChild )
{
  int size = m_Children.size();

  for ( int i=0; i<size; i++ )
    {
    if ( m_Children[i] == oldChild )
      {
      m_Children[i] = newChild;
      return true;
      }
    }
  return false;
}

/** Return the child position given a node */
template <class TValueType>
int TreeNode<TValueType>::ChildPosition( const TreeNode<TValueType> *node ) const 
{
  for ( unsigned int i=0; i < m_Children.size(); i++ )
    {
    if ( m_Children[i] == node )
      {
      return i;
      }
    }
  return -1;
}

/** Return the child position given an element, the first child found. */
template <class TValueType>
int TreeNode<TValueType>::ChildPosition( TValueType element ) const 
{
  for ( unsigned int i=0; i < m_Children.size(); i++ ) 
    {
    if ( m_Children[i]->Get() == element )
    return i;
    }
  return -1;
}

/** Add a child node */
template <class TValueType>
void TreeNode<TValueType>::AddChild( TreeNode<TValueType> *node ) 
{
  node->SetParent(this);
  m_Children.push_back(node);
}

/** Add a child at a specific position in the children list */
template <class TValueType>
void 
TreeNode<TValueType>
::AddChild( int number, TreeNode<TValueType> *node ) 
{  
  size_t size = m_Children.size();

  if ( number > size ) 
    {
    for ( size_t i=size; i <= number; i++ )
      {
      m_Children[i] = NULL;
      }
    m_Children[number] = node;
    return;
    }

  TreeNode<TValueType>* old = m_Children[number];
  if ( old != NULL )
    {
    delete old;
    }
  m_Children[number] = node;
}

/** Get the number of children given a name and a depth */
template <class TValueType>
unsigned int
TreeNode<TValueType>
::GetNumberOfChildren( unsigned int depth, char * name ) const
{
  typename ChildrenListType::const_iterator it = m_Children.begin();
  typename ChildrenListType::const_iterator itEnd = m_Children.end();

  unsigned int cnt = 0;
  while(it != itEnd)
    {
    if(name == NULL || strstr(typeid(**it).name(), name))
      {
      cnt++;
      }
    it++;
    }

  it = m_Children.begin();
  itEnd = m_Children.end();
  if( depth > 0 )
    {
    while(it != itEnd)
      {
      cnt += (*it)->GetNumberOfChildren( depth-1, name );
      it++;
      }
    }

  return cnt;
}

/** Get children given a name and a depth */
template <class TValueType>
typename TreeNode<TValueType>::ChildrenListType* 
TreeNode<TValueType>
::GetChildren( unsigned int depth, char * name) const
{
  ChildrenListType * children = new ChildrenListType;

  typename ChildrenListType::const_iterator childrenListIt = 
    m_Children.begin();
  typename ChildrenListType::const_iterator childrenListEnd = 
    m_Children.end();

  while( childrenListIt != childrenListEnd )
    {
    if( name == NULL || strstr(typeid(**childrenListIt).name(), name) )
      {
      children->push_back(*childrenListIt);
      }
    if( depth > 0 )
      {
      ChildrenListType * nextchildren = (**childrenListIt).GetChildren(depth-1,
                                                                       name);  
      // Add the child to the current list
      typename ChildrenListType::const_iterator nextIt = nextchildren->begin();
      while(nextIt != nextchildren->end())
        {
        children->push_back(*nextIt);
        nextIt++;
        }
      delete nextchildren;
      }
    childrenListIt++;
    }

  return children;
}


} // namespace itk

#endif
