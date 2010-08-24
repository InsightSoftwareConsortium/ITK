/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLevelOrderTreeIterator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLevelOrderTreeIterator_h
#define __itkLevelOrderTreeIterator_h

#include <queue>
#include <climits>
#include <itkTreeIteratorBase.h>

namespace itk
{
template< class TTreeType >
class LevelOrderTreeIterator:public TreeIteratorBase< TTreeType >
{
public:

  /** Typedefs */
  typedef LevelOrderTreeIterator            Self;
  typedef TreeIteratorBase< TTreeType >     Superclass;
  typedef TTreeType                         TreeType;
  typedef typename TTreeType::ValueType     ValueType;
  typedef typename Superclass::TreeNodeType TreeNodeType;

  /** Constructors */
  LevelOrderTreeIterator(TreeType *tree, int endLevel = INT_MAX, const TreeNodeType *start = NULL);
  LevelOrderTreeIterator(TreeType *tree, int startLevel, int endLevel, const TreeNodeType *start = NULL);

  virtual ~LevelOrderTreeIterator() {}

  /** Get the type of the iterator */
  int GetType() const;

  /** Get the start level */
  int GetStartLevel() const;

  /** Get the end level */
  int GetEndLevel() const;

  /** Get the current level */
  int GetLevel() const;

  /** Clone function */
  TreeIteratorBase< TTreeType > * Clone();

  /** operator = */
  const Self & operator=(const Self & iterator)
  {
    this->Superclass::operator=(iterator);
    m_StartLevel = iterator.m_StartLevel;
    m_EndLevel = iterator.m_EndLevel;
    m_Queue = iterator.m_Queue;
    return *this;
  }

protected:

  /** Return the next node */
  const ValueType & Next();

  /** Return true if the next node exists */
  bool HasNext() const;

private:

  const TreeNodeType * FindNextNode() const;

  const TreeNodeType * FindNextNodeHelp() const;

  int GetLevel(const TreeNodeType *node) const;

  int                                        m_StartLevel;
  int                                        m_EndLevel;
  mutable std::queue< const TreeNodeType * > m_Queue;
};

/** Constructor with end level specification */
template< class TTreeType >
LevelOrderTreeIterator< TTreeType >
::LevelOrderTreeIterator(TTreeType *tree, int endLevel, const TreeNodeType *start):
  TreeIteratorBase< TTreeType >(tree, start)
{
  m_StartLevel =  -1;
  m_EndLevel = endLevel;
  if ( start != NULL )
    {
    m_Queue.push(start);
    this->m_Position = const_cast< TreeNodeType * >( start );
    }
  else
    {
    if ( tree->GetRoot() )
      {
      m_Queue.push( dynamic_cast< const TreeNodeType * >( tree->GetRoot() ) );
      this->m_Position = const_cast< TreeNodeType * >( dynamic_cast< const TreeNodeType * >( tree->GetRoot() ) );
      }
    }
  this->m_Begin = this->m_Position;
}

/** Constructor with end level specification */
template< class TTreeType >
LevelOrderTreeIterator< TTreeType >
::LevelOrderTreeIterator(TTreeType *tree, int startLevel, int endLevel, const TreeNodeType *start):
  TreeIteratorBase< TTreeType >(tree, start)
{
  m_StartLevel = startLevel;
  m_EndLevel = endLevel;
  if ( start != NULL )
    {
    m_Queue.push(start);
    this->m_Position = const_cast< TreeNodeType * >( start );
    }
  else
    {
    if ( tree->GetRoot() )
      {
      m_Queue.push( dynamic_cast< const TreeNodeType * >( tree->GetRoot() ) );
      this->m_Position = const_cast< TreeNodeType * >( dynamic_cast< const TreeNodeType * >( tree->GetRoot() ) );
      }
    }
  this->m_Begin = this->m_Position;
}

/** Return the type of iterator */
template< class TTreeType >
int
LevelOrderTreeIterator< TTreeType >::GetType() const
{
  return TreeIteratorBase< TTreeType >::LEVELORDER;
}

/** Return true if the next value exists */
template< class TTreeType >
bool
LevelOrderTreeIterator< TTreeType >::HasNext() const
{
  if ( const_cast< TreeNodeType * >( FindNextNode() ) )
    {
    return true;
    }
  return false;
}

/** Return the next node */
template< class TTreeType >
const typename LevelOrderTreeIterator< TTreeType >::ValueType &
LevelOrderTreeIterator< TTreeType >::Next()
{
  this->m_Position = const_cast< TreeNodeType * >( FindNextNode() );
  return this->m_Position->Get();
}

/** Get the start Level */
template< class TTreeType >
int LevelOrderTreeIterator< TTreeType >::GetStartLevel() const
{
  return m_StartLevel;
}

/** Get the end level */
template< class TTreeType >
int
LevelOrderTreeIterator< TTreeType >::GetEndLevel() const
{
  return m_EndLevel;
}

/** Find the next available node */
template< class TTreeType >
const typename LevelOrderTreeIterator< TTreeType >::TreeNodeType *
LevelOrderTreeIterator< TTreeType >::FindNextNode() const
{
  int                 level;
  const TreeNodeType *node;

  do
    {
    node = FindNextNodeHelp();
    if ( node == NULL )
      {
      return NULL;
      }
    level = GetLevel(node);
    if ( level > m_EndLevel )
      {
      return NULL;
      }
    }
  while ( level < m_StartLevel );

  return node;
}

/** Return the current level */
template< class TTreeType >
int
LevelOrderTreeIterator< TTreeType >::GetLevel() const
{
  if ( this->m_Position == NULL )
    {
    return -1;
    }

  int           level = 0;
  TreeNodeType *node = this->m_Position;
  while ( node->HasParent() && node != this->m_Root )
    {
    node = dynamic_cast< TreeNodeType * >( node->GetParent() );
    level++;
    }
  return level;
}

/** Return the level given a node */
template< class TTreeType >
int
LevelOrderTreeIterator< TTreeType >::GetLevel(const TreeNodeType *node) const
{
  if ( node == NULL )
    {
    return -1;
    }
  int level = 0;

  while ( node->HasParent() && node != this->m_Root )
    {
    node = dynamic_cast< const TreeNodeType * >( node->GetParent() );
    level++;
    }
  return level;
}

/** Helper function to find the next node */
template< class TTreeType >
const typename LevelOrderTreeIterator< TTreeType >::TreeNodeType *
LevelOrderTreeIterator< TTreeType >::FindNextNodeHelp() const
{
  if ( m_Queue.empty() )
    {
    return NULL;
    }

  const TreeNodeType *currentNode = m_Queue.front();
  m_Queue.pop();

  if ( currentNode == NULL )
    {
    return NULL;
    }

  int size = currentNode->CountChildren();

  for ( int i = 0; i < size; i++ )
    {
    TreeNodeType *child = dynamic_cast< TreeNodeType * >( currentNode->GetChild(i) );
    if ( child != NULL )
      {
      m_Queue.push(child);
      }
    }

  // If the current node is the root we try again
  if ( currentNode == this->m_Root )
    {
    currentNode = const_cast< TreeNodeType * >( FindNextNodeHelp() );
    }
  return currentNode;
}

/** Clone function */
template< class TTreeType >
TreeIteratorBase< TTreeType > *LevelOrderTreeIterator< TTreeType >::Clone()
{
  LevelOrderTreeIterator< TTreeType > *clone =
    new LevelOrderTreeIterator< TTreeType >(const_cast< TTreeType * >( this->m_Tree ), m_StartLevel, m_EndLevel);
  *clone = *this;
  return clone;
}
} // end namespace itk

#endif
