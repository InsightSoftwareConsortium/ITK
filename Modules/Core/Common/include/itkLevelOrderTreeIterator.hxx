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
#ifndef itkLevelOrderTreeIterator_hxx
#define itkLevelOrderTreeIterator_hxx

#include "itkLevelOrderTreeIterator.h"

namespace itk
{
/** Constructor with end level specification */
template< typename TTreeType >
LevelOrderTreeIterator< TTreeType >
::LevelOrderTreeIterator(TTreeType *tree, int endLevel, const TreeNodeType *start):
  TreeIteratorBase< TTreeType >(tree, start)
{
  m_StartLevel =  -1;
  m_EndLevel = endLevel;
  if ( start != ITK_NULLPTR )
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
template< typename TTreeType >
LevelOrderTreeIterator< TTreeType >
::LevelOrderTreeIterator(TTreeType *tree, int startLevel, int endLevel, const TreeNodeType *start):
  TreeIteratorBase< TTreeType >(tree, start)
{
  m_StartLevel = startLevel;
  m_EndLevel = endLevel;
  if ( start != ITK_NULLPTR )
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
template< typename TTreeType >
typename LevelOrderTreeIterator< TTreeType >::NodeType
LevelOrderTreeIterator< TTreeType >::GetType() const
{
  return TreeIteratorBase< TTreeType >::LEVELORDER;
}

/** Return true if the next value exists */
template< typename TTreeType >
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
template< typename TTreeType >
const typename LevelOrderTreeIterator< TTreeType >::ValueType &
LevelOrderTreeIterator< TTreeType >::Next()
{
  this->m_Position = const_cast< TreeNodeType * >( FindNextNode() );
  return this->m_Position->Get();
}

/** Get the start Level */
template< typename TTreeType >
int LevelOrderTreeIterator< TTreeType >::GetStartLevel() const
{
  return m_StartLevel;
}

/** Get the end level */
template< typename TTreeType >
int
LevelOrderTreeIterator< TTreeType >::GetEndLevel() const
{
  return m_EndLevel;
}

/** Find the next available node */
template< typename TTreeType >
const typename LevelOrderTreeIterator< TTreeType >::TreeNodeType *
LevelOrderTreeIterator< TTreeType >::FindNextNode() const
{
  int                 level;
  const TreeNodeType *node;

  do
    {
    node = FindNextNodeHelp();
    if ( node == ITK_NULLPTR )
      {
      return ITK_NULLPTR;
      }
    level = GetLevel(node);
    if ( level > m_EndLevel )
      {
      return ITK_NULLPTR;
      }
    }
  while ( level < m_StartLevel );

  return node;
}

/** Return the current level */
template< typename TTreeType >
int
LevelOrderTreeIterator< TTreeType >::GetLevel() const
{
  if ( this->m_Position == ITK_NULLPTR )
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
template< typename TTreeType >
int
LevelOrderTreeIterator< TTreeType >::GetLevel(const TreeNodeType *node) const
{
  if ( node == ITK_NULLPTR )
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
template< typename TTreeType >
const typename LevelOrderTreeIterator< TTreeType >::TreeNodeType *
LevelOrderTreeIterator< TTreeType >::FindNextNodeHelp() const
{
  if ( m_Queue.empty() )
    {
    return ITK_NULLPTR;
    }

  const TreeNodeType *currentNode = m_Queue.front();
  m_Queue.pop();

  if ( currentNode == ITK_NULLPTR )
    {
    return ITK_NULLPTR;
    }

  int size = currentNode->CountChildren();

  for ( int i = 0; i < size; i++ )
    {
    TreeNodeType *child = dynamic_cast< TreeNodeType * >( currentNode->GetChild(i) );
    if ( child != ITK_NULLPTR )
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
template< typename TTreeType >
TreeIteratorBase< TTreeType > *LevelOrderTreeIterator< TTreeType >::Clone()
{
  LevelOrderTreeIterator< TTreeType > *clone =
    new LevelOrderTreeIterator< TTreeType >(const_cast< TTreeType * >( this->m_Tree ), m_StartLevel, m_EndLevel);
  *clone = *this;
  return clone;
}
} // end namespace itk

#endif
