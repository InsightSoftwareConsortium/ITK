/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRootTreeIterator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkRootTreeIterator_h
#define __itkRootTreeIterator_h

#include <itkTreeIteratorBase.h>

namespace itk
{
template< class TTreeType >
class RootTreeIterator:public TreeIteratorBase< TTreeType >
{
public:

  /** Typedefs */
  typedef TreeIteratorBase< TTreeType >     Superclass;
  typedef TTreeType                         TreeType;
  typedef typename TTreeType::ValueType     ValueType;
  typedef typename Superclass::TreeNodeType TreeNodeType;

  /** Constructor */
  RootTreeIterator(TreeType *tree, const TreeNodeType *start = NULL);

  /** Return the type of the iterator */
  int GetType() const;

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
template< class TTreeType >
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
template< class TTreeType >
int
RootTreeIterator< TTreeType >::GetType() const
{
  return TreeIteratorBase< TTreeType >::ROOT;
}

/** Return true if the next node exists */
template< class TTreeType >
bool
RootTreeIterator< TTreeType >::HasNext() const
{
  if ( const_cast< TreeNodeType * >( FindNextNode() ) != NULL )
    {
    return true;
    }
  return false;
}

/** Go to the next node */
template< class TTreeType >
const typename RootTreeIterator< TTreeType >::ValueType &
RootTreeIterator< TTreeType >::Next()
{
  this->m_Position = const_cast< TreeNodeType * >( FindNextNode() );
  return this->m_Position->Get();
}

/** Find the next node */
template< class TTreeType >
const typename RootTreeIterator< TTreeType >::TreeNodeType *
RootTreeIterator< TTreeType >::FindNextNode() const
{
  if ( this->m_Position == NULL )
    {
    return NULL;
    }
  if ( this->m_Position == this->m_Root )
    {
    return NULL;
    }
  return this->m_Position->GetParent();
}

/** Clone function */
template< class TTreeType >
TreeIteratorBase< TTreeType > *RootTreeIterator< TTreeType >::Clone()
{
  RootTreeIterator< TTreeType > *clone = new RootTreeIterator< TTreeType >(const_cast< TTreeType * >( this->m_Tree ),
                                                                           this->m_Position);
  *clone = *this;
  return clone;
}
} // end namespace itk

#endif
