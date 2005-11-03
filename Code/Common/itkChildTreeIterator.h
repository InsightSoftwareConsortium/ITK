/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkChildTreeIterator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkChildTreeIterator_h
#define __itkChildTreeIterator_h

#include <itkTreeIteratorBase.h>


namespace itk{

template <class TTreeType>
class ChildTreeIterator : public TreeIteratorBase<TTreeType> 
{
public:
  
  /** Typedefs */
  typedef TreeIteratorBase<TTreeType>  Superclass;
  typedef TTreeType TreeType;
  typedef typename Superclass::Self Self;
  typedef typename TTreeType::ValueType ValueType;
  typedef typename Superclass::TreeNodeType TreeNodeType;

  /** Constructor */
  ChildTreeIterator( TreeType* tree,const  TreeNodeType* start=NULL );

  /** Constructor */
  ChildTreeIterator( const TreeIteratorBase<TTreeType>& iterator );

  /** Get the type of the iterator */
  int GetType( ) const;

  /** Go to a specific child node */
  virtual bool GoToChild( int number = 0 );

  /** Go to a parent node */
  virtual bool GoToParent();

  /** Clone function */
  TreeIteratorBase<TTreeType>* Clone();

  /** operator = */
  Self& operator=(Superclass& iterator) 
    {
    Superclass::operator=(iterator);
    ChildTreeIterator<TTreeType>& it = static_cast<ChildTreeIterator<TTreeType>&>(iterator);
    m_ListPosition = it.m_ListPosition;
    m_ParentNode = it.m_ParentNode;
    return *this;
    }

protected:

  /** Get the next value */
  const ValueType& Next();

  /** Return true if the next value exists */
  bool HasNext() const;

private:

  mutable int m_ListPosition;
  TreeNode<ValueType>* m_ParentNode;
};

/** Constructor */
template <class TTreeType>
ChildTreeIterator<TTreeType>::ChildTreeIterator(TTreeType* tree, const TreeNodeType* start)  
  :TreeIteratorBase<TTreeType>(tree, start)
{
  m_ListPosition = 0;
  m_ParentNode = this->m_Position;
  this->m_Position = m_ParentNode->GetChild( m_ListPosition );
  this->m_Begin = this->m_Position;
}

template <class TTreeType>
ChildTreeIterator<TTreeType>::ChildTreeIterator(const TreeIteratorBase<TTreeType>& iterator)
  :TreeIteratorBase<TTreeType>(iterator.GetTree(), iterator.GetNode())
{
  m_ListPosition = 0;
  m_ParentNode = this->m_Position;
  this->m_Position = m_ParentNode->GetChild( m_ListPosition );
}

/** Go to a specific child */
template <class TTreeType>
bool 
ChildTreeIterator<TTreeType>::GoToChild(int number)
{
  if ( m_ParentNode->GetChild( number ) == NULL )
    {
    return false;
    }

  m_ListPosition = 0;
  m_ParentNode = m_ParentNode->GetChild( number );
  this->m_Position = m_ParentNode->GetChild( m_ListPosition );
  this->m_Begin = this->m_Position;
  return true;
}

/** Go to the parent node */
template <class TTreeType>
bool 
ChildTreeIterator<TTreeType>::GoToParent()
{
  TreeNode<ValueType>* parent =  m_ParentNode->GetParent();
    
  if ( parent == NULL )
    {
    return false;
    }

  m_ListPosition = 0;
  m_ParentNode = parent;
  this->m_Position = m_ParentNode->GetChild( m_ListPosition );
  this->m_Begin = this->m_Position;
  return true;
}

/** Return the type of the iterator */
template <class TTreeType>
int 
ChildTreeIterator<TTreeType>::GetType() const
{
  return TreeIteratorBase<TTreeType>::CHILD;
}

/** Return true if the next node exists */
template <class TTreeType>
bool 
ChildTreeIterator<TTreeType>::HasNext() const
{
  if( m_ListPosition < m_ParentNode->CountChildren() - 1 )
    {
    return true;
    }
  else
    {
    return false;
    }
}

/** Return the next node */
template <class TTreeType>
const typename ChildTreeIterator<TTreeType>::ValueType&
ChildTreeIterator<TTreeType>::Next() 
{
  m_ListPosition++;
  this->m_Position = m_ParentNode->GetChild( m_ListPosition );
  return this->m_Position->Get();
}

/** Clone function */
template <class TTreeType>
TreeIteratorBase<TTreeType>* ChildTreeIterator<TTreeType>::Clone() 
{
  ChildTreeIterator<TTreeType>* clone = new ChildTreeIterator<TTreeType>( const_cast<TTreeType*>(this->m_Tree), this->m_Position );   
  *clone = *this;
  return clone;
}

} // end namespace itk

#endif
