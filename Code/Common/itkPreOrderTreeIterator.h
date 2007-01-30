/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPreOrderTreeIterator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkPreOrderTreeIterator_h
#define __itkPreOrderTreeIterator_h

#include <itkTreeIteratorBase.h>

namespace itk{

template <class TTreeType>
class PreOrderTreeIterator : public TreeIteratorBase<TTreeType> 
{
public:

  /** Typedefs */
  typedef typename TTreeType::ValueType  ValueType;
  typedef TreeIteratorBase<TTreeType> Superclass;
  typedef typename Superclass::TreeNodeType TreeNodeType;
 
  /** Constructor */
  PreOrderTreeIterator( const TTreeType* tree,const TreeNodeType* start = NULL );

  /** Get the type of the iterator */
  int GetType() const;
  /** Clone function */
  TreeIteratorBase<TTreeType>* Clone();

protected:
  /** Return the next node */
  const ValueType& Next();
  /** Return true if the next node exists */
  bool HasNext() const;
  
private:  

  /** Find the next node */
  const TreeNodeType* FindNextNode() const;

};


/** Constructor */
template <class TTreeType>
PreOrderTreeIterator<TTreeType>::PreOrderTreeIterator( const TTreeType* tree,const TreeNodeType* start )
  :TreeIteratorBase<TTreeType>(tree,start) 
{

}

/** Return the type of the iterator */
template <class TTreeType>
int 
PreOrderTreeIterator<TTreeType>::GetType() const 
{
  return TreeIteratorBase<TTreeType>::PREORDER; 
}

/** Return true if the next node exists */
template <class TTreeType>
bool 
PreOrderTreeIterator<TTreeType>::HasNext() const
{
  if ( const_cast<TreeNodeType* >(FindNextNode()) != NULL )
    {
    return true;
    }
  return false;
}

/** Return the next node */
template <class TTreeType>
const typename PreOrderTreeIterator<TTreeType>::ValueType&
PreOrderTreeIterator<TTreeType>::Next() 
{
  this->m_Position = const_cast<TreeNodeType* >(FindNextNode());
  return this->m_Position->Get();
}

/** Find the next node */
template <class TTreeType>
const typename PreOrderTreeIterator<TTreeType>::TreeNodeType* 
PreOrderTreeIterator<TTreeType>::FindNextNode() const
{ 
  if ( this->m_Position == NULL )
    {
    return NULL;
    }
  if ( this->m_Position->HasChildren() )
    {
    return dynamic_cast<const TreeNodeType*>(this->m_Position->GetChild(0));
    }

  if ( !this->m_Position->HasParent() )
    {
    return NULL;
    }

  TreeNodeType* child = this->m_Position;
  TreeNodeType* parent = dynamic_cast<TreeNodeType*>(this->m_Position->GetParent());

  int childPosition = parent->ChildPosition( child ); 
  int lastChildPosition = parent->CountChildren() - 1;

  while ( childPosition < lastChildPosition ) 
    {
    TreeNodeType* help = dynamic_cast<TreeNodeType*>(parent->GetChild( childPosition + 1 ));

    if ( help != NULL )
      {
      return help;
      }
    childPosition++;
    }
  
  while ( parent->HasParent() ) 
    {
    child = parent;
    parent = dynamic_cast<TreeNodeType*>(parent->GetParent());

    // Subtree
    if( parent->ChildPosition( this->m_Root ) >= 0 )
      {
      return NULL;
      }

    childPosition = parent->ChildPosition(child);
    lastChildPosition = parent->CountChildren() - 1;

    while ( childPosition < lastChildPosition ) 
      {
      TreeNodeType* help = dynamic_cast<TreeNodeType*>(parent->GetChild( childPosition + 1 ));

      if ( help != NULL )
        {
        return help;
        }
      }
    }
  return NULL;
}

/** Clone function */
template <class TTreeType>
TreeIteratorBase<TTreeType>* PreOrderTreeIterator<TTreeType>::Clone() 
{
  PreOrderTreeIterator<TTreeType>* clone = new PreOrderTreeIterator<TTreeType>(this-> m_Tree, this->m_Position );
  *clone = *this;
  return clone;
}

} // end namespace itk

#endif
