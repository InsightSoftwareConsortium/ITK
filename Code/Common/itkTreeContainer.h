/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTreeContainer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkTreeContainer_h
#define __itkTreeContainer_h

#include "itkMacro.h"
#include <itkTreeContainerBase.h>
#include <itkPreOrderTreeIterator.h>

namespace itk
{

/** \class TreeContainer
 *  \brief TreeContainer class
 * 
 * This class derives from the TreeContainerBase class.
 *
 * The class is templated over the type of the elements.
 *
 * Template parameters for class TreeContainer:
 *
 * - TValueType = Element type stored at each location in the Tree.
 *
 * \ingroup DataRepresentation 
 */
template <class TValueType>
class TreeContainer : public TreeContainerBase<TValueType>
{

public:

  /** Standard typedefs */
  typedef TreeContainerBase<TValueType> Superclass;
  typedef TreeContainer<TValueType> Self;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  typedef TValueType ValueType;
  typedef TreeNode<ValueType> TreeNodeType;

  /** Iterators typedef */ 
  typedef TreeIteratorBase<Self> IteratorType;
  typedef PreOrderTreeIterator<Self> PreOrderIteratorType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(TreeContainer, TreeContainerBase);
 
  /** Constructor */
  TreeContainer( int defaultChildrenCount );

  /** Constructor */
  TreeContainer( TreeContainer<TValueType>& tree );

  /** Set the root as an element */
  virtual bool SetRoot( TValueType element);

  /** The the root as an iterator position */
  bool SetRoot( IteratorType& pos );

  /** Set the root as a tree node */
  virtual bool SetRoot( TreeNode<TValueType>* node);

  /** Return true if the element is in the tree*/
  bool Contains( const TValueType element ) ;

  /** Return the number of elements in the tree */
  int Count() const;

  /** Return true if the element is a leaf */
  bool IsLeaf( const TValueType element );

  /** Return true if the element is a root */
  bool IsRoot( const TValueType element );

  /** Clear the tree */
  bool Clear();

  /** operator equal */
  bool operator==( TreeContainer<TValueType>& tree );

  /** Swap the iterators */
  bool Swap( IteratorType& v, IteratorType& w );

  /** Get the root */
  const TreeNodeType* GetRoot() const {return m_Root.GetPointer();}

  /** Add a child to a given parent using values*/
  bool Add(const TValueType child, const TValueType parent);

  /** Get node given a value */
  const TreeNodeType* GetNode(TValueType val) const;

protected:
  
  TreeContainer(); 
  virtual ~TreeContainer();

  typename TreeNodeType::Pointer    m_Root;
  int   m_DefaultChildrenCount;

  void PrintSelf(std::ostream& os, Indent indent) const;

};

} // namespace itk

// Define instantiation macro for this template.
#define ITK_TEMPLATE_TreeContainer(_, EXPORT, x, y) namespace itk { \
  _(1(class EXPORT TreeContainer< ITK_TEMPLATE_1 x >)) \
  namespace Templates { typedef TreeContainer< ITK_TEMPLATE_1 x > \
                                                  TreeContainer##y; } \
  }

#if ITK_TEMPLATE_EXPLICIT
# include "Templates/itkTreeContainer+-.h"
#endif

#if ITK_TEMPLATE_TXX
# include "itkTreeContainer.txx"
#endif

#endif
