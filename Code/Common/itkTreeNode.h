/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTreeNode.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkTreeNode_h
#define __itkTreeNode_h

#include <vector>
#include <algorithm>
#include <iostream>
#include <itkObject.h>

namespace itk
{
/** \class TreeNode
 *  \brief TreeNode class
 * 
 * This class derives from the Object class.
 *
 * The class is templated over the type of the elements.
 *
 * Template parameters for class TreeNode:
 *
 * - TValueType = Element type stored in the node
 *
 * \ingroup DataRepresentation 
 */
template <class TValueType>
class TreeNode : public Object
{

public:

  /** Standard typedefs */
  typedef Object                    Superclass;
  typedef TreeNode<TValueType>      Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  typedef std::vector<Pointer>      ChildrenListType;

  /** Method for creation through the object factory. */
  itkNewMacro( Self );
 
  /** Run-time type information (and related methods). */ 
  itkTypeMacro( TreeNode, Object );

  /** Get the value of the node */
  const TValueType& Get() const;

  /** Set the current value of the node */
  TValueType Set(const TValueType data);

  /** Get the child node */
  TreeNode<TValueType>* GetChild( int number ) const;

  /** Get the parent node */
  TreeNode<TValueType>* GetParent( ) const;

  /** Return true if the node has children */
  bool HasChildren( ) const;

  /** Return true if the node has a parent */
  bool HasParent( ) const;

  /** Set the parent of the node */
  void SetParent( TreeNode<TValueType>* n );

  /** Return the number of children */
  int CountChildren( ) const;

  /** Remove a node from the node */
  bool Remove( TreeNode<TValueType> *n );

  /** Get the number of children given a name and depth */
  unsigned int GetNumberOfChildren(unsigned int depth=0, char * name=NULL ) const;

  /** Replace a given child by a new one */
  bool ReplaceChild( TreeNode<TValueType> *oldChild, TreeNode<TValueType> *newChild );

  /** Return the child position given a node */
  int ChildPosition( const TreeNode<TValueType> *node ) const;
  /** Return the child position given a value */
  int ChildPosition( TValueType node ) const;

  /** Add a child to the node */
  void AddChild( TreeNode<TValueType> *node );

  /** Add a child to the node and specify the number in the children list */
  virtual void AddChild( int number, TreeNode<TValueType> *node );

  /** Get the children list */
#if !defined(CABLE_CONFIGURATION)
  virtual ChildrenListType* GetChildren( unsigned int depth=0, char * name=NULL) const;
#endif

  /** Get the internal list of children */
#if !defined(CABLE_CONFIGURATION)
  virtual ChildrenListType& GetChildrenList() {return m_Children;}
#endif

  /** Set the data of the node */
  //virtual void SetData(TValueType data) {m_Data = data;}

protected:

  TreeNode();
  virtual ~TreeNode();
  TValueType m_Data;
  Self* m_Parent;
  ChildrenListType m_Children;
private:
  TreeNode(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTreeNode.txx"
#endif

#endif
