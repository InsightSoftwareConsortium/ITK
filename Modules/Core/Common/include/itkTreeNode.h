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
#ifndef itkTreeNode_h
#define itkTreeNode_h

#include <vector>
#include <algorithm>
#include <iostream>
#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkIntTypes.h"

namespace itk
{
/** \class TreeNode
 *  \brief Represents a node in a tree.
 *
 * This class derives from the Object class.
 *
 * The class is templated over the type of the elements.
 *
 * \tparam TValue = Element type stored in the node
 *
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */
template< typename TValue >
class ITK_TEMPLATE_EXPORT TreeNode:public Object
{
public:

  /** Standard typedefs */
  typedef Object                     Superclass;
  typedef TreeNode< TValue >         Self;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;
  typedef std::vector< Pointer >     ChildrenListType;
  typedef ::itk::OffsetValueType     ChildIdentifier;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(TreeNode, Object);

  /** Get the value of the node */
  const TValue & Get() const;

  /** Set the current value of the node */
  TValue Set(const TValue data);

  /** Get the child node */
  Self * GetChild(ChildIdentifier number) const;

  /** Get the parent node */
  Self * GetParent() const;

  /** Return true if the node has children */
  bool HasChildren() const;

  /** Return true if the node has a parent */
  bool HasParent() const;

  /** Set the parent of the node */
  void SetParent(Self *n);

  /** Return the number of children */
  ChildIdentifier CountChildren() const;

  /** Remove a node from the node */
  bool Remove(Self *n);

  /** Get the number of children given a name and depth */
  ChildIdentifier GetNumberOfChildren(unsigned int depth = 0, char *name = ITK_NULLPTR) const;

  /** Replace a given child by a new one */
  bool ReplaceChild(Self *oldChild, Self *newChild);

  /** Return the child position given a node */
  ChildIdentifier ChildPosition(const Self *node) const;

  /** Return the child position given a value */
  ChildIdentifier ChildPosition(TValue node) const;

  /** Add a child to the node */
  void AddChild(Self *node);

  /** Add a child to the node and specify the number in the children list */
  virtual void AddChild(ChildIdentifier number, Self *node);

  /** Get the children list */
#if !defined( ITK_WRAPPING_PARSER )
  virtual ChildrenListType * GetChildren(unsigned int depth = 0, char *name = ITK_NULLPTR) const;

#endif

  /** Get the internal list of children */
#if !defined( ITK_WRAPPING_PARSER )
  virtual ChildrenListType & GetChildrenList() { return m_Children; }
#endif

  /** Set the data of the node */
  //virtual void SetData(TValue data) {m_Data = data;}

protected:

  TreeNode();
  virtual ~TreeNode() ITK_OVERRIDE;
  TValue m_Data;

  Self *m_Parent;

  ChildrenListType m_Children;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(TreeNode);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTreeNode.hxx"
#endif

#endif
