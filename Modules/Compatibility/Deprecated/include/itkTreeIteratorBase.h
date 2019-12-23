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
#ifndef itkTreeIteratorBase_h
#define itkTreeIteratorBase_h

#include "itkTreeNode.h"

namespace itk
{
/** \class TreeIteratorBaseNodeEnum
 * \ingroup ITKDeprecated
 * Enumerations for node type
 */
enum class TreeIteratorBaseNodeEnum : uint8_t
{
  UNDEFIND = 0,
  PREORDER = 1,
  INORDER = 2,
  POSTORDER = 3,
  LEVELORDER = 4,
  CHILD = 5,
  ROOT = 6,
  LEAF = 7
};

/** \class TreeIteratorBase
 *  \brief This class provides the base implementation for tree iterators.
 *
 * Events will notify interested observers about tree changes. These events all derive from TreeChangeEvent. They are:
 *
 *  - TreeNodeChangeEvent: invoked when Set() is called, i.e. exactly one node changes
 *  - TreeAddEvent: invoked when Add() is called.
 *  - TreeRemoveEvent: when a single node has been removed, i.e. Disconnect() has been called.
 *  - TreePruneEvent: when a node and all its children were removed, i.e. Remove() has been called.
 *
 *  All those events have a member GetChangePosition(), which returns an iterator to the position that has changd.
 * Please note that this iterator may not be fully functional, but you should always be able to use its Get() method to
 * retrieve the thing it points to.
 *
 * \ingroup ITKDeprecated
 */
template <typename TTreeType>
class ITK_TEMPLATE_EXPORT TreeIteratorBase
{
public:
  /** Typedefs */
  using Self = TreeIteratorBase;
  using ValueType = typename TTreeType::ValueType;
  using TreeNodeType = typename TTreeType::TreeNodeType;
  using ChildIdentifier = typename TreeNodeType::ChildIdentifier;

  /** Backwards compatibility for enum values */
  using NodeType = TreeIteratorBaseNodeEnum;
#if !defined(ITK_LEGACY_REMOVE)
  // We need to expose the enum values at the class level
  // for backwards compatibility
  static constexpr NodeType UNDEFIND = NodeType::UNDEFIND;
  static constexpr NodeType PREORDER = NodeType::PREORDER;
  static constexpr NodeType INORDER = NodeType::INORDER;
  static constexpr NodeType POSTORDER = NodeType::POSTORDER;
  static constexpr NodeType LEVELORDER = NodeType::LEVELORDER;
  static constexpr NodeType CHILD = NodeType::CHILD;
  static constexpr NodeType ROOT = NodeType::ROOT;
  static constexpr NodeType LEAF = NodeType::LEAF;
#endif

  /** Add an element to the tree */
  virtual bool
  Add(ValueType element);

  /** Add an element at a given position */
  virtual bool
  Add(int position, ValueType element);

  /** Add a subtree */
  virtual bool
  Add(TTreeType & subTree);

  /** Get a value */
  virtual const ValueType &
  Get() const;

  /** Get the subtree */
  virtual TTreeType *
  GetSubTree() const;

  /** Return true if the current node is a leaf */
  virtual bool
  IsLeaf() const;

  /** Return true if the current node is a root */
  virtual bool
  IsRoot() const;

  /** Get the type of iterator */
  virtual NodeType
  GetType() const = 0;

  /** Go to the specified child */
  virtual bool
  GoToChild(ChildIdentifier number = 0);

  /** Go to the parent */
  virtual bool
  GoToParent();

  /** Set the current value of the node */
  void
  Set(ValueType element);

  /** Return true if the current node has a child */
  virtual bool
  HasChild(int number = 0) const;

  /** Return the current ChildPosition of an element */
  virtual int
  ChildPosition(ValueType element) const;

  /** Remove a child */
  virtual bool
  RemoveChild(int number);

  /** Count the number of children */
  virtual int
  CountChildren() const;

  /** Return true if the current node has a parent */
  virtual bool
  HasParent() const;

  /** Disconnect the tree */
  virtual bool
  Disconnect();

  /** Return a list of children */
  virtual TreeIteratorBase<TTreeType> *
  Children();

  /** Return a list of parents */
  virtual TreeIteratorBase<TTreeType> *
  Parents();

  /** Return a list of child */
  virtual TreeIteratorBase<TTreeType> *
  GetChild(int number) const;

  /** Count the number of nodes */
  virtual int
  Count();

  /** Remove the current node from the tree */
  bool
  Remove();

  /** Get the current node */
  virtual TreeNodeType *
  GetNode();

  virtual const TreeNodeType *
  GetNode() const;

  /** Get the root */
  TreeNodeType *
  GetRoot();

  const TreeNodeType *
  GetRoot() const;

  /** Get the tree */
  TTreeType *
  GetTree() const;

  /** Return the first parent found */
  const TreeNodeType *
  GetParent() const;

  /** Move an iterator to the beginning of the tree */
  void
  GoToBegin()
  {
    m_Position = m_Begin;
  }

  /** Move an iterator to the end of the tree. */
  void
  GoToEnd()
  {
    m_Position = nullptr;
  }

  /** Is the iterator at the beginning of the tree? */
  bool
  IsAtBegin() const
  {
    return (m_Position == m_Begin);
  }

  /** Is the iterator at the end of the tree? */
  bool
  IsAtEnd() const
  {
    return (m_Position == nullptr);
  }

  /** Clone the iterator */
  virtual TreeIteratorBase<TTreeType> *
  Clone() = 0;

  /** operator++ */
  Self &
  operator++()
  {
    this->Next();
    return *this;
  }

  /** operator++ */
  void
  operator++(int)
  {
    this->Next();
  }

  /** operator = */
  Self &
  operator=(const Self & iterator)
  {
    if (this != &iterator)
    {
      m_Position = iterator.m_Position;
      m_Begin = iterator.m_Begin;
      m_Root = iterator.m_Root;
      m_Tree = iterator.m_Tree;
    }
    return *this;
  }

  virtual ~TreeIteratorBase() = default;

protected:
  /** Constructors */
  TreeIteratorBase(TTreeType * tree, const TreeNodeType * start);
  TreeIteratorBase(const TTreeType * tree, const TreeNodeType * start);

  mutable TreeNodeType * m_Position; // Current position of the iterator
  mutable TreeNodeType * m_Begin;
  const TreeNodeType *   m_Root;
  TTreeType *            m_Tree;

  virtual bool
  HasNext() const = 0;

  virtual const ValueType &
  Next() = 0;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkTreeIteratorBase.hxx"
#endif

#endif
