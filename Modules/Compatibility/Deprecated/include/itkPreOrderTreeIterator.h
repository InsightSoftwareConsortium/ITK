/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkPreOrderTreeIterator_h
#define itkPreOrderTreeIterator_h

#include "itkTreeIteratorBase.h"

namespace itk
{
// Forward reference because of circular dependencies
template <typename TTreeType>
class ITK_TEMPLATE_EXPORT LeafTreeIterator;

template <typename TTreeType>
class PreOrderTreeIterator : public TreeIteratorBase<TTreeType>
{
public:
  /** Typedefs */
  using ValueType = typename TTreeType::ValueType;
  using Superclass = TreeIteratorBase<TTreeType>;
  using TreeNodeType = typename Superclass::TreeNodeType;
  using NodeType = typename Superclass::NodeType;

  /** Constructor */
  PreOrderTreeIterator(const TTreeType * tree, const TreeNodeType * start = nullptr);

  /** Get the type of the iterator */
  NodeType
  GetType() const override;

  /** Clone function */
  TreeIteratorBase<TTreeType> *
  Clone() override;

protected:
  /** Return the next node */
  const ValueType &
  Next() override;

  /** Return true if the next node exists */
  bool
  HasNext() const override;

private:
  /** Find the next node */
  const TreeNodeType *
  FindNextNode() const;

  /** LeafTreeIterator uses PreOrderTreeIterator in its implementation, but it
   * needs to adjust its root.  A friend designation is added to correct
   * behavior and retain backwards compatible behavior. */
  friend class LeafTreeIterator<TTreeType>;
};

/** Constructor */
template <typename TTreeType>
PreOrderTreeIterator<TTreeType>::PreOrderTreeIterator(const TTreeType * tree, const TreeNodeType * start)
  : TreeIteratorBase<TTreeType>(tree, start)
{}

/** Return the type of the iterator */
template <typename TTreeType>
typename PreOrderTreeIterator<TTreeType>::NodeType
PreOrderTreeIterator<TTreeType>::GetType() const
{
  return TreeIteratorBaseEnums::TreeIteratorBaseNode::PREORDER;
}

/** Return true if the next node exists */
template <typename TTreeType>
bool
PreOrderTreeIterator<TTreeType>::HasNext() const
{
  if (const_cast<TreeNodeType *>(FindNextNode()) != nullptr)
  {
    return true;
  }
  return false;
}

/** Return the next node */
template <typename TTreeType>
const typename PreOrderTreeIterator<TTreeType>::ValueType &
PreOrderTreeIterator<TTreeType>::Next()
{
  this->m_Position = const_cast<TreeNodeType *>(FindNextNode());
  if (this->m_Position == nullptr)
  {
    return this->m_Root->Get(); // value irrelevant, but we have to return something
  }
  return this->m_Position->Get();
}

/** Find the next node */
template <typename TTreeType>
const typename PreOrderTreeIterator<TTreeType>::TreeNodeType *
PreOrderTreeIterator<TTreeType>::FindNextNode() const
{
  if (this->m_Position == nullptr)
  {
    return nullptr;
  }
  if (this->m_Position->HasChildren())
  {
    return dynamic_cast<const TreeNodeType *>(this->m_Position->GetChild(0));
  }

  if (!this->m_Position->HasParent())
  {
    return nullptr;
  }

  TreeNodeType * child = this->m_Position;
  TreeNodeType * parent = this->m_Position;

  while (parent->HasParent())
  {
    child = parent;
    parent = dynamic_cast<TreeNodeType *>(parent->GetParent());

    // Subtree
    if (parent->ChildPosition(this->m_Root) >= 0)
    {
      return nullptr;
    }

    int childPosition = parent->ChildPosition(child);
    int lastChildPosition = parent->CountChildren() - 1;

    while (childPosition < lastChildPosition)
    {
      auto * help = dynamic_cast<TreeNodeType *>(parent->GetChild(childPosition + 1));

      if (help != nullptr)
      {
        return help;
      }
      childPosition++;
    }
  }
  return nullptr;
}

/** Clone function */
template <typename TTreeType>
TreeIteratorBase<TTreeType> *
PreOrderTreeIterator<TTreeType>::Clone()
{
  auto * clone = new PreOrderTreeIterator<TTreeType>(this->m_Tree, this->m_Position);
  *clone = *this;
  return clone;
}
} // end namespace itk

#endif
