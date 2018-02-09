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
#ifndef itkChildTreeIterator_h
#define itkChildTreeIterator_h

#include "itkTreeIteratorBase.h"

namespace itk
{
template< typename TTreeType >
class ITK_TEMPLATE_EXPORT ChildTreeIterator:public TreeIteratorBase< TTreeType >
{
public:

  /** Typedefs */
  using Self = ChildTreeIterator;
  using Superclass = TreeIteratorBase< TTreeType >;
  using TreeType = TTreeType;
  using ValueType = typename TTreeType::ValueType;
  using TreeNodeType = typename Superclass::TreeNodeType;
  using ChildIdentifier = typename TreeNodeType::ChildIdentifier;
  using NodeType = typename Superclass::NodeType;

  /** Constructor */
  ChildTreeIterator(TreeType *tree, const TreeNodeType *start = nullptr);

  /** Constructor */
  ChildTreeIterator(const TreeIteratorBase< TTreeType > & iterator);

  /** Get the type of the iterator */
  NodeType GetType() const override;

  /** Go to a specific child node */
  bool GoToChild(ChildIdentifier number = 0) override;

  /** Go to a parent node */
  bool GoToParent() override;

  /** Clone function */
  TreeIteratorBase< TTreeType > * Clone() override;

  /** operator = */
  Self & operator=(Superclass & iterator)
  {
    if(this != &iterator)
      {
      Superclass::operator=(iterator);
      auto & it = static_cast< ChildTreeIterator< TTreeType > & >( iterator );
      m_ListPosition = it.m_ListPosition;
      m_ParentNode = it.m_ParentNode;
      }
    return *this;
  }

protected:

  /** Get the next value */
  const ValueType & Next() override;

  /** Return true if the next value exists */
  bool HasNext() const override;

private:

  mutable ChildIdentifier  m_ListPosition;
  TreeNodeType *           m_ParentNode;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkChildTreeIterator.hxx"
#endif

#endif
