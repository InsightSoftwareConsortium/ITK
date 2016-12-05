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
  typedef ChildTreeIterator                       Self;
  typedef TreeIteratorBase< TTreeType >           Superclass;
  typedef TTreeType                               TreeType;
  typedef typename TTreeType::ValueType           ValueType;
  typedef typename Superclass::TreeNodeType       TreeNodeType;
  typedef typename TreeNodeType::ChildIdentifier  ChildIdentifier;
  typedef typename Superclass::NodeType           NodeType;

  /** Constructor */
  ChildTreeIterator(TreeType *tree, const TreeNodeType *start = ITK_NULLPTR);

  /** Constructor */
  ChildTreeIterator(const TreeIteratorBase< TTreeType > & iterator);

  /** Get the type of the iterator */
  NodeType GetType() const;

  /** Go to a specific child node */
  virtual bool GoToChild(ChildIdentifier number = 0);

  /** Go to a parent node */
  virtual bool GoToParent();

  /** Clone function */
  TreeIteratorBase< TTreeType > * Clone();

  /** operator = */
  Self & operator=(Superclass & iterator)
  {
    if(this != &iterator)
      {
      Superclass::operator=(iterator);
      ChildTreeIterator< TTreeType > & it =
        static_cast< ChildTreeIterator< TTreeType > & >( iterator );
      m_ListPosition = it.m_ListPosition;
      m_ParentNode = it.m_ParentNode;
      }
    return *this;
  }

protected:

  /** Get the next value */
  const ValueType & Next();

  /** Return true if the next value exists */
  bool HasNext() const;

private:

  mutable ChildIdentifier  m_ListPosition;
  TreeNodeType *           m_ParentNode;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkChildTreeIterator.hxx"
#endif

#endif
