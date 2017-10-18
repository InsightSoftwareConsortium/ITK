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
#ifndef itkTreeContainer_h
#define itkTreeContainer_h

#include "itkTreeContainerBase.h"
#include "itkPreOrderTreeIterator.h"

namespace itk
{
/** \class TreeContainer
 *  \brief A tree container.
 *
 * This class derives from the TreeContainerBase class.
 * The class is templated over the type of the elements.
 *
 * \tparam TValue Element type stored at each location in the Tree.
 *
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */
template< typename TValue >
class ITK_TEMPLATE_EXPORT TreeContainer:public TreeContainerBase< TValue >
{
public:

  /** Standard typedefs */
  typedef TreeContainerBase< TValue > Superclass;
  typedef TreeContainer< TValue >     Self;
  typedef SmartPointer< Self >        Pointer;
  typedef SmartPointer< const Self >  ConstPointer;
  typedef TValue                      ValueType;
  typedef TreeNode< ValueType >       TreeNodeType;

  /** Iterators typedef */
  typedef TreeIteratorBase< Self >     IteratorType;
  typedef PreOrderTreeIterator< Self > PreOrderIteratorType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(TreeContainer, TreeContainerBase);

  /** Constructor */
  TreeContainer(int defaultChildrenCount);

  /** Constructor */
  TreeContainer(TreeContainer< TValue > & tree);

  /** Set the root as an element */
  virtual bool SetRoot(const TValue element) ITK_OVERRIDE;

  /** The the root as an iterator position */
  bool SetRoot(IteratorType & pos);

  /** Set the root as a tree node */
  virtual bool SetRoot(TreeNode< TValue > *node) ITK_OVERRIDE;

  /** Return true if the element is in the tree */
  bool Contains(const TValue element) ITK_OVERRIDE;

  /** Return the number of elements in the tree */
  int Count() const ITK_OVERRIDE;

  /** Return true if the element is a leaf */
  bool IsLeaf(const TValue element) ITK_OVERRIDE;

  /** Return true if the element is a root */
  bool IsRoot(const TValue element) ITK_OVERRIDE;

  /** Clear the tree */
  bool Clear() ITK_OVERRIDE;

  /** operator equal */
  bool operator==(TreeContainer< TValue > & tree);

  /** Swap the iterators */
  bool Swap(IteratorType & v, IteratorType & w);

  /** Get the root */
  const TreeNodeType * GetRoot() const ITK_OVERRIDE { return m_Root.GetPointer(); }

  /** Add a child to a given parent using values */
  bool Add(const TValue child, const TValue parent);

  /** Get node given a value */
  const TreeNodeType * GetNode(TValue val) const;

protected:

  TreeContainer();
  virtual ~TreeContainer() ITK_OVERRIDE;

  typename TreeNodeType::Pointer m_Root;

  int m_DefaultChildrenCount;

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTreeContainer.hxx"
#endif

#endif
