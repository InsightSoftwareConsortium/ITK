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
#ifndef itkLevelOrderTreeIterator_h
#define itkLevelOrderTreeIterator_h

#include <queue>
#include <climits>
#include "itkTreeIteratorBase.h"

namespace itk
{
/**
 * \class LevelOrderTreeIterator
 * \brief Iterate over a tree in level order.
 *
 * \ingroup ITKCommon
 */
template< typename TTreeType >
class ITK_TEMPLATE_EXPORT LevelOrderTreeIterator:public TreeIteratorBase< TTreeType >
{
public:

  /** Typedefs */
  typedef LevelOrderTreeIterator            Self;
  typedef TreeIteratorBase< TTreeType >     Superclass;
  typedef TTreeType                         TreeType;
  typedef typename TTreeType::ValueType     ValueType;
  typedef typename Superclass::TreeNodeType TreeNodeType;
  typedef typename Superclass::NodeType     NodeType;

  /** Constructor with end level specification */
  LevelOrderTreeIterator(TreeType *tree, int endLevel = INT_MAX, const TreeNodeType *start = ITK_NULLPTR);

  /** Constructor with end level specification */
  LevelOrderTreeIterator(TreeType *tree, int startLevel, int endLevel, const TreeNodeType *start = ITK_NULLPTR);

  virtual ~LevelOrderTreeIterator() {}

  /** Get the type of the iterator */
  NodeType GetType() const;

  /** Get the start level */
  int GetStartLevel() const;

  /** Get the end level */
  int GetEndLevel() const;

  /** Get the current level */
  int GetLevel() const;

  /** Clone function */
  TreeIteratorBase< TTreeType > * Clone();

  /** operator = */
  const Self & operator=(const Self & iterator)
  {
    if(this != &iterator)
      {
      this->Superclass::operator=(iterator);
      m_StartLevel = iterator.m_StartLevel;
      m_EndLevel = iterator.m_EndLevel;
      m_Queue = iterator.m_Queue;
      }
    return *this;
  }

protected:

  /** Return the next node */
  const ValueType & Next();

  /** Return true if the next node exists */
  bool HasNext() const;

private:

  const TreeNodeType * FindNextNode() const;

  const TreeNodeType * FindNextNodeHelp() const;

  int GetLevel(const TreeNodeType *node) const;

  int                                        m_StartLevel;
  int                                        m_EndLevel;
  mutable std::queue< const TreeNodeType * > m_Queue;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelOrderTreeIterator.hxx"
#endif

#endif
