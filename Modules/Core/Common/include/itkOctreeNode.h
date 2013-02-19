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
#ifndef __itkOctreeNode_h
#define __itkOctreeNode_h
#include "itkMacro.h"
namespace itk
{
enum LeafIdentifier { ZERO = 0, ONE = 1, TWO = 2, THREE = 3, FOUR = 4, FIVE = 5, SIX = 6, SEVEN = 7 };

//A forward-declaration
class OctreeNodeBranch;
class OctreeBase;

/**
 * \class OctreeNode
 * \brief A data structure representing a node in an Octree.
 *
 * OctreeNode is the basic building block of an octree. It is rarely used by
 * itself, and commonly used by the Octree class.
 *
 * OctreeNodes have two states: 1) They are a Colored node and the m_Branch is
 * a sentinal value indicating the color, or 2) they are a branch node, and
 * m_Branch is a dynamically allocated array of 8 pointers to OctreeNodes. In
 * the second state, the 8 child OctreeNodes are instantiated by the parent node.
 *
 * \author Hans J. Johnson
 * \todo FIXME copy & paste documentation in all methods.
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT OctreeNode
{
public:
  /**
   * Default constructor
   * \post After construction, it is assumed all children of this node are colored
   * with values of 0.
   */
  OctreeNode(void);

  /**
   * Default destructor
   */
  virtual ~OctreeNode(void);

  /**
   * Returns the value of the specified Child for this OctreeNode
   * \param ChildID The numerical identifier of the desired child.
   * \return A pointer to the Desired child. NOTE: This is always an
   * instance of an OctreeNode.
   * @{
   */
  OctreeNode & GetChild(const enum LeafIdentifier ChildID) const;

  OctreeNode & GetChild(const enum LeafIdentifier ChildID);
  /** @}
  */

  /**
   * Determines the color value of the specified Child for this OctreeNode
   * \pre Must determine that the specified node is colored (Use IsNodeColored()
   * member function.  Behavior is undefined when the child is another Octree.
   * \return A value between 0 and 255 to indicate the color of the Desired child.
   */
  long int GetColor(void) const;

  /**
   * Sets the color value of the specified Child for this OctreeNode
   * \param NodeColor The desired color of this node.
   * \post All children of the specified child are removed, and the child is set to
   * the desired value.
   */
  void SetColor(int NodeColor);

  /**
   * Sets the color value of the specified Child for this OctreeNode
   * \post All children of the specified child are removed, and the child is set to
   * the desired value.
   */
  void SetBranch(OctreeNodeBranch *NewBranch);

  /**
   * Determines if the child is a leaf node (colored), or a branch node (uncolored)
   * \return true if it is colored, false if it is not
   */
  bool IsNodeColored(void) const;

  inline void SetParentOctree(OctreeBase *parent)
  {
    m_Parent = parent;
  }

protected:

private:
  /**
   * Removes all children from this node down, and sets the value
   * value of the children to background.
   */
  void RemoveChildren(void);

  /**
   * Each element holds COLOR or pointer to another octree node
   */
  OctreeNodeBranch *m_Branch;
  OctreeBase *      m_Parent;
};

class OctreeNodeBranch
{
public:
  OctreeNodeBranch(OctreeBase *parent)
  {
    for ( int i = 0; i < 8; i++ )
      {
      m_Leaves[i].SetParentOctree(parent);
      }
  }

  inline OctreeNode * GetLeaf(enum LeafIdentifier LeafID)
  {
    return &m_Leaves[LeafID];
  }

private:
  OctreeNode m_Leaves[8];
};
} //End of itk Namespace
#endif                          /* __itkOctreeNode_h */
