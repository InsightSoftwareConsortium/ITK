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
#ifndef itkOctreeNode_h
#define itkOctreeNode_h
#include "itkMacro.h"
#include "itkCommonEnums.h"

namespace itk
{

// Forward reference because of circular dependencies
class ITK_FORWARD_EXPORT OctreeNodeBranch;
class ITK_FORWARD_EXPORT OctreeBase;

/**
 * \class OctreeNode
 * \brief A data structure representing a node in an Octree.
 *
 * OctreeNode is the basic building block of an octree. It is rarely used by
 * itself, and commonly used by the Octree class.
 *
 * OctreeNodes have two states: 1) They are a Colored node and the m_Branch is
 * a sentinel value indicating the color, or 2) they are a branch node, and
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
  OctreeNode();

  /**
   * Default destructor
   */
  virtual ~OctreeNode();

  using LeafIdentifierEnum = OctreeEnums::LeafIdentifier;
#if !defined(ITK_LEGACY_REMOVE)
  // We need to expose the enum values at the class level
  // for backwards compatibility
  static constexpr LeafIdentifierEnum ZERO = LeafIdentifierEnum::ZERO;
  static constexpr LeafIdentifierEnum ONE = LeafIdentifierEnum::ONE;
  static constexpr LeafIdentifierEnum TWO = LeafIdentifierEnum::TWO;
  static constexpr LeafIdentifierEnum THREE = LeafIdentifierEnum::THREE;
  static constexpr LeafIdentifierEnum FOUR = LeafIdentifierEnum::FOUR;
  static constexpr LeafIdentifierEnum FIVE = LeafIdentifierEnum::FIVE;
  static constexpr LeafIdentifierEnum SIX = LeafIdentifierEnum::SIX;
  static constexpr LeafIdentifierEnum SEVEN = LeafIdentifierEnum::SEVEN;
#endif

  /**
   * Returns the value of the specified Child for this OctreeNode
   * \param ChildID The numerical identifier of the desired child.
   * \return A pointer to the Desired child. NOTE: This is always an
   * instance of an OctreeNode.
   * @{
   */
  OctreeNode &
  GetChild(const LeafIdentifierEnum ChildID) const;

  OctreeNode &
  GetChild(const LeafIdentifierEnum ChildID);
  /** @}
   */

  /**
   * Determines the color value of the specified Child for this OctreeNode
   * \pre Must determine that the specified node is colored (Use IsNodeColored()
   * member function.  Behavior is undefined when the child is another Octree.
   * \return A value between 0 and 255 to indicate the color of the Desired child.
   */
  long int
  GetColor() const;

  /**
   * Sets the color value of the specified Child for this OctreeNode
   * \param NodeColor The desired color of this node.
   * \post All children of the specified child are removed, and the child is set to
   * the desired value.
   */
  void
  SetColor(int color);

  /**
   * Sets the color value of the specified Child for this OctreeNode
   * \post All children of the specified child are removed, and the child is set to
   * the desired value.
   */
  void
  SetBranch(OctreeNodeBranch * NewBranch);

  /**
   * Determines if the child is a leaf node (colored), or a branch node (uncolored)
   * \return true if it is colored, false if it is not
   */
  bool
  IsNodeColored() const;

  inline void
  SetParentOctree(OctreeBase * parent)
  {
    m_Parent = parent;
  }

protected:
private:
  /**
   * Removes all children from this node down, and sets the value
   * value of the children to background.
   */
  void
  RemoveChildren();

  /**
   * Each element holds COLOR or pointer to another octree node
   */
  OctreeNodeBranch * m_Branch;
  OctreeBase *       m_Parent;
};

class ITKCommon_EXPORT OctreeNodeBranch
{
public:
  OctreeNodeBranch() = default;
  OctreeNodeBranch(OctreeBase * parent)
  {
    for (auto & leaf : m_Leaves)
    {
      leaf.SetParentOctree(parent);
    }
  }

  inline OctreeNode *
  GetLeaf(OctreeNode::LeafIdentifierEnum LeafID)
  {
    return &m_Leaves[static_cast<uint8_t>(LeafID)];
  }

private:
  OctreeNode m_Leaves[8];
};

} // namespace itk
#endif /* itkOctreeNode_h */
