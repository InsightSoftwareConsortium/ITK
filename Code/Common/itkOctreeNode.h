#ifndef __ITKOCTREENODE_H__
#define __ITKOCTREENODE_H__
/*********************************** MH-CRC IPL *****************************************
 * Iowa MH-CRC IPL C Header File
 * Copyright (C) 1998 Nancy C. Andreasen and Vincent A. Magnotta
 * File Name:        itkOctree.h
 * \author Hans J. Johnson
 * \brief Includes the basic class definitions for using Octtrees.
 *****************************************************************************************/

namespace itk {
  enum LeafIdentifier { ZERO=0,ONE=1,TWO=2,THREE=3,FOUR=4,FIVE=5,SIX=6,SEVEN=7 };


  //A forward-declaration
  class OctreeNodeBranch;
  class OctreeBase;
  /**
   * OctreeNode data structure,  OctreeNodes have two states: 1) They are a Colored node and the 
   * m_Branch is a sentinal value indicating the color, or 2) they are a branch node, and m_Branch
   * is a dynamically allocated array of 8 pointers to OctreeNodes.  In the second state, the 8 child
   * OctreeNodes are instantiated by the parent node.
   * \author Hans J. Johnson
   * This class is the basic building block of an octree.  It is rarely used by itself, and commonly
   * used by the Octree class.
   */
  class OctreeNode
  {
  public:
    /**
     * Default constructor
     * \author Hans J. Johnson
     * \post After construction, it is assumed all children of this node are colored
     * with values of 0.
     */
    OctreeNode(void);
    /**
     * Default destructor
     * \author Hans J. Johnson
     */
    virtual ~OctreeNode(void);

    /**
     * Returns the value of the specified Child for for this OctreeNode
     * \author Hans J. Johnson
     * \param ChildID The numerical identifier of the desired child.
     * \return A pointer to the Disired child. NOTE: This is always an
     * instance of an OctreeNode.
     * @{
     */
    OctreeNode & GetChild(const enum LeafIdentifier ChildID) const;
    OctreeNode & GetChild(const enum LeafIdentifier ChildID);
    /** @} */
    /**
     * Determines the color value of the specified Child for for this OctreeNode
     * \author Hans J. Johnson
     * \param ChildID The numerical identifier of the desired child.
     * \return A value between 0 and 255 to indicate the color of the Disired child.
     * \pre Must determine that the specified node is colored (Use IsNodeColored() 
     * member function.  Behavior is undefined when the child is another Octree.
     */
    int GetColor(void) const;
    /**
     * Sets the color value of the specified Child for for this OctreeNode
     * \author Hans J. Johnson
     * \param ChildID The numerical identifier of the desired child.
     * \param NodeColor The disired color of this node.
     * \post All children of the specified child are removed, and the child is set to
     * the desired value.
     */
    void SetColor( int NodeColor);
    /**
     * Sets the color value of the specified Child for for this OctreeNode
     * \author Hans J. Johnson
     * \param ChildID The numerical identifier of the desired child.
     * \param NodeColor The disired color of this node.
     * \post All children of the specified child are removed, and the child is set to
     * the desired value.
     */
    void SetBranch(OctreeNodeBranch * NewBranch);
    /**
     * Determines if the child is a leaf node (colored), or a branch node (uncolored)
     * \author Hans J. Johnson
     * \param ChildID The numerical identifier of the desired child.
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
     * \author Hans J. Johnson
     */
    void RemoveChildren(void);

    /**
     * Each element holds COLOR or pointer to another octree node 
     */
    OctreeNodeBranch * m_Branch; 
    OctreeBase *m_Parent;
  };

  class OctreeNodeBranch
  {
  public:
    OctreeNodeBranch(OctreeBase *parent)
    {
      for(int i = 0; i < 8; i++)
        m_Leaves[i].SetParentOctree(parent);
    }
    inline OctreeNode *GetLeaf(enum LeafIdentifier LeafID)
    {
      return &m_Leaves[LeafID];
    }
  private:
    OctreeNode m_Leaves[8];
  };
} //End of itk Namespace
#endif                          /* __ITKOCTREENODE_H__ */
