/*!\verbatim***************************** MH-CRC IPL ******************************************
 * Iowa MH-CRC IPL C++ Source File
 * Copyright (C) 2002 Iowa Mental Health Clinical Research Center
 * \endverbatim
 * \file        itkOctree.cxx
 * \author      Hans J. Johnson
 * \brief       Implements member functions for OctreeNode Classes
 *****************************************************************************************/
/* Local Include Files */
#include "itkOctree.h"
#include "itkOctreeNode.h"

namespace itk {


/*
     * ====================================================================================
     * ====================================================================================
     * ====================================================================================
     */
OctreeNode::OctreeNode(void)
{
  m_Parent = 0;
  m_Branch = 0;
}

OctreeNode::~OctreeNode(void)
{
  this->RemoveChildren();
}

OctreeNode & OctreeNode::GetChild(const enum LeafIdentifier ChildID) const
{
  return *m_Branch->GetLeaf(ChildID);
}

OctreeNode & OctreeNode::GetChild(const enum LeafIdentifier ChildID)
{
  return *m_Branch->GetLeaf(ChildID);
}

int OctreeNode::GetColor(void) const
{
  int x = reinterpret_cast<const char *>(m_Branch) - m_Parent->GetColorTable();
  //
  // you'll want to indicate that the branch
  // is a subtree and not in fact a color.
  if(x >= 0 && x < m_Parent->GetColorTableSize())
    return x;
  return -1;
}

void OctreeNode::SetColor(int color)
{
  this->RemoveChildren();
  m_Branch = reinterpret_cast<OctreeNodeBranch *>
    (const_cast<char *>(m_Parent->GetColorTable()) + color);
  return;
}

void OctreeNode::SetBranch(OctreeNodeBranch * NewBranch)
{
  this->RemoveChildren();
  m_Branch=NewBranch;
  return;
}

/**
 * \brief Determines if the current node is colored
 * \param void
 * \return  bool true if this node is colored
 */
bool OctreeNode::IsNodeColored(void) const
{
  const char *colorTable = m_Parent->GetColorTable();
  const OctreeNodeBranch *first = 
    reinterpret_cast<const OctreeNodeBranch *>(&(colorTable[0]));
  const OctreeNodeBranch *last = 
    reinterpret_cast<const OctreeNodeBranch *>(&(colorTable[m_Parent->GetColorTableSize()-1]));
  if(this->m_Branch >= first && this->m_Branch <= last)
    return true;
  return false;
}

void OctreeNode::RemoveChildren(void)
{
  if(m_Branch != 0 && !this->IsNodeColored())
    {
    delete m_Branch;
    m_Branch = reinterpret_cast<OctreeNodeBranch *>
      (&(const_cast<char *>(m_Parent->GetColorTable())[0]));
    }
}



} //End of itk Namespace
