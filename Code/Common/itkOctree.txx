/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkOctree.txx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) 2002 Insight Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

Portions of this code are covered under the VTK copyright.
See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkOctree_txx
#define _itkOctree_txx
#include "itkOctree.h"

namespace itk
{
  template <class TPixel,unsigned int ColorTableSize,class MappingFunctionType>
  Octree<TPixel,ColorTableSize,MappingFunctionType>::
  Octree(void):  m_Plane(UNKNOWN_PLANE), m_Width(0),m_Depth(0),m_Tree()
  {
    m_TrueDims[0]=0;
    m_TrueDims[1]=1;
    m_TrueDims[2]=2;
    m_Tree.SetParentOctree(this);
  }

  template <class TPixel,unsigned int ColorTableSize,class MappingFunctionType>
  Octree<TPixel,ColorTableSize,MappingFunctionType>::
  ~Octree(void) {/*Nothing to be done here*/};
  
  template <class TPixel,unsigned int ColorTableSize,class MappingFunctionType>
  void 
  Octree<TPixel,ColorTableSize,MappingFunctionType>::
  SetTrueDims(const unsigned int Dim0, const unsigned int Dim1,
              const unsigned int Dim2)
  {
    this->m_TrueDims[0]=Dim0;
    this->m_TrueDims[1]=Dim1;
    this->m_TrueDims[2]=Dim2;
  }

  /*This is moving bits to get the values of the 8 octants
    Possible values are the 3 bits to be set.
    0   000 Contains origin
    1   001
    2   010
    3   011
    4   100
    5   101
    6   110
    7   111 Contains extents
    ....^^^
    ....|| \This value is 1 if requested X voxel is greater than X 
    ....    centerline of subcube
    ....| \This value is 1 if requested Y voxel is greater than Y 
    .......centerline of subcube
    ....   \This value is 1 if requested Z voxel is greater than Z centerline 
    .......of subcube
    \author Hans J. Johnson, adapted from Vincent A. Magnotta
    \param VoxX The desired voxel
    \param VoxY The desired voxel
    \param VoxZ The desired voxel
    \param CenterLineX The division line between octants
    \param CenterLineY The division line between octants
    \param CenterLineZ The division line between octants
    \return The octant that the voxel falls into.
  */
  inline unsigned int OCTREE_OCTANT(const int VoxX, const int CenterLineX,
                                    const int VoxY, const int CenterLineY,
                                    const int VoxZ, const int CenterLineZ)
  {
    return (
            (
             (static_cast<unsigned int>(((VoxZ)>=(CenterLineZ)))<<2)
             |  (static_cast<unsigned int>(((VoxY)>=(CenterLineY)))<<1)
             )
            | (static_cast<unsigned int>((VoxX)>=(CenterLineX)))
            );
  }
  /**
   * \defgroup Octant directional identifying functions
   * These functions determine if the directions are in the "lower" or 
   * "upper" portion of the Octree in the given directions.
   * @{
   */
  inline unsigned int XF(const unsigned int octantID)
  {
    return octantID&1; //Just return 1 if 0th bit is a one
  }
  inline unsigned int YF(const unsigned int octantID)
  {
    return (octantID>>1)&1; //Just return 1 if 1st bit is a one
  }
  inline unsigned int ZF(const unsigned int octantID)
  {
    return (octantID>>2)&1; //Just return 1 if 2nd bit is a one
  }
  /** @} */ // End of defgroup

  template <class TPixel,unsigned int ColorTableSize,class MappingFunctionType>
  unsigned int
  Octree<TPixel,ColorTableSize,MappingFunctionType>::
  GetValue(const unsigned int Dim0, 
           const unsigned int Dim1,
           const unsigned int Dim2)
  {
    if ((Dim2 >= this->m_TrueDims[2]) || 
        (Dim1 >= this->m_TrueDims[1]) || (Dim0 >= this->m_TrueDims[0]))
      {
      return 0;
      }

    //Define CurrentOctreeNode at the Octree head Node
    OctreeNode * CurrentOctreeNode = &m_Tree;
    //Define the origin of current OctreeNode
    unsigned int ox = 0, oy = 0, oz = 0; 
    //Define the halfwidth, this will be changed inside of while loop
    unsigned int halfwidth = this->m_Width; 


    while ((CurrentOctreeNode->IsNodeColored())==false)
      {
      //NOTE:  halfwidth=halfwidth/2 is the same as halfwidth >> 1
      halfwidth=halfwidth >> 1; 
      const unsigned int octantID = 
        OCTREE_OCTANT (Dim0, ox + halfwidth, Dim1, 
                       oy + halfwidth, Dim2, oz + halfwidth);
      //Determine new origin for next child.
      ox = ox + XF(octantID) * halfwidth;
      oy = oy + YF(octantID) * halfwidth;
      oz = oz + ZF(octantID) * halfwidth;

      CurrentOctreeNode = 
        &CurrentOctreeNode->GetChild(static_cast<enum LeafIdentifier>(octantID));
      }
    return CurrentOctreeNode->GetColor();
  }

  
  template <class TPixel,unsigned int ColorTableSize,class MappingFunctionType>
  OctreeNodeBranch *
  Octree<TPixel,ColorTableSize,MappingFunctionType>::
  maskToOctree (const TPixel* Mask, unsigned width, unsigned x, 
                unsigned y, unsigned z, unsigned xsize, 
                unsigned ysize, unsigned zsize)
  {
    if ((x >= xsize) || (y >= ysize) || (z >= zsize))
      {
      return reinterpret_cast<OctreeNodeBranch *>(m_ColorTable
                                                  +B2_MASKFILE_BLACK);
      }
    if (width == 1)
      {
      return reinterpret_cast<OctreeNodeBranch *>
        (m_ColorTable + m_MappingFunction.Evaluate(&Mask[z * ysize * xsize + y * xsize + x]));
      }
    width /= 2;
    OctreeNodeBranch *nodeArray[8];
    nodeArray[0] = this->maskToOctree (Mask, width, x, y, z,
                                       xsize, ysize, zsize);
    nodeArray[1] = this->maskToOctree (Mask, width, x + width, y, z, 
                                       xsize, ysize, zsize);
    nodeArray[2] = this->maskToOctree (Mask, width, x, y + width, z, 
                                       xsize, ysize, zsize);
    nodeArray[3] = this->maskToOctree (Mask, width, x + width, y + width, z, 
                                       xsize, ysize, zsize);
    nodeArray[4] = this->maskToOctree (Mask, width, x, y, z + width, 
                                       xsize, ysize, zsize);
    nodeArray[5] = this->maskToOctree (Mask, width, x + width, y, z + width, 
                                       xsize, ysize, zsize);
    nodeArray[6] = this->maskToOctree (Mask, width, x, y + width, z + width, 
                                       xsize, ysize, zsize);
    nodeArray[7] = this->maskToOctree (Mask, width, x + width, y + width, 
                                       z + width, xsize, ysize, zsize);

    if((nodeArray[0] == nodeArray[1]) &&
       (nodeArray[0] == nodeArray[2]) &&
       (nodeArray[0] == nodeArray[3]) &&
       (nodeArray[0] == nodeArray[4]) &&
       (nodeArray[0] == nodeArray[5]) &&
       (nodeArray[0] == nodeArray[6]) &&
       (nodeArray[0] == nodeArray[7]))
      {
      return nodeArray[0];
      }
    else
      {
      OctreeNodeBranch *q = new OctreeNodeBranch(this);
      OctreeNode *newbranch;

      newbranch = q->GetLeaf(ZERO);
      newbranch->SetBranch(nodeArray[ZERO]);

      newbranch = q->GetLeaf(ONE);
      newbranch->SetBranch(nodeArray[ONE]);

      newbranch = q->GetLeaf(TWO);
      newbranch->SetBranch(nodeArray[TWO]);

      newbranch = q->GetLeaf(THREE);
      newbranch->SetBranch(nodeArray[THREE]);

      newbranch = q->GetLeaf(FOUR);
      newbranch->SetBranch(nodeArray[FOUR]);

      newbranch = q->GetLeaf(FIVE);
      newbranch->SetBranch(nodeArray[FIVE]);

      newbranch = q->GetLeaf(SIX);
      newbranch->SetBranch(nodeArray[SIX]);

      newbranch = q->GetLeaf(SEVEN);
      newbranch->SetBranch(nodeArray[SEVEN]);

      return (q);
      }
  }

  template <class TPixel,unsigned int ColorTableSize,class MappingFunctionType>
  void 
  Octree<TPixel,ColorTableSize,MappingFunctionType>::
  BuildFromBuffer(const void *frombuffer,const int xsize,const int ysize,const int zsize)
  {
    unsigned maxSize = xsize >= ysize ?
      (xsize >= zsize ? xsize : zsize) :
      (ysize >= zsize ? ysize : zsize);
    unsigned width = 1;
    unsigned depth = 0;
    while(width < maxSize)
      {
      width *= 2;
      depth++;
      }
    this->SetDepth(depth);
    this->SetWidth(width);
    m_TrueDims[0] = xsize;
    m_TrueDims[1] = ysize;
    m_TrueDims[2] = zsize;
  const TPixel *bufcast = static_cast<const TPixel *>(frombuffer);
    OctreeNodeBranch *branch = 
      this->maskToOctree(bufcast,width,0,0,0,
                         xsize,ysize,zsize);
    m_Tree.SetBranch(branch);
  }
  template <class TPixel,unsigned int ColorTableSize,class MappingFunctionType>
  void 
  Octree<TPixel,ColorTableSize,MappingFunctionType>::
  BuildFromImage(ImageType *fromImage)
  {
    const typename Image<TPixel,3>::RegionType &region =
      fromImage->GetLargestPossibleRegion();
    unsigned int xsize,ysize,zsize;
    xsize = region.GetSize(0);
    ysize = region.GetSize(1);
    zsize = region.GetSize(2);
    this->BuildFromBuffer(static_cast<void *>(fromImage->GetBufferPointer()),
                          xsize,ysize,zsize);
  }

  template <class TPixel,unsigned int ColorTableSize,class MappingFunctionType>
  typename Octree<TPixel,ColorTableSize,MappingFunctionType>::ImageTypePointer 
  Octree<TPixel,ColorTableSize,MappingFunctionType>::
  GetImage()
  {
    unsigned int i,j,k;
    typename ImageType::SizeType imageSize = {{0,0,0}};
    typedef typename ImageType::SizeType::SizeValueType SizeValueType;
    SizeValueType sizes[3];
    sizes[0] = m_TrueDims[0];
    sizes[1] = m_TrueDims[1];
    sizes[2] = m_TrueDims[2];
    imageSize.SetSize(sizes);
    const typename ImageType::IndexType imageIndex = {{0,0,0}};
    typename ImageType::RegionType region;
    region.SetSize(imageSize);
    region.SetIndex(imageIndex);
    typename ImageType::Pointer img = ImageType::New();
    img->SetLargestPossibleRegion(region);
    img->SetBufferedRegion(region);
    img->SetRequestedRegion(region);
    img->Allocate();
    typename ImageType::IndexType setIndex;
    for(i = 0; i < m_TrueDims[0]; i++)
      {
      setIndex[0] = i;
      for(j = 0; j < m_TrueDims[0]; j++)
        {
        setIndex[1] = j;
        for(k = 0; k < m_TrueDims[0]; k++) 
          {
          setIndex[2] = k;
          img->SetPixel(setIndex,(TPixel)this->GetValue(i,j,k));
          }
        }
      }
    return img;
  }

  template <class TPixel,unsigned int ColorTableSize,class MappingFunctionType>
  OctreeNode *
  Octree<TPixel,ColorTableSize,MappingFunctionType>::
  GetTree() 
  { 
    return &m_Tree; 
  }

  template <class TPixel,unsigned int ColorTableSize,class MappingFunctionType>
  void 
  Octree<TPixel,ColorTableSize,MappingFunctionType>::
  SetWidth(unsigned int width) 
  {
    m_Width = width;
  }

  template <class TPixel,unsigned int ColorTableSize,class MappingFunctionType>
  void 
  Octree<TPixel,ColorTableSize,MappingFunctionType>::
  SetDepth(unsigned int depth)
  {
    m_Depth = depth;
  }

  template <class TPixel,unsigned int ColorTableSize,class MappingFunctionType>
  unsigned int 
  Octree<TPixel,ColorTableSize,MappingFunctionType>::
  GetWidth()
  {
    return m_Width;
  }

  template <class TPixel,unsigned int ColorTableSize,class MappingFunctionType>
  unsigned int 
  Octree<TPixel,ColorTableSize,MappingFunctionType>::
  GetDepth()
  {
    return m_Depth;
  }
  template <class TPixel,unsigned int ColorTableSize,class MappingFunctionType>
  const char *
  Octree<TPixel,ColorTableSize,MappingFunctionType>::
  GetColorTable() const 
  { 
    return m_ColorTable; 
  }
  template <class TPixel,unsigned int ColorTableSize,class MappingFunctionType>
  int 
  Octree<TPixel,ColorTableSize,MappingFunctionType>::
  GetColorTableSize() const 
  {
    return ColorTableSize; 
  }

}


#endif
