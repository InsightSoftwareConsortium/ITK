/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkOctree.h
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) 2002 Insight Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __ITKOCTREE_H__
#define __ITKOCTREE_H__

#include <cstdlib>
#include "itkOctreeNode.h"
#include "itkImage.h"
/*
 * Octree data structure
 */
namespace itk {

  enum
    {
      B2_MASKFILE_BLACK = 0,
      B2_MASKFILE_WHITE = 1,
      B2_MASKFILE_GRAY = 2
    };
  /**
   * The enumeration to define the planar orientation of the octree
   */
  enum OctreePlaneType 
    {
      UNKNOWN_PLANE, /** < The plane is Unknown */
      SAGITAL_PLANE, /** < The plane is Sagital */
      CORONAL_PLANE, /** < The plane is Coronal */
      TRANSVERSE_PLANE  /** < The plane is Transverse */
    };

  /**
   * \class OctreeBase
   * \brief Provides non-templated access to templated instances of Octree
   * 
   */
  class OctreeBase : public Object
  {
  public:
    /** Standard class typedefs. */
    typedef OctreeBase Self;
    typedef SmartPointer<Self>  Pointer;

    /** Get the actual tree base
     *
     * Returns the tree, or 0 if the Octree isn't built yet
     */
    virtual OctreeNode *GetTree() = 0;
    /** Get tree depth.
     *
     * Depth represents x, for the smallest 2^x >= largest image dimension
     */
    virtual unsigned int GetDepth() = 0;
    /** Get tree width.
     *
     * Width == smallest 2^x >= largest image dimension
     * i.e. 2^Depth == Width
     */
    virtual unsigned int GetWidth() = 0;

    /** Set the depth, e.g. when reading tree from a file. */
    virtual void SetDepth(unsigned int depth) = 0;

    /** Set width, e.g. when reading from a file. */
    virtual void SetWidth(unsigned int width) = 0;
    /** Build an Octree from an Image's pixel buffer.
     *
     * Method needed for ImageIO class, which has no handle on image, just
     * the pixel buffer.
     */
    virtual void BuildFromBuffer(const void *buffer,
                                 const int xsize,const int ysize,const int zsize) = 0;
    /** Get the ColorTable Pointer
     * 
     * Returns color table pointer for this tree.
     *
     * Each Octree has an array of char whose size = the # of color table
     * entries. Each Node in the Octree points either to 8 sub-nodes, or
     * into the ColorTable;  The color table isn't actually used to hold
     * data; it simply provides a range of unique addresses that are distinct
     * from the address of any valid subtree.
     */
    virtual const char *GetColorTable() const = 0;

    /** Get the size of the Color Table  */
    virtual int GetColorTableSize() const = 0;
  };

  /**
   * \class Octree
   * \brief represent a 3D Image with an Octree data structure.
   *
   * Parameterized on Pixel type of the image, # of colors in color table,
   * and a Mapping function, derived from itk::FunctionBase
   */
  template <class TPixel,unsigned int ColorTableSize,class MappingFunctionType>
  class Octree: public OctreeBase
  {
  public:
    /** Standard class typedefs. */
    typedef Octree            Self;
    typedef OctreeBase  Superclass;
    typedef SmartPointer<Self>  Pointer;
    typedef Image<TPixel,3> ImageType;
    typedef typename ImageType::Pointer ImageTypePointer;
    /** Method for creation through the object factory. */
    itkNewMacro(Self);

    /** Run-time type information (and related methods). */
    itkTypeMacro(AnalyzeImageIO, Superclass);

    ImageTypePointer GetImage();
    virtual void BuildFromBuffer(const void *buffer,const int xsize,const int ysize,const int zsize);
    void BuildFromImage(Image<TPixel,3> *fromImage);

    Octree(void);
    ~Octree(void);
    void SetColor(unsigned int color) { m_Tree.SetColor(color); }
    void SetTree(OctreeNodeBranch *branch) { m_Tree.SetBranch(branch); }
    void SetTrueDims(const unsigned int Dim0, const unsigned int Dim1, 
                     const unsigned int Dim2);

    unsigned int getValue(const unsigned int Dim0, const unsigned int Dim1, 
                           const unsigned int Dim2);
    
    virtual void SetWidth(unsigned int width);
    virtual void SetDepth(unsigned int depth);
    virtual unsigned int GetWidth();
    virtual unsigned int GetDepth();

    virtual OctreeNode *GetTree();
    virtual const char *GetColorTable() const;
    virtual int GetColorTableSize() const;
  private:
    Octree(const Self&);         // purposely not implemented
    void operator=(const Self&); // purposely not implemented

    OctreeNodeBranch *maskToOctree (const TPixel* Mask, unsigned width, unsigned x, 
                                    unsigned y, unsigned z, unsigned xsize, 
                                    unsigned ysize, unsigned zsize);
    enum OctreePlaneType m_Plane; // The orientation of the plane for this octree
    unsigned int m_Width;         // The width of the Octree 
                                  // ( This is always a power of 2, and large 
                                  // enough to contain MAX(DIMS[1,2,3]))
    unsigned int m_Depth;         // < The depth of the Octree
    unsigned int m_TrueDims[3];   // The true dimensions of the image
    char m_ColorTable[ColorTableSize];
    OctreeNode m_Tree;
    // OctreeColorMapFunction m_ColorMapFunction;
    MappingFunctionType m_MappingFunction;
  };

}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkOctree.txx"
#endif

#endif                          /* __ITKOCTREE_H__ */
