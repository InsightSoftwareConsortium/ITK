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
#ifndef itkOctree_h
#define itkOctree_h

#include "itkOctreeNode.h"
#include "itkImage.h"
#include "itkCommonEnums.h"

namespace itk
{
enum
{
  B2_MASKFILE_BLACK = 0,
  B2_MASKFILE_WHITE = 1,
  B2_MASKFILE_GRAY = 2
};

/**
 * \class OctreeBase
 * \brief Provides non-templated access to templated instances of Octree.
 *
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT OctreeBase : public Object
{
public:
  /** Standard class type aliases. */
  using Self = OctreeBase;
  using Pointer = SmartPointer<Self>;

  using OctreeEnum = OctreeEnums::Octree;

#if !defined(ITK_LEGACY_REMOVE)
  // We need to expose the enum values at the class level
  // for backwards compatibility
  static constexpr OctreeEnum UNKNOWN_PLANE = OctreeEnum::UNKNOWN_PLANE;
  static constexpr OctreeEnum SAGITAL_PLANE = OctreeEnum::SAGITAL_PLANE;
  static constexpr OctreeEnum CORONAL_PLANE = OctreeEnum::CORONAL_PLANE;
  static constexpr OctreeEnum TRANSVERSE_PLANE = OctreeEnum::TRANSVERSE_PLANE;
#endif

  /** Get the actual tree base
   *
   * Returns the tree, or 0 if the Octree isn't built yet
   */
  virtual OctreeNode *
  GetTree() = 0;

  /** Get tree depth.
   *
   * Depth represents x, for the smallest 2^x >= largest image dimension
   */
  virtual unsigned int
  GetDepth() = 0;

  /** Get tree width.
   *
   * Width == smallest 2^x >= largest image dimension
   * i.e. 2^Depth == Width
   */
  virtual unsigned int
  GetWidth() = 0;

  /** Set the depth, e.g. when reading tree from a file. */
  virtual void
  SetDepth(unsigned int depth) = 0;

  /** Set width, e.g. when reading from a file. */
  virtual void
  SetWidth(unsigned int width) = 0;

  /** Build an Octree from an Image's pixel buffer.
   *
   * Method needed for ImageIO class, which has no handle on image, just
   * the pixel buffer.
   */
  virtual void
  BuildFromBuffer(const void *       buffer,
                  const unsigned int xsize,
                  const unsigned int ysize,
                  const unsigned int zsize) = 0;

  /** Get the ColorTable Pointer
   *
   * Returns color table pointer for this tree.
   *
   * Each Octree has an array of OctreeNodeBranch whose size = the number of
   * color table entries. Each Node in the Octree points either to 8 sub-nodes,
   * or into the ColorTable;  The color table isn't actually used to hold
   * data; it simply provides a range of unique addresses that are distinct
   * from the address of any valid subtree.
   */
  virtual const OctreeNodeBranch *
  GetColorTable() const = 0;

  /** Get the size of the Color Table  */
  virtual int
  GetColorTableSize() const = 0;
};

/**
 * \class Octree
 * \brief Represent a 3D Image with an Octree data structure.
 *
 * Parameterized on Pixel type of the image, number of colors in color
 * table, and a Mapping function, derived from itk::FunctionBase
 * \ingroup ITKCommon
 */
template <typename TPixel, unsigned int ColorTableSize, typename MappingFunctionType>
class ITK_TEMPLATE_EXPORT Octree : public OctreeBase
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(Octree);

  /** Standard class type aliases. */
  using Self = Octree;
  using Superclass = OctreeBase;
  using Pointer = SmartPointer<Self>;
  using ImageType = Image<TPixel, 3>;
  using ImageTypePointer = typename ImageType::Pointer;
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(Octree, Superclass);

  ImageTypePointer
  GetImage();

  void
  BuildFromBuffer(const void *       buffer,
                  const unsigned int xsize,
                  const unsigned int ysize,
                  const unsigned int zsize) override;

  void BuildFromImage(Image<TPixel, 3> * fromImage);

  Octree();
  ~Octree() override;
  void
  SetColor(unsigned int color)
  {
    m_Tree.SetColor(color);
  }
  void
  SetTree(OctreeNodeBranch * branch)
  {
    m_Tree.SetBranch(branch);
  }
  void
  SetTrueDims(const unsigned int Dim0, const unsigned int Dim1, const unsigned int Dim2);

  int
  GetValue(const unsigned int Dim0, const unsigned int Dim1, const unsigned int Dim2);

  void
  SetWidth(unsigned int width) override;

  void
  SetDepth(unsigned int depth) override;

  unsigned int
  GetWidth() override;

  unsigned int
  GetDepth() override;

  /***
   * Exposes enum values for backwards compatibility
   * */
#if !defined(ITK_LEGACY_REMOVE)
  static constexpr OctreeEnum UNKNOWN_PLANE = OctreeEnum::UNKNOWN_PLANE;
  static constexpr OctreeEnum SAGITAL_PLANE = OctreeEnum::SAGITAL_PLANE;
  static constexpr OctreeEnum CORONAL_PLANE = OctreeEnum::CORONAL_PLANE;
  static constexpr OctreeEnum TRANSVERSE_PLANE = OctreeEnum::TRANSVERSE_PLANE;
#endif

  OctreeNode *
  GetTree() override;

  const OctreeNodeBranch *
  GetColorTable() const override;

  int
  GetColorTableSize() const override;

private:
  OctreeNodeBranch *
  maskToOctree(const TPixel * Mask,
               unsigned       width,
               unsigned       x,
               unsigned       y,
               unsigned       z,
               unsigned       xsize,
               unsigned       ysize,
               unsigned       zsize);

  OctreeEnum m_Plane{ OctreeEnum::UNKNOWN_PLANE }; // The orientation of the plane for this octree

  // The width of the Octree. This is always a power of 2,
  // and large enough to contain MAX(DIMS[1,2,3])
  unsigned int m_Width{ 0 };

  unsigned int        m_Depth{ 0 };  // The depth of the Octree
  unsigned int        m_TrueDims[3]; // The true dimensions of the image
  OctreeNodeBranch    m_ColorTable[ColorTableSize];
  OctreeNode          m_Tree;
  MappingFunctionType m_MappingFunction;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkOctree.hxx"
#endif

#endif // itkOctree_h
