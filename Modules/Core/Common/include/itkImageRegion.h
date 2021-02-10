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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkImageRegion_h
#define itkImageRegion_h

#include "itkRegion.h"

#include "itkSize.h"
#include "itkContinuousIndex.h"
#include "itkMath.h"

namespace itk
{
// Forward declaration of ImageBase so it can be declared a friend
// (needed for PrintSelf mechanism)
template <unsigned int VImageDimension>
class ITK_TEMPLATE_EXPORT ImageBase;

/** \class ImageRegion
 * \brief An image region represents a structured region of data.
 *
 * ImageRegion is an class that represents some structured portion or
 * piece of an Image. The ImageRegion is represented with an index and
 * a size in each of the n-dimensions of the image. (The index is the
 * corner of the image, the size is the lengths of the image in each of
 * the topological directions.)
 *
 * \sa Region
 * \sa Index
 * \sa Size
 * \sa MeshRegion
 *
 * \ingroup ImageObjects
 * \ingroup ITKCommon
 *
 * \sphinx
 * \sphinxexample{Core/Common/CreateAnImageRegion,An object which holds the index (start) and size of a region of an
 * image} \sphinxexample{Core/Common/ImageRegionIntersection,Determine image region intersection}
 * \sphinxexample{Core/Common/IsPixelInsideRegion,Determine if a pixel is inside of a region}
 * \sphinxexample{Core/Common/ImageRegionOverlap,Determine the overlap of two regions}
 * \endsphinx
 */
template <unsigned int VImageDimension>
class ITK_TEMPLATE_EXPORT ImageRegion final : public Region
{
public:
  /** Standard class type aliases. */
  using Self = ImageRegion;
  using Superclass = Region;

  /** Standard part of all itk objects. */
  itkTypeMacro(ImageRegion, Region);

  /** Dimension of the image available at compile time. */
  static constexpr unsigned int ImageDimension = VImageDimension;

  /** Dimension one lower than the image unless the image is one dimensional
      in which case the SliceDimension is also one dimensional. */
  static constexpr unsigned int SliceDimension = ImageDimension - (ImageDimension > 1);

  /** Dimension of the image available at run time. */
  static unsigned int
  GetImageDimension()
  {
    return ImageDimension;
  }

  /** Index type alias support An index is used to access pixel values. */
  using IndexType = Index<Self::ImageDimension>;
  using IndexValueType = typename IndexType::IndexValueType;
  using OffsetType = typename IndexType::OffsetType;
  using OffsetValueType = typename OffsetType::OffsetValueType;
  using IndexValueArrayType = IndexValueType[ImageDimension];
  using OffsetTableType = OffsetValueType[ImageDimension + 1];

  /** Size type alias support A size is used to define region bounds. */
  using SizeType = Size<Self::ImageDimension>;
  using SizeValueType = typename SizeType::SizeValueType;

  /** Slice region type alias. SliceRegion is one dimension less than Self. */
  using SliceRegion = ImageRegion<Self::SliceDimension>;

  /** Return the region type. Images are described with structured regions. */
  Superclass::RegionEnum
  GetRegionType() const override
  {
    return Superclass::RegionEnum::ITK_STRUCTURED_REGION;
  }

  /** Constructor. ImageRegion is a lightweight object that is not reference
   * counted, so the constructor is public. Its two data members are filled
   * with zeros (using C++11 default member initializers). */
  ImageRegion() ITK_NOEXCEPT = default;

  /** Destructor. ImageRegion is a lightweight object that is not reference
   * counted, so the destructor is public. */
  ~ImageRegion() override = default;

  /** Copy constructor. ImageRegion is a lightweight object that is not
   * reference counted, so the copy constructor is public. */
  ImageRegion(const Self &) ITK_NOEXCEPT = default;

  /** Constructor that takes an index and size. ImageRegion is a lightweight
   * object that is not reference counted, so this constructor is public. */
  ImageRegion(const IndexType & index, const SizeType & size) ITK_NOEXCEPT
    :
    // Note: Use parentheses instead of curly braces to initialize data members,
    // to avoid AppleClang 6.0.0.6000056 compile errors, "no viable conversion..."
    m_Index(index)
    , m_Size(size)
  {}

  /** Constructor that takes a size and assumes an index of zeros. ImageRegion
   * is lightweight object that is not reference counted so this constructor
   * is public. */
  ImageRegion(const SizeType & size) ITK_NOEXCEPT : m_Size(size)
  {
    // Note: m_Index is initialized by its C++11 default member initializer.
  }

  /** operator=. ImageRegion is a lightweight object that is not reference
   * counted, so operator= is public. */
  Self &
  operator=(const Self &) ITK_NOEXCEPT = default;

  /** Set the index defining the corner of the region. */
  void
  SetIndex(const IndexType & index)
  {
    m_Index = index;
  }

  /** Get index defining the corner of the region. */
  const IndexType &
  GetIndex() const
  {
    return m_Index;
  }
  IndexType &
  GetModifiableIndex()
  {
    return m_Index;
  }

  /** Set the size of the region. This plus the index determines the
   * rectangular shape, or extent, of the region. */
  void
  SetSize(const SizeType & size)
  {
    m_Size = size;
  }

  /** Get the size of the region. */
  const SizeType &
  GetSize() const
  {
    return m_Size;
  }
  SizeType &
  GetModifiableSize()
  {
    return m_Size;
  }

  /** Convenience methods to get and set the size of the particular dimension i.
   */
  void
  SetSize(unsigned int i, SizeValueType sze)
  {
    m_Size[i] = sze;
  }
  SizeValueType
  GetSize(unsigned int i) const
  {
    return m_Size[i];
  }

  /** Convenience methods to get and set the index of the particular dimension
    i. */
  void
  SetIndex(unsigned int i, IndexValueType sze)
  {
    m_Index[i] = sze;
  }
  IndexValueType
  GetIndex(unsigned int i) const
  {
    return m_Index[i];
  }

  /** Get index defining the upper corner of the region. */
  IndexType
  GetUpperIndex() const;

  /** Modify the Size of the ImageRegion so that the provided index will be the upper corner index. */
  void
  SetUpperIndex(const IndexType & idx);

  /** Compute an offset table based on the Size. */
  void
  ComputeOffsetTable(OffsetTableType offsetTable) const;

  /** Compare two regions. */
  bool
  operator==(const Self & region) const ITK_NOEXCEPT
  {
    return (m_Index == region.m_Index) && (m_Size == region.m_Size);
  }

  /** Compare two regions. */
  bool
  operator!=(const Self & region) const ITK_NOEXCEPT
  {
    return !(*this == region);
  }

  /** Test if an index is inside */
  bool
  IsInside(const IndexType & index) const
  {
    for (unsigned int i = 0; i < ImageDimension; i++)
    {
      if (index[i] < m_Index[i])
      {
        return false;
      }
      if (index[i] >= (m_Index[i] + static_cast<IndexValueType>(m_Size[i])))
      {
        return false;
      }
    }
    return true;
  }

  /** Test if a continuous index is inside the region.
   * We take into account the fact that each voxel has its
   * center at the integer coordinate and extends half way
   * to the next integer coordinate. */
  template <typename TCoordRepType>
  bool
  IsInside(const ContinuousIndex<TCoordRepType, VImageDimension> & index) const
  {
    for (unsigned int i = 0; i < ImageDimension; i++)
    {
      if (Math::RoundHalfIntegerUp<IndexValueType>(index[i]) < static_cast<IndexValueType>(m_Index[i]))
      {
        return false;
      }
      // bound is the last valid pixel location
      const auto bound = static_cast<TCoordRepType>(m_Index[i] + m_Size[i] - 0.5);

      /* Note for NaN: test using negation of a positive test in order
       * to always evaluate to true (and thus return false) when index[i]
       * is NaN. The cast above to integer via RoundHalfIntegerUp will cast
       * NaN into a platform-dependent value (large negative, -1 or large
       * positive, empirically). Thus this test here is relied on
       * to 'catch' NaN's. */
      if (!(index[i] <= bound))
      {
        return false;
      }
    }
    return true;
  }

  /** Test if a region (the argument) is completely inside of this region. If
   * the region that is passed as argument to this method, has a size of value
   * zero, then it will not be considered to be inside of the current region,
   * even its starting index is inside. */
  bool
  IsInside(const Self & region) const
  {
    IndexType beginCorner = region.GetIndex();

    if (!this->IsInside(beginCorner))
    {
      return false;
    }
    IndexType        endCorner;
    const SizeType & size = region.GetSize();
    for (unsigned int i = 0; i < ImageDimension; i++)
    {
      endCorner[i] = beginCorner[i] + static_cast<OffsetValueType>(size[i]) - 1;
    }
    if (!this->IsInside(endCorner))
    {
      return false;
    }
    return true;
  }

  /** Get the number of pixels contained in this region. This just
   * multiplies the size components. */
  SizeValueType
  GetNumberOfPixels() const;

  /** Pad an image region by the specified radius. Region can be padded
   * uniformly in all dimensions or can be padded by different amounts
   * in each dimension. */
  void
  PadByRadius(OffsetValueType radius);

  void
  PadByRadius(const IndexValueArrayType radius);

  void
  PadByRadius(const SizeType & radius);

  /** Shrink an image region by the specified radius.  The region can be shrunk
   * uniformly in all dimension or can be shrink by different amounts in each
   * direction.  If the shrink operation fails because the radius is too large,
   * this method returns false. */
  bool
  ShrinkByRadius(OffsetValueType radius);

  bool
  ShrinkByRadius(const IndexValueArrayType radius);

  bool
  ShrinkByRadius(const SizeType & radius);

  /** Crop a region by another region. If this region is outside of the
   * crop, this method returns false and does not modify the
   * region. Otherwise, this method returns true and the region is
   * modified to reflect the crop. */
  bool
  Crop(const Self & region);

  /** Slice a region, producing a region that is one dimension lower
   * than the current region. Parameter "dim" specifies which dimension
   * to remove. */
  SliceRegion
  Slice(const unsigned int dim) const;

protected:
  /** Methods invoked by Print() to print information about the object
   * including superclasses. Typically not called by the user (use Print()
   * instead) but used in the hierarchical print process to combine the
   * output of several classes.  */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  IndexType m_Index = { { 0 } };
  SizeType  m_Size = { { 0 } };

  /** Friends of ImageRegion */
  friend class ImageBase<VImageDimension>;
};

template <unsigned int VImageDimension>
std::ostream &
operator<<(std::ostream & os, const ImageRegion<VImageDimension> & region);
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkImageRegion.hxx"
#endif

#endif
