/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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
#include <type_traits> // For conditional and integral_constant.
#include <utility>     // For tuple_element and tuple_size.

// Macro added to each `ImageRegion` member function that overrides a virtual member function of `Region`, when legacy
// support is enabled. Without legacy support, `ImageRegion` will no longer inherit from `Region`, so then those
// `ImageRegion` member functions will no longer override.
#ifdef ITK_LEGACY_REMOVE
#  define itkRegionOverrideMacro // nothing
#else
#  define itkRegionOverrideMacro override
#endif

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
class ITK_TEMPLATE_EXPORT ImageRegion final
#ifndef ITK_LEGACY_REMOVE
  // This inheritance is only there when legacy support is enabled.
  : public Region
#endif
{
public:
  /** Standard class type aliases. */
  using Self = ImageRegion;

#ifndef ITK_LEGACY_REMOVE
  using Superclass = Region;
#endif

  /** Standard part of all itk objects. */
  const char *
  GetNameOfClass() const itkRegionOverrideMacro
  {
    return "ImageRegion";
  }

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
  using IndexType = Index<VImageDimension>;
  using IndexValueType = typename IndexType::IndexValueType;
  using OffsetType = typename IndexType::OffsetType;
  using OffsetValueType = typename OffsetType::OffsetValueType;
  using IndexValueArrayType = IndexValueType[ImageDimension];
  using OffsetTableType = OffsetValueType[ImageDimension + 1];

  /** Size type alias support A size is used to define region bounds. */
  using SizeType = Size<VImageDimension>;
  using SizeValueType = typename SizeType::SizeValueType;

  /** Slice region type alias. SliceRegion is one dimension less than Self. */
  using SliceRegion = ImageRegion<Self::SliceDimension>;

  /** Return the region type. Images are described with structured regions. */
  Region::RegionEnum
  GetRegionType() const itkRegionOverrideMacro
  {
    return Region::RegionEnum::ITK_STRUCTURED_REGION;
  }

  /** Print the region. */
  void
  Print(std::ostream & os, Indent indent = 0) const itkRegionOverrideMacro;

  /** Constructor. ImageRegion is a lightweight object that is not reference
   * counted, so the constructor is public. Its two data members are filled
   * with zeros (using C++11 default member initializers). */
  ImageRegion() noexcept = default;

  /** Destructor. ImageRegion is a lightweight object that is not reference
   * counted, so the destructor is public. */
  ~ImageRegion() itkRegionOverrideMacro = default;

  /** Copy constructor. ImageRegion is a lightweight object that is not
   * reference counted, so the copy constructor is public. */
  ImageRegion(const Self &) noexcept = default;

  /** Constructor that takes an index and size. ImageRegion is a lightweight
   * object that is not reference counted, so this constructor is public.
   * \note This constructor supports class template argument deduction (CTAD). */
  ImageRegion(const IndexType & index, const SizeType & size) noexcept
    : // Note: Use parentheses instead of curly braces to initialize data members,
      // to avoid AppleClang 6.0.0.6000056 compile errors, "no viable conversion..."
    m_Index(index)
    , m_Size(size)
  {}

  /** Constructor that takes a size and assumes an index of zeros. ImageRegion
   * is lightweight object that is not reference counted so this constructor
   * is public.
   * \note This constructor supports class template argument deduction (CTAD). */
  ImageRegion(const SizeType & size) noexcept
    : m_Size(size)
  {
    // Note: m_Index is initialized by its C++11 default member initializer.
  }

  /** operator=. ImageRegion is a lightweight object that is not reference
   * counted, so operator= is public. */
  Self &
  operator=(const Self &) noexcept = default;

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
  operator==(const Self & region) const noexcept
  {
    return (m_Index == region.m_Index) && (m_Size == region.m_Size);
  }

  ITK_UNEQUAL_OPERATOR_MEMBER_FUNCTION(Self);

  /** Test if an index is inside */
  bool
  IsInside(const IndexType & index) const
  {
    for (unsigned int i = 0; i < ImageDimension; ++i)
    {
      if (index[i] < m_Index[i] || index[i] >= m_Index[i] + static_cast<IndexValueType>(m_Size[i]))
      {
        return false;
      }
    }
    return true;
  }

  /** Test if a continuous index is inside the region.
   * We take into account the fact that each voxel has its
   * center at the integer coordinate and extends half way
   * to the next integer coordinate, inclusive on all sides. */
  template <typename TCoordRepType>
  bool
  IsInside(const ContinuousIndex<TCoordRepType, VImageDimension> & index) const
  {
    constexpr TCoordRepType half = 0.5;
    for (unsigned int i = 0; i < ImageDimension; ++i)
    {
      // Use negation of tests so that index[i]==NaN leads to returning false.
      if (!(index[i] >= m_Index[i] - half && index[i] <= (m_Index[i] + static_cast<IndexValueType>(m_Size[i])) - half))
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
  IsInside(const Self & otherRegion) const
  {
    const auto & otherIndex = otherRegion.m_Index;
    const auto & otherSize = otherRegion.m_Size;

    for (unsigned int i = 0; i < ImageDimension; ++i)
    {
      if (otherIndex[i] < m_Index[i] || otherSize[i] == 0 ||
          otherIndex[i] + static_cast<IndexValueType>(otherSize[i]) >
            m_Index[i] + static_cast<IndexValueType>(m_Size[i]))
      {
        return false;
      }
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

  /** Supports tuple-like access: `get<0>()` returns a reference to the index and `get<1>()` returns a reference to the
   * size of the region. */
  template <size_t VTupleIndex>
  [[nodiscard]] auto &
  get()
  {
    if constexpr (VTupleIndex == 0)
    {
      return m_Index;
    }
    else
    {
      static_assert(VTupleIndex == 1);
      return m_Size;
    }
  }

  /** Supports tuple-like access. Const overload. */
  template <size_t VTupleIndex>
  [[nodiscard]] const auto &
  get() const
  {
    if constexpr (VTupleIndex == 0)
    {
      return m_Index;
    }
    else
    {
      static_assert(VTupleIndex == 1);
      return m_Size;
    }
  }

protected:
  /** Methods invoked by Print() to print information about the object
   * including superclasses. Typically not called by the user (use Print()
   * instead) but used in the hierarchical print process to combine the
   * output of several classes.  */
  void
  PrintSelf(std::ostream & os, Indent indent) const itkRegionOverrideMacro;

private:
  IndexType m_Index = { { 0 } };
  SizeType  m_Size = { { 0 } };

  /** Friends of ImageRegion */
  friend class ImageBase<VImageDimension>;
};


// Deduction guide to avoid compiler warnings (-wctad-maybe-unsupported) when using class template argument deduction.
template <unsigned int VImageDimension>
ImageRegion(const Index<VImageDimension> &, const Size<VImageDimension> &)->ImageRegion<VImageDimension>;


template <unsigned int VImageDimension>
std::ostream &
operator<<(std::ostream & os, const ImageRegion<VImageDimension> & region);
} // end namespace itk


namespace std
{
#if defined(__clang__) && defined(__apple_build_version__) && (__clang_major__ <= 10)
#  pragma clang diagnostic push
// Old AppleClang 10.0.0 (Xcode 10.1, newest on macOS 10.13) produced some unimportant warnings, like:
// "warning: 'tuple_size' defined as a struct template here but previously declared as a class template"
#  pragma clang diagnostic ignored "-Wmismatched-tags"
#endif

// NOLINTBEGIN(cert-dcl58-cpp)
// Locally suppressed the following warning from Clang-Tidy (LLVM 17.0.1), as it appears undeserved.
// > warning: modification of 'std' namespace can result in undefined behavior [cert-dcl58-cpp]

/** `std::tuple_size` specialization, needed for ImageRegion to support C++ structured binding.
 *
 * Example, using structured binding to retrieve the index and size of a region:
   \code
    auto [index, size] = image.GetRequestedRegion();
   \endcode
 */
template <unsigned int VImageDimension>
struct tuple_size<itk::ImageRegion<VImageDimension>> : integral_constant<size_t, 2>
{};

/** `std::tuple_element` specialization, needed for ImageRegion to support C++ structured binding. */
template <size_t VTupleIndex, unsigned int VImageDimension>
struct tuple_element<VTupleIndex, itk::ImageRegion<VImageDimension>>
  : conditional<VTupleIndex == 0, itk::Index<VImageDimension>, itk::Size<VImageDimension>>
{
  static_assert(VTupleIndex < tuple_size_v<itk::ImageRegion<VImageDimension>>);
};

// NOLINTEND(cert-dcl58-cpp)

#if defined(__clang__) && defined(__apple_build_version__) && (__clang_major__ <= 10)
#  pragma clang diagnostic pop
#endif
} // namespace std

#undef itkRegionOverrideMacro

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkImageRegion.hxx"
#endif

#endif
