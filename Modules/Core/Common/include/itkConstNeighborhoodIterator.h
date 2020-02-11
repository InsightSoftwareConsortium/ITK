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
#ifndef itkConstNeighborhoodIterator_h
#define itkConstNeighborhoodIterator_h

#include <vector>
#include <cstring>
#include <iostream>
#include "itkImage.h"
#include "itkNeighborhood.h"
#include "itkMacro.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"

namespace itk
{
/** \class ConstNeighborhoodIterator
 *
 * \brief Const version of NeighborhoodIterator, defining iteration of a local
 * N-dimensional neighborhood of pixels across an itk::Image.
 *
 * ConstNeighborhoodIterator implements the read-only methods of
 * NeighborhoodIterator.  It serves as a base class from which other iterators
 * are derived. See NeighborhoodIterator for more complete information.
 *
 * \ingroup ImageIterators
 *
 * \sa Neighborhood \sa ImageIterator \sa NeighborhoodIterator
 * \ingroup ITKCommon
 *
 * \sphinx
 * \sphinxexample{Core/Common/IterateWithNeighborhoodWithoutAccess,Iterate Region In Image With Neighborhood Without
 * Write Access} \endsphinx
 */
template <typename TImage, typename TBoundaryCondition = ZeroFluxNeumannBoundaryCondition<TImage>>
class ITK_TEMPLATE_EXPORT ConstNeighborhoodIterator
  : public Neighborhood<typename TImage::InternalPixelType *, TImage::ImageDimension>
{
public:
  /** Extract image type information. */
  using InternalPixelType = typename TImage::InternalPixelType;
  using PixelType = typename TImage::PixelType;

  /** Type used to refer to space dimensions */
  using DimensionValueType = unsigned int;

  /** Save the image dimension. */
  static constexpr DimensionValueType Dimension = TImage::ImageDimension;

  /** Standard class type aliases. */
  using Self = ConstNeighborhoodIterator;
  using Superclass = Neighborhood<InternalPixelType *, Self::Dimension>;

  /** Inherit type alias from superclass */
  using OffsetType = typename Superclass::OffsetType;
  using RadiusType = typename Superclass::RadiusType;
  using SizeType = typename Superclass::SizeType;
  using Iterator = typename Superclass::Iterator;
  using ConstIterator = typename Superclass::ConstIterator;

  /** Typedef support for common objects */
  using ImageType = TImage;
  using RegionType = typename TImage::RegionType;
  using IndexType = Index<Self::Dimension>;
  using NeighborhoodType = Neighborhood<PixelType, Self::Dimension>;


  /** Typedef for boundary condition type. */
  using BoundaryConditionType = TBoundaryCondition;

  using OutputImageType = typename BoundaryConditionType::OutputImageType;

  /** Type used to refer to the elements in the list of neighbor pixels. */
  using NeighborIndexType = typename NeighborhoodType::NeighborIndexType;

  /** Typedef for the functor used to access neighborhoods of pixel pointers.
   * This is obtained as a trait from the image and is different for Image
   * and VectorImage. */
  using NeighborhoodAccessorFunctorType = typename ImageType::NeighborhoodAccessorFunctorType;

  /** Typedef for generic boundary condition pointer */
  using ImageBoundaryConditionPointerType = ImageBoundaryCondition<ImageType, OutputImageType> *;
  using ImageBoundaryConditionConstPointerType = const ImageBoundaryCondition<ImageType, OutputImageType> *;

  /** Default constructor */
  ConstNeighborhoodIterator();

  /** Virtual destructor */
  ~ConstNeighborhoodIterator() override = default;

  /** Copy constructor */
  ConstNeighborhoodIterator(const ConstNeighborhoodIterator &);

  /** Constructor which establishes the region size, neighborhood, and image
   * over which to walk. */
  ConstNeighborhoodIterator(const SizeType & radius, const ImageType * ptr, const RegionType & region)
  {
    this->Initialize(radius, ptr, region);
    for (DimensionValueType i = 0; i < Dimension; i++)
    {
      m_InBounds[i] = false;
    }
    this->ResetBoundaryCondition();
    m_NeighborhoodAccessorFunctor = ptr->GetNeighborhoodAccessor();
    m_NeighborhoodAccessorFunctor.SetBegin(ptr->GetBufferPointer());
  }

  /** Assignment operator */
  Self &
  operator=(const Self & orig);

  /** Standard itk print method */
  void
  PrintSelf(std::ostream &, Indent) const override;

  /** Computes the internal, N-d offset of a pixel array position n from
   * (0,0, ..., 0) in the "upper-left" corner of the neighborhood. */
  OffsetType
  ComputeInternalIndex(const NeighborIndexType n) const;

  /** Returns the array of upper loop bounds used during iteration. */
  IndexType
  GetBound() const
  {
    return m_Bound;
  }

  /** Returns the loop bound used to define the edge of a single
   * dimension in the itk::Image region. */
  IndexValueType
  GetBound(NeighborIndexType n) const
  {
    return m_Bound[n];
  }

  /** Returns the pointer to the center pixel of the neighborhood. */
  const InternalPixelType *
  GetCenterPointer() const
  {
    return (this->operator[]((this->Size()) >> 1));
  }

  /** Returns the pixel referenced at the center of the
   *  ConstNeighborhoodIterator. */
  PixelType
  GetCenterPixel() const
  {
    return m_NeighborhoodAccessorFunctor.Get(this->GetCenterPointer());
  }

  /** Returns a smartpointer to the image on which this iterator operates. */
  const ImageType *
  GetImagePointer() const
  {
    return m_ConstImage;
  }

  /** Returns the N-dimensional index of the iterator's position in
   * the image. */
  ITK_ITERATOR_VIRTUAL IndexType
                       GetIndex() const ITK_ITERATOR_FINAL
  {
    return m_Loop;
  }

  inline IndexType
  GetFastIndexPlusOffset(const OffsetType & o) const
  {
    return m_Loop + o;
  }

  /** Function that "dereferences" a ConstNeighborhoodIterator,
   * returning a Neighborhood of pixel values. */
  ITK_ITERATOR_VIRTUAL NeighborhoodType
                       GetNeighborhood() const ITK_ITERATOR_FINAL;

  /** Returns the pixel value located at a linear array location i. */
  ITK_ITERATOR_VIRTUAL PixelType
                       GetPixel(const NeighborIndexType i) const ITK_ITERATOR_FINAL
  {
    if (!m_NeedToUseBoundaryCondition || this->InBounds())
    {
      return (m_NeighborhoodAccessorFunctor.Get(this->operator[](i)));
    }

    OffsetType internalIndex;
    OffsetType offset;

    return this->IndexInBounds(i, internalIndex, offset)
             ? m_NeighborhoodAccessorFunctor.Get(this->operator[](i))
             : m_NeighborhoodAccessorFunctor.BoundaryCondition(internalIndex, offset, this, m_BoundaryCondition);
  }

  /** Return the pixel value located at a linear array location i.
   * Sets "IsInBounds" to true if the location is inside the
   * image and the pixel value returned is an actual pixel in the
   * image. Sets "IsInBounds" to false if the location is outside the
   * image and the pixel value returned is a boundary condition. */
  ITK_ITERATOR_VIRTUAL PixelType
                       GetPixel(NeighborIndexType i, bool & IsInBounds) const ITK_ITERATOR_FINAL;

  /** Returns the pixel value located at the itk::Offset o from the center of
      the neighborhood. */
  ITK_ITERATOR_VIRTUAL PixelType
                       GetPixel(const OffsetType & o) const ITK_ITERATOR_FINAL
  {
    bool inbounds;

    return (this->GetPixel(this->GetNeighborhoodIndex(o), inbounds));
  }

  /** Returns the pixel value located at the itk::Offset o from the center of
   * the neighborhood. Sets "IsInBounds" to true if the offset is inside the
   * image and the pixel value returned is an actual pixel in the
   * image. Sets "IsInBounds" to false if the offset is outside the
   * image and the pixel value returned is a boundary condition. */
  ITK_ITERATOR_VIRTUAL PixelType
                       GetPixel(const OffsetType & o, bool & IsInBounds) const ITK_ITERATOR_FINAL
  {
    return (this->GetPixel(this->GetNeighborhoodIndex(o), IsInBounds));
  }

  /** Returns the pixel value located i pixels distant from the neighborhood
   *  center in the positive specified "axis" direction. No bounds checking
   *  is done on the size of the neighborhood. */
  ITK_ITERATOR_VIRTUAL PixelType
                       GetNext(const unsigned axis, NeighborIndexType i) const ITK_ITERATOR_FINAL
  {
    return (this->GetPixel(this->GetCenterNeighborhoodIndex() + (i * this->GetStride(axis))));
  }

  /** Returns the pixel value located one pixel distant from the neighborhood
   *  center in the specified positive axis direction. No bounds checking is
   *  done on the size of the neighborhood. */
  ITK_ITERATOR_VIRTUAL PixelType
                       GetNext(const unsigned axis) const ITK_ITERATOR_FINAL
  {
    return (this->GetPixel(this->GetCenterNeighborhoodIndex() + this->GetStride(axis)));
  }

  /** Returns the pixel value located i pixels distant from the neighborhood
   *  center in the negative specified "axis" direction. No bounds checking
   *  is done on the size of the neighborhood. */
  ITK_ITERATOR_VIRTUAL PixelType
                       GetPrevious(const unsigned axis, NeighborIndexType i) const ITK_ITERATOR_FINAL
  {
    return (this->GetPixel(this->GetCenterNeighborhoodIndex() - (i * this->GetStride(axis))));
  }

  /** Returns the pixel value located one pixel distant from the neighborhood
   *  center in the specified negative axis direction. No bounds checking is
   *  done on the size of the neighborhood. */
  ITK_ITERATOR_VIRTUAL PixelType
                       GetPrevious(const unsigned axis) const ITK_ITERATOR_FINAL
  {
    return (this->GetPixel(this->GetCenterNeighborhoodIndex() - this->GetStride(axis)));
  }

  /** Returns the image index for neighbor pixel at offset o from the center of
      the neighborhood. */
  ITK_ITERATOR_VIRTUAL IndexType
                       GetIndex(const OffsetType & o) const ITK_ITERATOR_FINAL
  {
    return (this->GetIndex() + o);
  }

  /** Returns the image index for neighbor pixel at index i in the
      neighborhood. */
  ITK_ITERATOR_VIRTUAL IndexType
                       GetIndex(NeighborIndexType i) const ITK_ITERATOR_FINAL
  {
    return (this->GetIndex() + this->GetOffset(i));
  }

  /**  Returns the region of iteration. */
  RegionType
  GetRegion() const
  {
    return m_Region;
  }

  /** Returns the N-dimensional starting index of the iterator's position on
   * the image. */
  IndexType
  GetBeginIndex() const
  {
    return m_BeginIndex;
  }

  /** Returns a bounding box for the region spanned by this neighborhood
      represented by an itk::ImageRegion */
  RegionType
  GetBoundingBoxAsImageRegion() const;

  /** Returns the offsets used to wrap across dimensional boundaries. */
  OffsetType
  GetWrapOffset() const
  {
    return m_WrapOffset;
  }

  /** Returns the internal offset associated with wrapping around a single
   * dimension's region boundary in the itk::Image.  An offset for each
   * dimension is necessary to shift pointers when wrapping around region
   * edges because region memory is not necessarily contiguous within the
   * buffer. */
  OffsetValueType
  GetWrapOffset(NeighborIndexType n) const
  {
    return m_WrapOffset[n];
  }

  /** Method for rewinding the iterator to its beginning pixel. */
  ITK_ITERATOR_VIRTUAL void
  GoToBegin() ITK_ITERATOR_FINAL;

  /** Method for sending the iterator to one past the last pixel in its
   * region. */
  ITK_ITERATOR_VIRTUAL void
  GoToEnd() ITK_ITERATOR_FINAL;

  /** Initializes the iterator to walk a particular image and a particular
   * region of that image. */
  ITK_ITERATOR_VIRTUAL void
  Initialize(const SizeType & radius, const ImageType * ptr, const RegionType & region) ITK_ITERATOR_FINAL;

  /** Method for determining whether the iterator is at the
   * beginning of its iteration region. */
  ITK_ITERATOR_VIRTUAL bool
  IsAtBegin() const ITK_ITERATOR_FINAL
  {
    return (this->GetCenterPointer() == m_Begin);
  }

  /** Method for determining whether the iterator has reached the
   * end of its iteration region. */
  ITK_ITERATOR_VIRTUAL bool
  IsAtEnd() const ITK_ITERATOR_FINAL
  {
    if (this->GetCenterPointer() > m_End)
    {
      ExceptionObject    e(__FILE__, __LINE__);
      std::ostringstream msg;
      msg << "In method IsAtEnd, CenterPointer = " << this->GetCenterPointer() << " is greater than End = " << m_End
          << std::endl
          << "  " << *this;
      e.SetDescription(msg.str().c_str());
      throw e;
    }
    return (this->GetCenterPointer() == m_End);
  }

  /** Increments the pointers in the ConstNeighborhoodIterator,
   * wraps across boundaries automatically, accounting for
   * the disparity in the buffer size and the region size of the
   * image. */
  Self &
  operator++();

  /** Decrements the pointers in the ConstNeighborhoodIterator,
   * wraps across boundaries automatically, accounting for
   * the disparity in the buffer size and the region size of the
   * image. */
  Self &
  operator--();

  /** Returns a boolean == comparison of the memory addresses of the center
   * elements of two ConstNeighborhoodIterators of like pixel type and
   * dimensionality.  The radii of the iterators are ignored. */
  bool
  operator==(const Self & it) const
  {
    return it.GetCenterPointer() == this->GetCenterPointer();
  }

  /** Returns a boolean != comparison of the memory addresses of the center
   * elements of two ConstNeighborhoodIterators of like pixel type and
   * dimensionality.  The radii of the iterators are ignored. */
  bool
  operator!=(const Self & it) const
  {
    return it.GetCenterPointer() != this->GetCenterPointer();
  }

  /** Returns a boolean < comparison of the memory addresses of the center
   * elements of two ConstNeighborhoodIterators of like pixel type and
   * dimensionality.  The radii of the iterators are ignored. */
  bool
  operator<(const Self & it) const
  {
    return this->GetCenterPointer() < it.GetCenterPointer();
  }

  /** Returns a boolean < comparison of the memory addresses of the center
   * elements of two ConstNeighborhoodIterators of like pixel type and
   * dimensionality.  The radii of the iterators are ignored. */
  bool
  operator<=(const Self & it) const
  {
    return this->GetCenterPointer() <= it.GetCenterPointer();
  }

  /** Returns a boolean > comparison of the memory addresses of the center
   * elements of two ConstNeighborhoodIterators of like pixel type and
   * dimensionality.  The radii of the iterators are ignored. */
  bool
  operator>(const Self & it) const
  {
    return this->GetCenterPointer() > it.GetCenterPointer();
  }

  /** Returns a boolean >= comparison of the memory addresses of the center
   * elements of two ConstNeighborhoodIterators of like pixel type and
   * dimensionality.  The radii of the iterators are ignored. */
  bool
  operator>=(const Self & it) const
  {
    return this->GetCenterPointer() >= it.GetCenterPointer();
  }

  /** This method positions the iterator at an indexed location in the
   * image. SetLocation should _NOT_ be used to update the position of the
   * iterator during iteration, only for initializing it to a position
   * prior to iteration.  This method is not optimized for speed. */
  void
  SetLocation(const IndexType & position)
  {
    this->SetLoop(position);
    this->SetPixelPointers(position);
  }

  /** Addition of an itk::Offset.  Note that this method does not do any bounds
   * checking.  Adding an offset that moves the iterator out of its assigned
   * region will produce undefined results. */
  Self &
  operator+=(const OffsetType &);

  /** Subtraction of an itk::Offset. Note that this method does not do any
   *  bounds checking.  Subtracting an offset that moves the iterator out
   * of its assigned region will produce undefined results. */
  Self &
  operator-=(const OffsetType &);

  /** Distance between two iterators */
  OffsetType
  operator-(const Self & b)
  {
    return m_Loop - b.m_Loop;
  }

  /** Returns false if the iterator overlaps region boundaries, true
   * otherwise.  Also updates an internal boolean array indicating
   * which of the iterator's faces are out of bounds. */
  bool
  InBounds() const;

  /** Returns true if the neighborhood index is within region boundaries,
   * false otherwise.
   * If false, then internalIndex and offset are calculated. Otherwise their
   * values are left unchanged.
   * Also updates an internal boolean array indicating
   * which of the iterator's faces are out of bounds.
   * \param n - linear neighborhood index.
   * \param internalIndex - calculated for index \c n only when the neighborhood is not
   * completely within region boundaries.
   * \param offset - per-dimension offsets for index n to nearest boundary index,
   * calculate only when the neighborhood is not completely within region boundaries. */
  bool
  IndexInBounds(const NeighborIndexType n, OffsetType & internalIndex, OffsetType & offset) const;

  /** Returns true if the neighborhood index is within region boundaries,
   * false otherwise. */
  bool
  IndexInBounds(const NeighborIndexType n) const;

  /** Allows a user to override the internal boundary condition. Care should
   * be taken to ensure that the overriding boundary condition is a persistent
   * object during the time it is referenced.  The overriding condition
   * can be of a different type than the default type as long as it is
   * a subclass of ImageBoundaryCondition. */
  ITK_ITERATOR_VIRTUAL void
  OverrideBoundaryCondition(const ImageBoundaryConditionPointerType i) ITK_ITERATOR_FINAL
  {
    m_BoundaryCondition = i;
  }

  /** Resets the boundary condition to the internal, default conditions
   * specified by the template parameter. */
  ITK_ITERATOR_VIRTUAL void
  ResetBoundaryCondition() ITK_ITERATOR_FINAL
  {
    m_BoundaryCondition = &m_InternalBoundaryCondition;
  }

  /** Sets the internal, default boundary condition. */
  void
  SetBoundaryCondition(const TBoundaryCondition & c)
  {
    m_InternalBoundaryCondition = c;
  }

  /** */
  ImageBoundaryConditionPointerType
  GetBoundaryCondition() const
  {
    return m_BoundaryCondition;
  }

  /** */
  void
  NeedToUseBoundaryConditionOn()
  {
    this->SetNeedToUseBoundaryCondition(true);
  }

  void
  NeedToUseBoundaryConditionOff()
  {
    this->SetNeedToUseBoundaryCondition(false);
  }

  void
  SetNeedToUseBoundaryCondition(bool b)
  {
    m_NeedToUseBoundaryCondition = b;
  }

  bool
  GetNeedToUseBoundaryCondition() const
  {
    return m_NeedToUseBoundaryCondition;
  }

  /** Set the region to iterate over. */
  ITK_ITERATOR_VIRTUAL void
  SetRegion(const RegionType & region) ITK_ITERATOR_FINAL;

protected:
  /** Default method for setting the coordinate location of the iterator.
   * Loop indices correspond to the actual Image region index. */
  ITK_ITERATOR_VIRTUAL void
  SetLoop(const IndexType & p) ITK_ITERATOR_FINAL
  {
    m_Loop = p;
    m_IsInBoundsValid = false;
  }

  /** Method for setting internal loop boundaries.  This
   * method must be defined in each subclass because
   * each subclass may handle loop boundaries differently. */
  ITK_ITERATOR_VIRTUAL void
  SetBound(const SizeType &) ITK_ITERATOR_FINAL;

  /** Default method for setting the values of the internal pointers
   * to itk::Image memory buffer locations.  This method should
   * generally only be called when the iterator is initialized.
   * \sa SetLocation */
  ITK_ITERATOR_VIRTUAL void
  SetPixelPointers(const IndexType &) ITK_ITERATOR_FINAL;

  /** Default method for setting the index of the first pixel in the
   * iteration region. */
  ITK_ITERATOR_VIRTUAL void
  SetBeginIndex(const IndexType & start) ITK_ITERATOR_FINAL
  {
    m_BeginIndex = start;
  }

  /** Default method for setting the index of the first pixel in the
   * iteration region. */
  ITK_ITERATOR_VIRTUAL void
  SetEndIndex() ITK_ITERATOR_FINAL;

  /** The starting index for iteration within the itk::Image region
   * on which this ConstNeighborhoodIterator is defined. */
  IndexType m_BeginIndex;

  /** An array of upper looping boundaries used during iteration. */
  IndexType m_Bound;

  /** A pointer to the first pixel in the iteration region. */
  const InternalPixelType * m_Begin;

  /** The image on which iteration is defined. */
  typename ImageType::ConstWeakPointer m_ConstImage;

  /** A pointer to one past the last pixel in the iteration region. */
  const InternalPixelType * m_End;

  /** The end index for iteration within the itk::Image region
   * on which this ConstNeighborhoodIterator is defined. */
  IndexType m_EndIndex;

  /** Array of loop counters used during iteration. */
  IndexType m_Loop;

  /** The region over which iteration is defined. */
  RegionType m_Region;

  /** The internal array of offsets that provide support for regions of
   *  interest.
   *  An offset for each dimension is necessary to shift pointers when wrapping
   *  around region edges because region memory is not necessarily contiguous
   *  within the buffer. */
  OffsetType m_WrapOffset;

  /** Pointer to the actual boundary condition that will be used.
   * By default this points to m_BoundaryCondition, but
   * OverrideBoundaryCondition allows a user to point this variable an external
   * boundary condition.  */
  ImageBoundaryConditionPointerType m_BoundaryCondition;

  /** Denotes which of the iterators dimensional sides spill outside
   * region of interest boundaries. */
  mutable bool m_InBounds[Dimension];

  /** Denotes if iterator is entirely within bounds */
  mutable bool m_IsInBounds{ false };

  /** Is the m_InBounds and m_IsInBounds variables up to date? Set to
   * false whenever the iterator is repositioned.  Set to true within
   * InBounds(). */
  mutable bool m_IsInBoundsValid{ false };

  /** Lower threshold of in-bounds loop counter values. */
  IndexType m_InnerBoundsLow;

  /** Upper threshold of in-bounds loop counter values. */
  IndexType m_InnerBoundsHigh;

  /** Default boundary condition. */
  TBoundaryCondition m_InternalBoundaryCondition;

  /** Does the specified region need to worry about boundary conditions? */
  bool m_NeedToUseBoundaryCondition{ false };

  /** Functor type used to access neighborhoods of pixel pointers */
  NeighborhoodAccessorFunctorType m_NeighborhoodAccessorFunctor;
};

template <typename TImage>
inline ConstNeighborhoodIterator<TImage>
operator+(const ConstNeighborhoodIterator<TImage> &                      it,
          const typename ConstNeighborhoodIterator<TImage>::OffsetType & ind)
{
  ConstNeighborhoodIterator<TImage> ret(it);
  ret += ind;
  return ret;
}

template <typename TImage>
inline ConstNeighborhoodIterator<TImage>
operator+(const typename ConstNeighborhoodIterator<TImage>::OffsetType & ind,
          const ConstNeighborhoodIterator<TImage> &                      it)
{
  return (it + ind);
}

template <typename TImage>
inline ConstNeighborhoodIterator<TImage>
operator-(const ConstNeighborhoodIterator<TImage> &                      it,
          const typename ConstNeighborhoodIterator<TImage>::OffsetType & ind)
{
  ConstNeighborhoodIterator<TImage> ret(it);
  ret -= ind;
  return ret;
}
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkConstNeighborhoodIterator.hxx"
#endif

#endif
