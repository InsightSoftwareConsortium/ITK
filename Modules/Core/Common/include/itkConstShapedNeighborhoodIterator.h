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
#ifndef itkConstShapedNeighborhoodIterator_h
#define itkConstShapedNeighborhoodIterator_h

#include <vector>
#include <list>
#include "itkNeighborhoodIterator.h"

namespace itk
{
/** \class ConstShapedNeighborhoodIterator
 *
 * \brief Const version of ShapedNeighborhoodIterator, defining iteration
 * of a local N-dimensional neighborhood of pixels across an itk::Image.
 *
 * ConstShapedNeighborhoodIterator implements the read-only methods of
 * ShapedNeighborhoodIterator.  A "shaped" neighborhood iterator is one that
 * allows you to specify which neighbor elements are active and which are
 * inactive.  See ShapedNeighborhoodIterator for more information.
 *
 * \sa Neighborhood
 *
 * \par MORE INFORMATION
 * For a complete description of the ITK Image Iterators and their API, please
 * see the Iterators chapter in the ITK Software Guide.  The ITK Software Guide
 * is available in print and as a free .pdf download from https://www.itk.org.
 *
 * \ingroup ImageIterators
 *
 * \sa ImageConstIterator \sa ConditionalConstIterator
 * \sa ConstNeighborhoodIterator \sa ConstShapedNeighborhoodIterator
 * \sa ConstSliceIterator  \sa CorrespondenceDataStructureIterator
 * \sa FloodFilledFunctionConditionalConstIterator
 * \sa FloodFilledImageFunctionConditionalConstIterator
 * \sa FloodFilledImageFunctionConditionalIterator
 * \sa FloodFilledSpatialFunctionConditionalConstIterator
 * \sa FloodFilledSpatialFunctionConditionalIterator
 * \sa ImageConstIterator \sa ImageConstIteratorWithIndex
 * \sa ImageIterator \sa ImageIteratorWithIndex
 * \sa ImageLinearConstIteratorWithIndex  \sa ImageLinearIteratorWithIndex
 * \sa ImageRandomConstIteratorWithIndex  \sa ImageRandomIteratorWithIndex
 * \sa ImageRegionConstIterator \sa ImageRegionConstIteratorWithIndex
 * \sa ImageRegionExclusionConstIteratorWithIndex
 * \sa ImageRegionExclusionIteratorWithIndex
 * \sa ImageRegionIterator  \sa ImageRegionIteratorWithIndex
 * \sa ImageRegionReverseConstIterator  \sa ImageRegionReverseIterator
 * \sa ImageReverseConstIterator  \sa ImageReverseIterator
 * \sa ImageSliceConstIteratorWithIndex  \sa ImageSliceIteratorWithIndex
 * \sa NeighborhoodIterator \sa PathConstIterator  \sa PathIterator
 * \sa ShapedNeighborhoodIterator  \sa SliceIterator
 * \sa ImageConstIteratorWithIndex
 * \sa ShapedImageNeighborhoodRange
 * \ingroup ITKCommon
 */
template <typename TImage, typename TBoundaryCondition = ZeroFluxNeumannBoundaryCondition<TImage>>
class ITK_TEMPLATE_EXPORT ConstShapedNeighborhoodIterator : private NeighborhoodIterator<TImage, TBoundaryCondition>
{
public:
  /** Extract image type information. */
  using InternalPixelType = typename TImage::InternalPixelType;
  using PixelType = typename TImage::PixelType;

  /** Save the image dimension. */
  static constexpr unsigned int Dimension = TImage::ImageDimension;

  /** Standard class type aliases. */
  using Self = ConstShapedNeighborhoodIterator;
  using Superclass = NeighborhoodIterator<TImage, TBoundaryCondition>;

  /** Inherit type alias from superclass */
  using OffsetType = typename Superclass::OffsetType;
  using OffsetValueType = typename OffsetType::OffsetValueType;
  using RadiusType = typename Superclass::RadiusType;
  using SizeType = typename Superclass::SizeType;
  using SizeValueType = typename SizeType::SizeValueType;

  /** Typedef support for common objects */
  using ImageType = TImage;
  using RegionType = typename TImage::RegionType;
  using IndexType = Index<Self::Dimension>;
  using IndexValueType = typename IndexType::IndexValueType;
  using NeighborhoodType = Neighborhood<PixelType, Self::Dimension>;

  using NeighborIndexType = typename NeighborhoodType::NeighborIndexType;

  /** An stl storage container type that can be sorted.  The type used for
   *  the list of active offsets in the neighborhood. */
  using IndexListType = std::list<NeighborIndexType>;

  using IndexListIterator = typename IndexListType::iterator;
  using IndexListConstIterator = typename IndexListType::const_iterator;

  /** Typedef for boundary condition type. */
  using BoundaryConditionType = TBoundaryCondition;

  /** Typedef for generic boundary condition pointer */
  using ImageBoundaryConditionPointerType = ImageBoundaryCondition<ImageType> *;

  /** Const Interator */
  struct ConstIterator
  {
    ConstIterator() { m_NeighborhoodIterator = nullptr; }
    ConstIterator(Self * s)
    {
      m_NeighborhoodIterator = s;
      this->GoToBegin();
    }

    ITK_ITERATOR_VIRTUAL ~ConstIterator() = default;

    ConstIterator &
    operator=(const ConstIterator & o)
    {
      m_NeighborhoodIterator = o.m_NeighborhoodIterator;
      m_ListIterator = o.m_ListIterator;
      return *this;
    }

    ConstIterator(const ConstIterator & o)
    {
      m_NeighborhoodIterator = o.m_NeighborhoodIterator;
      m_ListIterator = o.m_ListIterator;
    }

    void
    operator++(int)
    {
      m_ListIterator++;
    }

    void
    operator--(int)
    {
      m_ListIterator--;
    }

    const ConstIterator &
    operator++()
    {
      m_ListIterator++;
      return *this;
    }

    const ConstIterator &
    operator--()
    {
      m_ListIterator--;
      return *this;
    }

    bool
    operator!=(const ConstIterator & o) const
    {
      return m_ListIterator != o.m_ListIterator;
    }
    bool
    operator==(const ConstIterator & o) const
    {
      return m_ListIterator == o.m_ListIterator;
    }

    bool
    IsAtEnd() const
    {
      if (m_ListIterator == m_NeighborhoodIterator->GetActiveIndexList().end())
      {
        return true;
      }
      else
      {
        return false;
      }
    }

    void
    GoToBegin()
    {
      m_ListIterator = m_NeighborhoodIterator->GetActiveIndexList().begin();
    }

    void
    GoToEnd()
    {
      m_ListIterator = m_NeighborhoodIterator->GetActiveIndexList().end();
    }

    PixelType
    Get() const
    {
      return m_NeighborhoodIterator->GetPixel(*m_ListIterator);
    }

    OffsetType
    GetNeighborhoodOffset() const
    {
      return m_NeighborhoodIterator->GetOffset(*m_ListIterator);
    }

    typename IndexListType::value_type
    GetNeighborhoodIndex() const
    {
      return *m_ListIterator;
    }

  protected:
    friend Self;

    ConstIterator(const Self * s, const typename IndexListType::const_iterator & li)
      : m_NeighborhoodIterator(const_cast<Self *>(s))
      , m_ListIterator(li)
    {}

    Self * m_NeighborhoodIterator;

    typename IndexListType::const_iterator m_ListIterator;

    void
    ProtectedSet(const PixelType & v) const
    {
      m_NeighborhoodIterator->SetPixel(*m_ListIterator, v);
    }
  };

  /** Returns a const iterator for the neighborhood which points to the first
   * pixel in the neighborhood. */
  ConstIterator
  Begin() const
  {
    return ConstIterator(this, this->m_ActiveIndexList.begin());
  }

  /** Returns a const iterator for the neighborhood which points to the last
   * pixel in the neighborhood. */
  ConstIterator
  End() const
  {
    return ConstIterator(this, this->m_ActiveIndexList.end());
  }

  /** Default constructor */
  ConstShapedNeighborhoodIterator() = default;

  /** Virtual destructor */
  ~ConstShapedNeighborhoodIterator() override = default;

  /** Constructor which establishes the region size, neighborhood, and image
   * over which to walk. */
  ConstShapedNeighborhoodIterator(const SizeType & radius, const ImageType * ptr, const RegionType & region)
    : Superclass(radius, const_cast<ImageType *>(ptr), region)
  {}

  /** Copy constructor */
  ConstShapedNeighborhoodIterator(const ConstShapedNeighborhoodIterator &) = delete;

  // Expose the following methods from the superclass.  This is a
  // restricted subset of the methods available for
  // ConstNeighborhoodIterator.
  using Superclass::GetImagePointer;
  using Superclass::GetRadius;
  using Superclass::GetIndex;
  using Superclass::GetNeighborhoodIndex;
  using Superclass::GetCenterNeighborhoodIndex;
  using Superclass::GetRegion;
  using Superclass::GetBeginIndex;
  using Superclass::GoToBegin;
  using Superclass::GoToEnd;
  using Superclass::IsAtBegin;
  using Superclass::IsAtEnd;
  using Superclass::GetOffset;
  using Superclass::operator==;
  using Superclass::operator!=;
  using Superclass::operator<;
  using Superclass::operator>;
  using Superclass::operator>=;
  using Superclass::operator<=;
  using Superclass::operator[];
  using Superclass::GetElement;
  using Superclass::SetLocation;
  using Superclass::GetCenterPointer;
  using Superclass::GetCenterPixel;
  using Superclass::OverrideBoundaryCondition;
  using Superclass::ResetBoundaryCondition;
  using Superclass::GetBoundaryCondition;
  using Superclass::SetBoundaryCondition;
  using Superclass::GetNeedToUseBoundaryCondition;
  using Superclass::SetNeedToUseBoundaryCondition;
  using Superclass::NeedToUseBoundaryConditionOn;
  using Superclass::NeedToUseBoundaryConditionOff;
  using Superclass::Print;
  using Superclass::operator-;
  using Superclass::GetPixel;
  using Superclass::SetRegion;

  /** Assignment operator */
  Self &
  operator=(const Self & orig)
  {
    if (this != &orig)
    {
      Superclass::operator=(orig);
      m_ActiveIndexList = orig.m_ActiveIndexList;
      m_CenterIsActive = orig.m_CenterIsActive;
    }
    return *this;
  }

  /** Standard itk print method */
  void
  PrintSelf(std::ostream &, Indent) const override;

  /** Add/Remove a neighborhood offset (from the center of the neighborhood)
   *  to/from the active list.  Active list offsets are the only locations
   *  updated and accessible through the iterator.  */
  ITK_ITERATOR_VIRTUAL void
  ActivateOffset(const OffsetType & off) ITK_ITERATOR_FINAL
  {
    this->ActivateIndex(Superclass::GetNeighborhoodIndex(off));
  }
  ITK_ITERATOR_VIRTUAL void
  DeactivateOffset(const OffsetType & off) ITK_ITERATOR_FINAL
  {
    this->DeactivateIndex(Superclass::GetNeighborhoodIndex(off));
  }

  /** Activates a whole range of offsets, for example, an std::vector<OffsetType>,
   * which could be from GenerateImageNeighborhoodOffsets(shape). */
  template <typename TOffsets>
  void
  ActivateOffsets(const TOffsets & offsets)
  {
    for (const auto & offset : offsets)
    {
      this->ActivateOffset(offset);
    }
  }

  /** Removes all active pixels from this neighborhood. */
  ITK_ITERATOR_VIRTUAL void
  ClearActiveList() ITK_ITERATOR_FINAL
  {
    m_ActiveIndexList.clear();
    m_CenterIsActive = false;
  }

  /** Returns the list of active indices in the neighborhood */
  const IndexListType &
  GetActiveIndexList() const
  {
    return m_ActiveIndexList;
  }

  /** Returns the size of the list of active neighborhood indices. */
  typename IndexListType::size_type
  GetActiveIndexListSize() const
  {
    return m_ActiveIndexList.size();
  }

  /** Add non-zero neighborhood offsets to the active list. The
   * radius of the neighborhood must match the radius of the shaped
   * iterator */
  void
  CreateActiveListFromNeighborhood(const NeighborhoodType &);

  /** Reimplements the operator++ method so that only active pixel locations
   * are updated. */
  Self &
  operator++();

  /** Reimplements the operator-- method so that only active pixel locations
   * are updated. */
  Self &
  operator--();

  /** Addition of an itk::Offset.  Note that this method does not do any bounds
   * checking.  Adding an offset that moves the iterator out of its assigned
   * region will produce undefined results. */
  Self &
  operator+=(const OffsetType &);

  /** Subtraction of an itk::Offset. Note that this method does not do
   *  any bounds checking.  Subtracting an offset that moves the iterator
   *  out of its assigned region will produce undefined results. */
  Self &
  operator-=(const OffsetType &);

protected:
  using Superclass::SetPixel;
  using Superclass::SetCenterPixel;

  friend struct ConstIterator;

  /** Class is protected here so that it is not publicly accessible, but can be
   * accessed by subclasses.. */
  //  Superclass::SetPixel;
  //  Superclass::SetCenterPixel;

  /** Add/Remove a neighborhood index to/from the active.  Locations in the
      active list are the only accessible elements in the neighborhood. The
      argument is an index location calculated as an offset into a linear
      array which represents the image region defined by the radius of this
      iterator, with the smallest dimension as the fastest increasing index. */
  ITK_ITERATOR_VIRTUAL void ActivateIndex(NeighborIndexType) ITK_ITERATOR_FINAL;

  ITK_ITERATOR_VIRTUAL void DeactivateIndex(NeighborIndexType) ITK_ITERATOR_FINAL;


  bool          m_CenterIsActive{ false };
  IndexListType m_ActiveIndexList;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkConstShapedNeighborhoodIterator.hxx"
#endif

#endif
