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
#ifndef itkNeighborhoodIteratorBase_h
#define itkNeighborhoodIteratorBase_h

// -----------------------------------------------------------------------------
// NeighborhoodIteratorBase: unified templated base that replaces the pair of
// legacy classes ConstNeighborhoodIterator<TImage, TBC> and
// NeighborhoodIterator<TImage, TBC> via a compile-time `bool VIsConst`
// parameter. See the "NeighborhoodIterator N+1 Port" design plan at
// docs/superpowers/plans/2026-04-23-neighborhood-iterator-n1-port.md.
// -----------------------------------------------------------------------------

#include <cstring>
#include <iostream>
#include <list>
#include <sstream>
#include <type_traits>
#include <vector>

#include "itkImage.h"
#include "itkNeighborhood.h"
#include "itkMacro.h"
#include "itkWeakPointer.h"
#include "itkImageBoundaryCondition.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"

namespace itk
{

// =============================================================================
// 1. Unified neighborhood iterator (replaces ConstNeighborhoodIterator +
//    NeighborhoodIterator).
// =============================================================================
template <typename TImage,
          typename TBoundaryCondition = ZeroFluxNeumannBoundaryCondition<TImage>,
          bool VIsConst = false>
class ITK_TEMPLATE_EXPORT NeighborhoodIteratorBase
  : public Neighborhood<
      std::conditional_t<VIsConst, const typename TImage::InternalPixelType *, typename TImage::InternalPixelType *>,
      TImage::ImageDimension>
{
public:
  // --- compile-time constness plumbing ---------------------------------------
  static constexpr bool IsConst = VIsConst;

  using InternalPixelType = typename TImage::InternalPixelType;
  using PixelType = typename TImage::PixelType;

  // Pointer / reference types flip based on VIsConst. No const_cast anywhere.
  using ImagePointer = std::conditional_t<IsConst, const TImage *, TImage *>;

  using InternalPixelPointer = std::conditional_t<IsConst, const InternalPixelType *, InternalPixelType *>;

  using PixelReference = std::conditional_t<IsConst, const PixelType &, PixelType &>;

  // --- standard iterator type aliases ----------------------------------------
  using Self = NeighborhoodIteratorBase;
  using Superclass = Neighborhood<InternalPixelPointer, TImage::ImageDimension>;

  using typename Superclass::OffsetType;
  using typename Superclass::RadiusType;
  using typename Superclass::SizeType;
  using typename Superclass::Iterator;
  using typename Superclass::ConstIterator;

  static constexpr unsigned int Dimension = TImage::ImageDimension;
  using DimensionValueType = unsigned int;

  using ImageType = TImage;
  using RegionType = typename TImage::RegionType;
  using IndexType = Index<Dimension>;
  using NeighborhoodType = Neighborhood<PixelType, Dimension>;
  using NeighborIndexType = typename NeighborhoodType::NeighborIndexType;
  using BoundaryConditionType = TBoundaryCondition;
  using OutputImageType = typename BoundaryConditionType::OutputImageType;

  using NeighborhoodAccessorFunctorType = typename ImageType::NeighborhoodAccessorFunctorType;

  using ImageBoundaryConditionPointerType = ImageBoundaryCondition<ImageType, OutputImageType> *;
  using ImageBoundaryConditionConstPointerType = const ImageBoundaryCondition<ImageType, OutputImageType> *;

  // m_ConstImage is a ConstWeakPointer on VIsConst=true and a (non-const)
  // WeakPointer on VIsConst=false.
  using ImageWeakPointerType =
    std::conditional_t<IsConst, typename ImageType::ConstWeakPointer, WeakPointer<ImageType>>;

  // Friending cross-const instantiations lets the converting ctor access
  // private state of its non-const sibling.
  template <typename, typename, bool>
  friend class NeighborhoodIteratorBase;

  /** Default constructor. */
  NeighborhoodIteratorBase() = default;

  /** Destructor. */
  ~NeighborhoodIteratorBase() = default;

  /** Constructor which establishes the region size, neighborhood, and image
   * over which to walk. */
  NeighborhoodIteratorBase(const SizeType & radius, ImagePointer ptr, const RegionType & region)
  {
    this->Initialize(radius, ptr, region);
    for (DimensionValueType i = 0; i < Dimension; ++i)
    {
      m_InBounds[i] = false;
    }
    this->ResetBoundaryCondition();
    m_NeighborhoodAccessorFunctor = ptr->GetNeighborhoodAccessor();
    m_NeighborhoodAccessorFunctor.SetBegin(ptr->GetBufferPointer());
  }

  /** Copy constructor (same constness). */
  NeighborhoodIteratorBase(const NeighborhoodIteratorBase & orig)
    : Superclass(orig)
    , m_BeginIndex(orig.m_BeginIndex)
    , m_Bound(orig.m_Bound)
    , m_Begin(orig.m_Begin)
    , m_ConstImage(orig.m_ConstImage)
    , m_End(orig.m_End)
    , m_EndIndex(orig.m_EndIndex)
    , m_Loop(orig.m_Loop)
    , m_Region(orig.m_Region)
    , m_WrapOffset(orig.m_WrapOffset)
    , m_InternalBoundaryCondition(orig.m_InternalBoundaryCondition)
    , m_NeedToUseBoundaryCondition(orig.m_NeedToUseBoundaryCondition)
  {
    for (DimensionValueType i = 0; i < Dimension; ++i)
    {
      m_InBounds[i] = orig.m_InBounds[i];
    }
    m_IsInBoundsValid = orig.m_IsInBoundsValid;
    m_IsInBounds = orig.m_IsInBounds;

    m_InnerBoundsLow = orig.m_InnerBoundsLow;
    m_InnerBoundsHigh = orig.m_InnerBoundsHigh;

    // Re-bind the (raw) boundary-condition pointer: if the source was
    // pointing at its own internal default, the copy's pointer needs to
    // point at *our* internal default, not the source's.
    if (orig.m_BoundaryCondition ==
        static_cast<ImageBoundaryConditionConstPointerType>(&orig.m_InternalBoundaryCondition))
    {
      this->ResetBoundaryCondition();
    }
    else
    {
      m_BoundaryCondition = orig.m_BoundaryCondition;
    }
    m_NeighborhoodAccessorFunctor = orig.m_NeighborhoodAccessorFunctor;
  }

  /** Non-const -> const converting constructor. Enabled only when *this* is
   * the const instantiation being built from a non-const source. */
  template <bool VOtherIsConst, std::enable_if_t<VIsConst && !VOtherIsConst, int> = 0>
  NeighborhoodIteratorBase(const NeighborhoodIteratorBase<TImage, TBoundaryCondition, VOtherIsConst> & other)
    : Superclass()
    , m_BeginIndex(other.m_BeginIndex)
    , m_Bound(other.m_Bound)
    , m_Begin(other.m_Begin)
    , m_End(other.m_End)
    , m_EndIndex(other.m_EndIndex)
    , m_Loop(other.m_Loop)
    , m_Region(other.m_Region)
    , m_WrapOffset(other.m_WrapOffset)
    , m_InternalBoundaryCondition(other.m_InternalBoundaryCondition)
    , m_NeedToUseBoundaryCondition(other.m_NeedToUseBoundaryCondition)
  {
    // Copy the Neighborhood<T*> base into Neighborhood<const T*> element-wise.
    this->SetRadius(other.GetRadius());
    auto       dstIt = this->Begin();
    const auto srcEnd = other.End();
    for (auto srcIt = other.Begin(); srcIt != srcEnd; ++srcIt, ++dstIt)
    {
      *dstIt = *srcIt; // T* -> const T* is implicit
    }
    m_ConstImage = other.m_ConstImage.GetPointer();
    for (DimensionValueType i = 0; i < Dimension; ++i)
    {
      m_InBounds[i] = other.m_InBounds[i];
    }
    m_IsInBoundsValid = other.m_IsInBoundsValid;
    m_IsInBounds = other.m_IsInBounds;
    m_InnerBoundsLow = other.m_InnerBoundsLow;
    m_InnerBoundsHigh = other.m_InnerBoundsHigh;
    this->ResetBoundaryCondition();
    m_NeighborhoodAccessorFunctor = other.m_NeighborhoodAccessorFunctor;
  }

  /** Assignment operator. */
  Self &
  operator=(const Self & orig)
  {
    if (this != &orig)
    {
      Superclass::operator=(orig);

      m_Bound = orig.m_Bound;
      m_Begin = orig.m_Begin;
      m_ConstImage = orig.m_ConstImage;
      m_End = orig.m_End;
      m_EndIndex = orig.m_EndIndex;
      m_Loop = orig.m_Loop;
      m_Region = orig.m_Region;
      m_BeginIndex = orig.m_BeginIndex;
      m_WrapOffset = orig.m_WrapOffset;

      m_InternalBoundaryCondition = orig.m_InternalBoundaryCondition;
      m_NeedToUseBoundaryCondition = orig.m_NeedToUseBoundaryCondition;

      m_InnerBoundsLow = orig.m_InnerBoundsLow;
      m_InnerBoundsHigh = orig.m_InnerBoundsHigh;

      for (DimensionValueType i = 0; i < Dimension; ++i)
      {
        m_InBounds[i] = orig.m_InBounds[i];
      }
      m_IsInBoundsValid = orig.m_IsInBoundsValid;
      m_IsInBounds = orig.m_IsInBounds;

      if (orig.m_BoundaryCondition ==
          static_cast<ImageBoundaryConditionConstPointerType>(&orig.m_InternalBoundaryCondition))
      {
        this->ResetBoundaryCondition();
      }
      else
      {
        m_BoundaryCondition = orig.m_BoundaryCondition;
      }
      m_NeighborhoodAccessorFunctor = orig.m_NeighborhoodAccessorFunctor;
    }
    return *this;
  }

  /** Standard itk print method. */
  void
  PrintSelf(std::ostream & os, Indent indent) const
  {
    Superclass::PrintSelf(os, indent);

    os << indent;
    os << "NeighborhoodIteratorBase {this= " << this;
    os << ", m_Region = { Start = {";
    for (DimensionValueType i = 0; i < Dimension; ++i)
    {
      os << m_Region.GetIndex()[i] << ' ';
    }
    os << "}, Size = { ";
    for (DimensionValueType i = 0; i < Dimension; ++i)
    {
      os << m_Region.GetSize()[i] << ' ';
    }
    os << "} }";
    os << ", m_BeginIndex = { ";
    for (DimensionValueType i = 0; i < Dimension; ++i)
    {
      os << m_BeginIndex[i] << ' ';
    }
    os << "} , m_EndIndex = { ";
    for (DimensionValueType i = 0; i < Dimension; ++i)
    {
      os << m_EndIndex[i] << ' ';
    }
    os << "} , m_Loop = { ";
    for (DimensionValueType i = 0; i < Dimension; ++i)
    {
      os << m_Loop[i] << ' ';
    }
    os << "}, m_Bound = { ";
    for (DimensionValueType i = 0; i < Dimension; ++i)
    {
      os << m_Bound[i] << ' ';
    }
    os << "}, m_IsInBounds = {" << m_IsInBounds;
    os << "}, m_IsInBoundsValid = {" << m_IsInBoundsValid;
    os << "}, m_WrapOffset = { ";
    for (DimensionValueType i = 0; i < Dimension; ++i)
    {
      os << m_WrapOffset[i] << ' ';
    }
    os << ", m_Begin = " << m_Begin;
    os << ", m_End = " << m_End;
    os << '}' << std::endl;

    os << indent << ",  m_InnerBoundsLow = { ";
    for (DimensionValueType i = 0; i < Dimension; ++i)
    {
      os << m_InnerBoundsLow[i] << ' ';
    }
    os << "}, m_InnerBoundsHigh = { ";
    for (DimensionValueType i = 0; i < Dimension; ++i)
    {
      os << m_InnerBoundsHigh[i] << ' ';
    }
    os << "} }" << std::endl;
  }

  /** Computes the internal, N-d offset of a pixel array position n. */
  OffsetType
  ComputeInternalIndex(const NeighborIndexType n) const
  {
    OffsetType ans;
    auto       r = static_cast<unsigned long>(n);
    for (long i = long{ Dimension } - 1; i >= 0; --i)
    {
      ans[i] = static_cast<OffsetValueType>(r / this->GetStride(i));
      r = r % this->GetStride(i);
    }
    return ans;
  }

  /** Upper loop bounds. */
  IndexType
  GetBound() const
  {
    return m_Bound;
  }
  IndexValueType
  GetBound(NeighborIndexType n) const
  {
    return m_Bound[n];
  }

  /** Pointer to the center pixel. */
  const InternalPixelType *
  GetCenterPointer() const
  {
    return (this->operator[]((this->Size()) >> 1));
  }

  PixelType
  GetCenterPixel() const
  {
    return m_NeighborhoodAccessorFunctor.Get(this->GetCenterPointer());
  }

  /** Access to the neighborhood accessor functor (read-only). */
  const NeighborhoodAccessorFunctorType &
  GetNeighborhoodAccessor() const
  {
    return m_NeighborhoodAccessorFunctor;
  }

  ImagePointer
  GetImagePointer() const
  {
    return m_ConstImage.GetPointer();
  }

  IndexType
  GetIndex() const
  {
    return m_Loop;
  }

  inline IndexType
  GetFastIndexPlusOffset(const OffsetType & o) const
  {
    return m_Loop + o;
  }

  NeighborhoodType
  GetNeighborhood() const
  {
    const ConstIterator _end = this->End();

    NeighborhoodType ans;
    ans.SetRadius(this->GetRadius());
    if (m_NeedToUseBoundaryCondition == false)
    {
      ConstIterator thisIt = this->Begin();
      for (typename NeighborhoodType::Iterator ansIt = ans.Begin(); thisIt < _end; ++ansIt, ++thisIt)
      {
        *ansIt = m_NeighborhoodAccessorFunctor.Get(*thisIt);
      }
    }
    else if (InBounds())
    {
      ConstIterator thisIt = this->Begin();
      for (typename NeighborhoodType::Iterator ansIt = ans.Begin(); thisIt < _end; ++ansIt, ++thisIt)
      {
        *ansIt = m_NeighborhoodAccessorFunctor.Get(*thisIt);
      }
    }
    else
    {
      OffsetType temp;
      OffsetType OverlapHigh;
      OffsetType OverlapLow;
      for (DimensionValueType i = 0; i < Dimension; ++i)
      {
        OverlapLow[i] = m_InnerBoundsLow[i] - m_Loop[i];
        OverlapHigh[i] = static_cast<OffsetValueType>(this->GetSize(i)) - ((m_Loop[i] + 2) - m_InnerBoundsHigh[i]);
        temp[i] = 0;
      }

      ConstIterator thisIt = this->Begin();
      for (typename NeighborhoodType::Iterator ansIt = ans.Begin(); thisIt < _end; ++ansIt, ++thisIt)
      {
        bool flag = true;

        OffsetType offset;
        for (DimensionValueType i = 0; i < Dimension; ++i)
        {
          if (m_InBounds[i])
          {
            offset[i] = 0;
          }
          else
          {
            if (temp[i] < OverlapLow[i])
            {
              flag = false;
              offset[i] = OverlapLow[i] - temp[i];
            }
            else if (OverlapHigh[i] < temp[i])
            {
              flag = false;
              offset[i] = OverlapHigh[i] - temp[i];
            }
            else
            {
              offset[i] = 0;
            }
          }
        }

        if (flag)
        {
          *ansIt = m_NeighborhoodAccessorFunctor.Get(*thisIt);
        }
        else
        {
          *ansIt = m_NeighborhoodAccessorFunctor.BoundaryCondition(temp, offset, this, this->m_BoundaryCondition);
        }

        m_BoundaryCondition->operator()(temp, offset, this);

        for (DimensionValueType i = 0; i < Dimension; ++i)
        {
          temp[i]++;
          if (temp[i] == static_cast<OffsetValueType>(this->GetSize(i)))
          {
            temp[i] = 0;
          }
          else
          {
            break;
          }
        }
      }
    }
    return ans;
  }

  PixelType
  GetPixel(const NeighborIndexType i) const
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

  PixelType
  GetPixel(NeighborIndexType n, bool & IsInBounds) const
  {
    if (!m_NeedToUseBoundaryCondition)
    {
      IsInBounds = true;
      return (m_NeighborhoodAccessorFunctor.Get(this->operator[](n)));
    }

    if (this->InBounds())
    {
      IsInBounds = true;
      return (m_NeighborhoodAccessorFunctor.Get(this->operator[](n)));
    }

    OffsetType offset;
    OffsetType internalIndex;
    const bool flag = this->IndexInBounds(n, internalIndex, offset);
    if (flag)
    {
      IsInBounds = true;
      return (m_NeighborhoodAccessorFunctor.Get(this->operator[](n)));
    }
    else
    {
      IsInBounds = false;
      return m_NeighborhoodAccessorFunctor.BoundaryCondition(internalIndex, offset, this, this->m_BoundaryCondition);
    }
  }

  PixelType
  GetPixel(const OffsetType & o) const
  {
    bool inbounds = false;
    return (this->GetPixel(this->GetNeighborhoodIndex(o), inbounds));
  }

  PixelType
  GetPixel(const OffsetType & o, bool & IsInBounds) const
  {
    return (this->GetPixel(this->GetNeighborhoodIndex(o), IsInBounds));
  }

  PixelType
  GetNext(const unsigned int axis, NeighborIndexType i) const
  {
    return (this->GetPixel(this->GetCenterNeighborhoodIndex() + (i * this->GetStride(axis))));
  }

  PixelType
  GetNext(const unsigned int axis) const
  {
    return (this->GetPixel(this->GetCenterNeighborhoodIndex() + this->GetStride(axis)));
  }

  PixelType
  GetPrevious(const unsigned int axis, NeighborIndexType i) const
  {
    return (this->GetPixel(this->GetCenterNeighborhoodIndex() - (i * this->GetStride(axis))));
  }

  PixelType
  GetPrevious(const unsigned int axis) const
  {
    return (this->GetPixel(this->GetCenterNeighborhoodIndex() - this->GetStride(axis)));
  }

  IndexType
  GetIndex(const OffsetType & o) const
  {
    return this->GetIndex() + o;
  }

  IndexType
  GetIndex(NeighborIndexType i) const
  {
    return this->GetIndex() + this->GetOffset(i);
  }

  RegionType
  GetRegion() const
  {
    return m_Region;
  }

  IndexType
  GetBeginIndex() const
  {
    return m_BeginIndex;
  }

  RegionType
  GetBoundingBoxAsImageRegion() const
  {
    constexpr IndexValueType zero{};
    const RegionType         ans(this->GetIndex(zero), this->GetSize());
    return ans;
  }

  OffsetType
  GetWrapOffset() const
  {
    return m_WrapOffset;
  }

  OffsetValueType
  GetWrapOffset(NeighborIndexType n) const
  {
    return m_WrapOffset[n];
  }

  void
  GoToBegin()
  {
    this->SetLocation(m_BeginIndex);
  }

  void
  GoToEnd()
  {
    this->SetLocation(m_EndIndex);
  }

  void
  Initialize(const SizeType & radius, ImagePointer ptr, const RegionType & region)
  {
    m_ConstImage = ptr;
    this->SetRadius(radius);
    SetRegion(region);
    m_IsInBoundsValid = false;
    m_IsInBounds = false;
  }

  bool
  IsAtBegin() const
  {
    return this->GetCenterPointer() == m_Begin;
  }

  bool
  IsAtEnd() const
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
    return this->GetCenterPointer() == m_End;
  }

  Self &
  operator++()
  {
    const Iterator _end = Superclass::End();
    m_IsInBoundsValid = false;

    for (Iterator it = Superclass::Begin(); it < _end; ++it)
    {
      (*it)++;
    }

    for (DimensionValueType i = 0; i < Dimension; ++i)
    {
      m_Loop[i]++;
      if (m_Loop[i] == m_Bound[i])
      {
        m_Loop[i] = m_BeginIndex[i];
        for (Iterator it = Superclass::Begin(); it < _end; ++it)
        {
          *it += m_WrapOffset[i];
        }
      }
      else
      {
        break;
      }
    }
    return *this;
  }

  Self &
  operator--()
  {
    const Iterator _end = Superclass::End();
    m_IsInBoundsValid = false;

    for (Iterator it = Superclass::Begin(); it < _end; ++it)
    {
      (*it)--;
    }

    for (DimensionValueType i = 0; i < Dimension; ++i)
    {
      if (m_Loop[i] == m_BeginIndex[i])
      {
        m_Loop[i] = m_Bound[i] - 1;
        for (Iterator it = Superclass::Begin(); it < _end; ++it)
        {
          *it -= m_WrapOffset[i];
        }
      }
      else
      {
        m_Loop[i]--;
        break;
      }
    }
    return *this;
  }

  bool
  operator==(const Self & it) const
  {
    return it.GetCenterPointer() == this->GetCenterPointer();
  }

  bool
  operator!=(const Self & it) const
  {
    return !(*this == it);
  }

  bool
  operator<(const Self & it) const
  {
    return this->GetCenterPointer() < it.GetCenterPointer();
  }

  bool
  operator<=(const Self & it) const
  {
    return this->GetCenterPointer() <= it.GetCenterPointer();
  }

  bool
  operator>(const Self & it) const
  {
    return this->GetCenterPointer() > it.GetCenterPointer();
  }

  bool
  operator>=(const Self & it) const
  {
    return this->GetCenterPointer() >= it.GetCenterPointer();
  }

  void
  SetLocation(const IndexType & position)
  {
    this->SetLoop(position);
    this->SetPixelPointers(position);
  }

  Self &
  operator+=(const OffsetType & idx)
  {
    const Iterator          _end = this->End();
    OffsetValueType         accumulator = 0;
    const OffsetValueType * stride = this->GetImagePointer()->GetOffsetTable();

    m_IsInBoundsValid = false;

    accumulator += idx[0];
    for (DimensionValueType i = 1; i < Dimension; ++i)
    {
      accumulator += idx[i] * stride[i];
    }

    for (Iterator it = this->Begin(); it < _end; ++it)
    {
      *it += accumulator;
    }

    m_Loop += idx;
    return *this;
  }

  Self &
  operator-=(const OffsetType & idx)
  {
    const Iterator          _end = this->End();
    OffsetValueType         accumulator = 0;
    const OffsetValueType * stride = this->GetImagePointer()->GetOffsetTable();

    m_IsInBoundsValid = false;

    accumulator += idx[0];
    for (DimensionValueType i = 1; i < Dimension; ++i)
    {
      accumulator += idx[i] * stride[i];
    }

    for (Iterator it = this->Begin(); it < _end; ++it)
    {
      *it -= accumulator;
    }

    m_Loop -= idx;
    return *this;
  }

  OffsetType
  operator-(const Self & b) const
  {
    return m_Loop - b.m_Loop;
  }

  bool
  InBounds() const
  {
    if (m_IsInBoundsValid)
    {
      return m_IsInBounds;
    }

    bool ans = true;
    for (DimensionValueType i = 0; i < Dimension; ++i)
    {
      if (m_Loop[i] < m_InnerBoundsLow[i] || m_Loop[i] >= m_InnerBoundsHigh[i])
      {
        m_InBounds[i] = ans = false;
      }
      else
      {
        m_InBounds[i] = true;
      }
    }
    m_IsInBounds = ans;
    m_IsInBoundsValid = true;
    return ans;
  }

  bool
  IndexInBounds(const NeighborIndexType n, OffsetType & internalIndex, OffsetType & offset) const
  {
    if (!m_NeedToUseBoundaryCondition)
    {
      return true;
    }
    if (this->InBounds())
    {
      return true;
    }
    else
    {
      bool flag = true;
      internalIndex = this->ComputeInternalIndex(n);

      for (DimensionValueType i = 0; i < Dimension; ++i)
      {
        if (m_InBounds[i])
        {
          offset[i] = 0;
        }
        else
        {
          const OffsetValueType OverlapLow(m_InnerBoundsLow[i] - m_Loop[i]);
          if (internalIndex[i] < OverlapLow)
          {
            flag = false;
            offset[i] = OverlapLow - internalIndex[i];
          }
          else
          {
            const auto overlapHigh(
              static_cast<OffsetValueType>(this->GetSize(i) - ((m_Loop[i] + 2) - m_InnerBoundsHigh[i])));
            if (overlapHigh < internalIndex[i])
            {
              flag = false;
              offset[i] = overlapHigh - internalIndex[i];
            }
            else
            {
              offset[i] = 0;
            }
          }
        }
      }
      return flag;
    }
  }

  bool
  IndexInBounds(const NeighborIndexType n) const
  {
    if (!m_NeedToUseBoundaryCondition)
    {
      return true;
    }
    if (this->InBounds())
    {
      return true;
    }
    else
    {
      bool               flag = true;
      const OffsetType & internalIndex = this->ComputeInternalIndex(n);
      for (DimensionValueType i = 0; i < Dimension; ++i)
      {
        if (!m_InBounds[i])
        {
          const OffsetValueType OverlapLow(m_InnerBoundsLow[i] - m_Loop[i]);
          if (internalIndex[i] < OverlapLow)
          {
            flag = false;
          }
          else
          {
            const auto overlapHigh(
              static_cast<OffsetValueType>(this->GetSize(i) - ((m_Loop[i] + 2) - m_InnerBoundsHigh[i])));
            if (overlapHigh < internalIndex[i])
            {
              flag = false;
            }
          }
        }
      }
      return flag;
    }
  }

  // --- boundary condition setters (available on both instantiations) --------
  void
  OverrideBoundaryCondition(const ImageBoundaryConditionPointerType i)
  {
    m_BoundaryCondition = i;
  }

  void
  ResetBoundaryCondition()
  {
    m_BoundaryCondition = &m_InternalBoundaryCondition;
  }

  void
  SetBoundaryCondition(const TBoundaryCondition & c)
  {
    m_InternalBoundaryCondition = c;
  }

  ImageBoundaryConditionPointerType
  GetBoundaryCondition() const
  {
    return m_BoundaryCondition;
  }

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
  void
  SetRegion(const RegionType & region)
  {
    m_Region = region;

    const IndexType regionIndex = region.GetIndex();

    this->SetBeginIndex(region.GetIndex());
    this->SetLocation(region.GetIndex());
    this->SetBound(region.GetSize());
    this->SetEndIndex();

    m_Begin = m_ConstImage->GetBufferPointer() + m_ConstImage->ComputeOffset(regionIndex);
    m_End = m_ConstImage->GetBufferPointer() + m_ConstImage->ComputeOffset(m_EndIndex);

    const IndexType bStart = m_ConstImage->GetBufferedRegion().GetIndex();
    const SizeType  bSize = m_ConstImage->GetBufferedRegion().GetSize();
    const IndexType rStart = region.GetIndex();
    const SizeType  rSize = region.GetSize();

    m_NeedToUseBoundaryCondition = false;
    for (DimensionValueType i = 0; i < Dimension; ++i)
    {
      const auto overlapLow =
        static_cast<OffsetValueType>((rStart[i] - static_cast<OffsetValueType>(this->GetRadius(i))) - bStart[i]);
      const auto overlapHigh = static_cast<OffsetValueType>(
        (bStart[i] + bSize[i]) - (rStart[i] + rSize[i] + static_cast<OffsetValueType>(this->GetRadius(i))));

      if (overlapLow < 0)
      {
        m_NeedToUseBoundaryCondition = true;
        break;
      }

      if (overlapHigh < 0)
      {
        m_NeedToUseBoundaryCondition = true;
        break;
      }
    }
  }

  // =========================================================================
  // Write API. SFINAE-gated on !VIsConst; these methods are not present on
  // the const instantiation.
  // =========================================================================
  template <bool VCopy = VIsConst, std::enable_if_t<!VCopy, int> = 0>
  void
  SetPixel(const unsigned int n, const PixelType & v)
  {
    if (this->m_NeedToUseBoundaryCondition == false)
    {
      this->m_NeighborhoodAccessorFunctor.Set(this->operator[](n), v);
    }
    else if (this->InBounds())
    {
      this->m_NeighborhoodAccessorFunctor.Set(this->operator[](n), v);
    }
    else
    {
      OffsetType temp = this->ComputeInternalIndex(n);
      OffsetType OverlapLow;
      OffsetType OverlapHigh;
      OffsetType offset;

      for (unsigned int ii = 0; ii < Dimension; ++ii)
      {
        OverlapLow[ii] = this->m_InnerBoundsLow[ii] - this->m_Loop[ii];
        OverlapHigh[ii] =
          static_cast<OffsetValueType>(this->GetSize(ii) - ((this->m_Loop[ii] + 2) - this->m_InnerBoundsHigh[ii]));
      }

      bool flag = true;

      for (unsigned int ii = 0; ii < Dimension; ++ii)
      {
        if (this->m_InBounds[ii])
        {
          offset[ii] = 0;
        }
        else
        {
          if (temp[ii] < OverlapLow[ii])
          {
            flag = false;
            offset[ii] = OverlapLow[ii] - temp[ii];
          }
          else if (OverlapHigh[ii] < temp[ii])
          {
            flag = false;
            offset[ii] = OverlapHigh[ii] - temp[ii];
          }
          else
          {
            offset[ii] = 0;
          }
        }
      }

      if (flag)
      {
        this->m_NeighborhoodAccessorFunctor.Set(this->operator[](n), v);
      }
      else
      {
        RangeError e(__FILE__, __LINE__);
        e.SetLocation(ITK_LOCATION);
        e.SetDescription("Attempt to write out of bounds.");
        throw e;
      }
    }
  }

  template <bool VCopy = VIsConst, std::enable_if_t<!VCopy, int> = 0>
  void
  SetPixel(const unsigned int n, const PixelType & v, bool & status)
  {
    if (this->m_NeedToUseBoundaryCondition == false)
    {
      status = true;
      this->m_NeighborhoodAccessorFunctor.Set(this->operator[](n), v);
    }
    else if (this->InBounds())
    {
      this->m_NeighborhoodAccessorFunctor.Set(this->operator[](n), v);
      status = true;
      return;
    }
    else
    {
      OffsetType temp = this->ComputeInternalIndex(n);
      for (unsigned int i = 0; i < Dimension; ++i)
      {
        if (!this->m_InBounds[i])
        {
          const typename OffsetType::OffsetValueType OverlapLow = this->m_InnerBoundsLow[i] - this->m_Loop[i];
          const auto                                 OverlapHigh =
            static_cast<OffsetValueType>(this->GetSize(i) - ((this->m_Loop[i] + 2) - this->m_InnerBoundsHigh[i]));
          if (temp[i] < OverlapLow || OverlapHigh < temp[i])
          {
            status = false;
            return;
          }
        }
      }

      this->m_NeighborhoodAccessorFunctor.Set(this->operator[](n), v);
      status = true;
    }
  }

  template <bool VCopy = VIsConst, std::enable_if_t<!VCopy, int> = 0>
  void
  SetCenterPixel(const PixelType & p)
  {
    this->m_NeighborhoodAccessorFunctor.Set(this->operator[]((this->Size()) >> 1), p);
  }

  template <bool VCopy = VIsConst, std::enable_if_t<!VCopy, int> = 0>
  void
  SetNeighborhood(const NeighborhoodType & N)
  {
    const Iterator _end = this->End();

    typename NeighborhoodType::ConstIterator N_it;

    if (this->m_NeedToUseBoundaryCondition == false)
    {
      Iterator this_it = this->Begin();
      for (N_it = N.Begin(); this_it < _end; ++this_it, ++N_it)
      {
        this->m_NeighborhoodAccessorFunctor.Set(*this_it, *N_it);
      }
    }
    else if (this->InBounds())
    {
      Iterator this_it = this->Begin();
      for (N_it = N.Begin(); this_it < _end; ++this_it, ++N_it)
      {
        this->m_NeighborhoodAccessorFunctor.Set(*this_it, *N_it);
      }
    }
    else
    {
      OffsetType OverlapLow;
      OffsetType OverlapHigh;
      OffsetType temp{ 0 };
      for (unsigned int i = 0; i < Dimension; ++i)
      {
        OverlapLow[i] = this->m_InnerBoundsLow[i] - this->m_Loop[i];
        OverlapHigh[i] =
          static_cast<OffsetValueType>(this->GetSize(i) - (this->m_Loop[i] - this->m_InnerBoundsHigh[i]) - 1);
      }

      Iterator this_it = this->Begin();
      for (N_it = N.Begin(); this_it < _end; ++N_it, ++this_it)
      {
        bool flag = true;
        for (unsigned int i = 0; i < Dimension; ++i)
        {
          if (!this->m_InBounds[i] && ((temp[i] < OverlapLow[i]) || (temp[i] >= OverlapHigh[i])))
          {
            flag = false;
            break;
          }
        }

        if (flag)
        {
          this->m_NeighborhoodAccessorFunctor.Set(*this_it, *N_it);
        }

        for (unsigned int i = 0; i < Dimension; ++i)
        {
          temp[i]++;
          if (static_cast<unsigned int>(temp[i]) == this->GetSize(i))
          {
            temp[i] = 0;
          }
          else
          {
            break;
          }
        }
      }
    }
  }

protected:
  /** Set the coordinate location of the iterator. */
  void
  SetLoop(const IndexType & p)
  {
    m_Loop = p;
    m_IsInBoundsValid = false;
  }

  /** Set internal loop boundaries. */
  void
  SetBound(const SizeType & size)
  {
    SizeType                radius = this->GetRadius();
    const OffsetValueType * offset = m_ConstImage->GetOffsetTable();
    const IndexType         imageBRStart = m_ConstImage->GetBufferedRegion().GetIndex();
    SizeType                imageBRSize = m_ConstImage->GetBufferedRegion().GetSize();

    for (DimensionValueType i = 0; i < Dimension; ++i)
    {
      m_Bound[i] = m_BeginIndex[i] + static_cast<OffsetValueType>(size[i]);
      m_InnerBoundsHigh[i] = static_cast<IndexValueType>(
        imageBRStart[i] + static_cast<OffsetValueType>(imageBRSize[i]) - static_cast<OffsetValueType>(radius[i]));
      m_InnerBoundsLow[i] = static_cast<IndexValueType>(imageBRStart[i] + static_cast<OffsetValueType>(radius[i]));
      m_WrapOffset[i] = (static_cast<OffsetValueType>(imageBRSize[i]) - (m_Bound[i] - m_BeginIndex[i])) * offset[i];
    }
    m_WrapOffset[Dimension - 1] = 0;
  }

  /** Set the values of the internal pointers. */
  void
  SetPixelPointers(const IndexType & pos)
  {
    const Iterator          _end = Superclass::End();
    const SizeType          size = this->GetSize();
    const OffsetValueType * OffsetTable = m_ConstImage->GetOffsetTable();
    const SizeType          radius = this->GetRadius();

    SizeType loop{};

    // First "upper-left-corner" pixel address of neighborhood. Using
    // m_ConstImage's natural pointer type avoids the legacy const_cast.
    auto *               ptr = m_ConstImage.GetPointer();
    InternalPixelPointer Iit = ptr->GetBufferPointer() + ptr->ComputeOffset(pos);

    for (DimensionValueType i = 0; i < Dimension; ++i)
    {
      Iit -= radius[i] * OffsetTable[i];
    }

    for (Iterator Nit = Superclass::Begin(); Nit != _end; ++Nit)
    {
      *Nit = Iit;
      ++Iit;
      for (DimensionValueType i = 0; i < Dimension; ++i)
      {
        loop[i]++;
        if (loop[i] == size[i])
        {
          if (i == Dimension - 1)
          {
            break;
          }
          Iit += OffsetTable[i + 1] - OffsetTable[i] * static_cast<OffsetValueType>(size[i]);
          loop[i] = 0;
        }
        else
        {
          break;
        }
      }
    }
  }

  void
  SetBeginIndex(const IndexType & start)
  {
    m_BeginIndex = start;
  }

  void
  SetEndIndex()
  {
    if (m_Region.GetNumberOfPixels() > 0)
    {
      m_EndIndex = m_Region.GetIndex();
      m_EndIndex[Dimension - 1] =
        m_Region.GetIndex()[Dimension - 1] + static_cast<OffsetValueType>(m_Region.GetSize()[Dimension - 1]);
    }
    else
    {
      m_EndIndex = m_Region.GetIndex();
    }
  }

  // -------------------------------------------------------------------------
  // Legacy protected member layout — mirrors itkConstNeighborhoodIterator.h
  // lines 584-646.
  // -------------------------------------------------------------------------

  IndexType                         m_BeginIndex{ { 0 } };
  IndexType                         m_Bound{ { 0 } };
  InternalPixelPointer              m_Begin{ nullptr };
  ImageWeakPointerType              m_ConstImage{};
  InternalPixelPointer              m_End{ nullptr };
  IndexType                         m_EndIndex{ { 0 } };
  IndexType                         m_Loop{ { 0 } };
  RegionType                        m_Region{};
  OffsetType                        m_WrapOffset{ { 0 } };
  TBoundaryCondition                m_InternalBoundaryCondition{};
  ImageBoundaryConditionPointerType m_BoundaryCondition{ &m_InternalBoundaryCondition };
  mutable bool                      m_InBounds[Dimension]{ false };
  mutable bool                      m_IsInBounds{ false };
  mutable bool                      m_IsInBoundsValid{ false };
  IndexType                         m_InnerBoundsLow{};
  IndexType                         m_InnerBoundsHigh{};
  bool                              m_NeedToUseBoundaryCondition{ false };
  NeighborhoodAccessorFunctorType   m_NeighborhoodAccessorFunctor{};
};

// -----------------------------------------------------------------------------
// Re-templated free operators (legacy versions templated on TImage only).
// -----------------------------------------------------------------------------
template <typename TImage, typename TBoundaryCondition, bool VIsConst>
inline NeighborhoodIteratorBase<TImage, TBoundaryCondition, VIsConst>
operator+(const NeighborhoodIteratorBase<TImage, TBoundaryCondition, VIsConst> &                      it,
          const typename NeighborhoodIteratorBase<TImage, TBoundaryCondition, VIsConst>::OffsetType & ind)
{
  NeighborhoodIteratorBase<TImage, TBoundaryCondition, VIsConst> ret(it);
  ret += ind;
  return ret;
}

template <typename TImage, typename TBoundaryCondition, bool VIsConst>
inline NeighborhoodIteratorBase<TImage, TBoundaryCondition, VIsConst>
operator+(const typename NeighborhoodIteratorBase<TImage, TBoundaryCondition, VIsConst>::OffsetType & ind,
          const NeighborhoodIteratorBase<TImage, TBoundaryCondition, VIsConst> &                      it)
{
  return it + ind;
}

template <typename TImage, typename TBoundaryCondition, bool VIsConst>
inline NeighborhoodIteratorBase<TImage, TBoundaryCondition, VIsConst>
operator-(const NeighborhoodIteratorBase<TImage, TBoundaryCondition, VIsConst> &                      it,
          const typename NeighborhoodIteratorBase<TImage, TBoundaryCondition, VIsConst>::OffsetType & ind)
{
  NeighborhoodIteratorBase<TImage, TBoundaryCondition, VIsConst> ret(it);
  ret -= ind;
  return ret;
}

// -----------------------------------------------------------------------------
// Aliases that preserve the legacy public names.
// -----------------------------------------------------------------------------
template <typename TImage, typename TBC = ZeroFluxNeumannBoundaryCondition<TImage>>
using ConstNeighborhoodIterator2 = NeighborhoodIteratorBase<TImage, TBC, /*IsConst=*/true>;

template <typename TImage, typename TBC = ZeroFluxNeumannBoundaryCondition<TImage>>
using NeighborhoodIterator2 = NeighborhoodIteratorBase<TImage, TBC, /*IsConst=*/false>;


// =============================================================================
// 2. Unified shaped neighborhood iterator scaffold (design shape only; full
//    port lands in a subsequent milestone).
// =============================================================================
template <typename TImage,
          typename TBoundaryCondition = ZeroFluxNeumannBoundaryCondition<TImage>,
          bool VIsConst = false>
class ITK_TEMPLATE_EXPORT ShapedNeighborhoodIteratorBase
{
public:
  static constexpr bool IsConst = VIsConst;

  using Base = NeighborhoodIteratorBase<TImage, TBoundaryCondition, VIsConst>;

  using ImagePointer = typename Base::ImagePointer;
  using PixelType = typename Base::PixelType;
  using SizeType = typename Base::SizeType;
  using RegionType = typename Base::RegionType;
  using OffsetType = typename Base::OffsetType;
  using IndexType = typename Base::IndexType;
  using NeighborhoodType = typename Base::NeighborhoodType;
  using NeighborIndexType = typename NeighborhoodType::NeighborIndexType;

  using IndexListType = std::list<NeighborIndexType>;

  using Self = ShapedNeighborhoodIteratorBase;

  template <typename, typename, bool>
  friend class ShapedNeighborhoodIteratorBase;

  ShapedNeighborhoodIteratorBase() = default;
  ~ShapedNeighborhoodIteratorBase() = default;

  ShapedNeighborhoodIteratorBase(const SizeType & radius, ImagePointer ptr, const RegionType & region)
    : m_Base(radius, ptr, region)
  {}

  ShapedNeighborhoodIteratorBase(const ShapedNeighborhoodIteratorBase &) = default;
  ShapedNeighborhoodIteratorBase(ShapedNeighborhoodIteratorBase &&) noexcept = default;
  ShapedNeighborhoodIteratorBase &
  operator=(const ShapedNeighborhoodIteratorBase &) = default;
  ShapedNeighborhoodIteratorBase &
  operator=(ShapedNeighborhoodIteratorBase &&) noexcept = default;

  // Converting ctor: non-const -> const only (VIsConst && !VOtherIsConst).
  template <bool VOtherIsConst, std::enable_if_t<VIsConst && !VOtherIsConst, int> = 0>
  ShapedNeighborhoodIteratorBase(const ShapedNeighborhoodIteratorBase<TImage, TBoundaryCondition, VOtherIsConst> & o)
    : m_Base(o.m_Base)
    , m_ActiveIndexList(o.m_ActiveIndexList)
    , m_CenterIsActive(o.m_CenterIsActive)
  {}

  template <bool VInnerIsConst>
  struct InnerIteratorT
  {
    using OuterPointer = std::conditional_t<VInnerIsConst, const Self *, Self *>;
    using ListIterator =
      std::conditional_t<VInnerIsConst, typename IndexListType::const_iterator, typename IndexListType::iterator>;

    InnerIteratorT() = default;
    friend Self;

    InnerIteratorT &
    operator++()
    {
      ++m_ListIterator;
      return *this;
    }
    bool
    operator==(const InnerIteratorT & o) const
    {
      return m_ListIterator == o.m_ListIterator;
    }
    bool
    operator!=(const InnerIteratorT & o) const
    {
      return !(*this == o);
    }

    PixelType
    Get() const
    {
      // Direct pointer access via the base's accessor. This matches the legacy
      // shaped iterator semantics (active offsets read directly, bypassing the
      // boundary condition) and also avoids instantiating the BC-branch of
      // NeighborhoodIteratorBase::GetPixel() for VIsConst=true instantiations.
      return m_Outer->m_Base.GetNeighborhoodAccessor().Get(m_Outer->m_Base.operator[](*m_ListIterator));
    }

    void
    Set(const PixelType & v) const
    {
      static_assert(!VInnerIsConst && !IsConst, "Set() requires a non-const ShapedNeighborhoodIterator.");
      m_Outer->m_Base.SetPixel(*m_ListIterator, v);
    }

  private:
    InnerIteratorT(OuterPointer outer, ListIterator li)
      : m_Outer(outer)
      , m_ListIterator(li)
    {}

    OuterPointer m_Outer{ nullptr };
    ListIterator m_ListIterator{};
  };

  using ConstIterator = InnerIteratorT<true>;
  using Iterator = InnerIteratorT<false>;

  ConstIterator
  Begin() const
  {
    return ConstIterator(this, m_ActiveIndexList.begin());
  }
  ConstIterator
  End() const
  {
    return ConstIterator(this, m_ActiveIndexList.end());
  }

  template <bool VCopy = IsConst, std::enable_if_t<!VCopy, int> = 0>
  Iterator
  Begin()
  {
    return Iterator(this, m_ActiveIndexList.begin());
  }
  template <bool VCopy = IsConst, std::enable_if_t<!VCopy, int> = 0>
  Iterator
  End()
  {
    return Iterator(this, m_ActiveIndexList.end());
  }

  void
  ActivateOffset(const OffsetType & off)
  {
    this->ActivateIndex(m_Base.GetNeighborhoodIndex(off));
  }
  void
  DeactivateOffset(const OffsetType & off)
  {
    this->DeactivateIndex(m_Base.GetNeighborhoodIndex(off));
  }
  template <typename TOffsets>
  void
  ActivateOffsets(const TOffsets & offsets)
  {
    for (const auto & off : offsets)
    {
      this->ActivateOffset(off);
    }
  }
  void
  ClearActiveList()
  {
    m_ActiveIndexList.clear();
    m_CenterIsActive = false;
  }

  template <typename TNeighborPixel>
  void
  CreateActiveListFromNeighborhood(const Neighborhood<TNeighborPixel, Base::Dimension> & neighborhood)
  {
    if (m_Base.GetRadius() != neighborhood.GetRadius())
    {
      itkGenericExceptionMacro("Radius of shaped iterator(" << m_Base.GetRadius()
                                                            << ") does not equal radius of neighborhood("
                                                            << neighborhood.GetRadius() << ')');
    }
    NeighborIndexType idx = 0;
    for (auto nit = neighborhood.Begin(); nit != neighborhood.End(); ++nit, ++idx)
    {
      if (*nit)
      {
        this->ActivateOffset(neighborhood.GetOffset(idx));
      }
      else
      {
        this->DeactivateOffset(neighborhood.GetOffset(idx));
      }
    }
  }

  bool
  GetCenterIsActive() const
  {
    return m_CenterIsActive;
  }

  NeighborIndexType
  GetCenterNeighborhoodIndex() const
  {
    return m_Base.GetCenterNeighborhoodIndex();
  }

  OffsetType
  GetOffset(NeighborIndexType i) const
  {
    return m_Base.GetOffset(i);
  }

  NeighborIndexType
  GetNeighborhoodIndex(const OffsetType & o) const
  {
    return m_Base.GetNeighborhoodIndex(o);
  }

  SizeType
  GetRadius() const
  {
    return m_Base.GetRadius();
  }

  // Iteration: delegate to the underlying (dense) base iterator. This loses
  // the shaped-only pointer-update optimization that the legacy iterator
  // performed, in exchange for structural elimination of the const_cast
  // sites and a much simpler implementation. GetPixel() for active indices
  // goes through the base, which is always correct.
  void
  GoToBegin()
  {
    m_Base.GoToBegin();
  }
  void
  GoToEnd()
  {
    m_Base.GoToEnd();
  }
  bool
  IsAtBegin() const
  {
    return m_Base.IsAtBegin();
  }
  bool
  IsAtEnd() const
  {
    return m_Base.IsAtEnd();
  }
  Self &
  operator++()
  {
    ++m_Base;
    return *this;
  }
  Self &
  operator--()
  {
    --m_Base;
    return *this;
  }
  void
  SetLocation(const IndexType & position)
  {
    m_Base.SetLocation(position);
  }

  bool
  operator==(const Self & o) const
  {
    return m_Base == o.m_Base;
  }
  bool
  operator!=(const Self & o) const
  {
    return !(*this == o);
  }

  void
  PrintSelf(std::ostream & os, Indent indent) const
  {
    m_Base.PrintSelf(os, indent);
    os << indent << "ActiveIndexList: [";
    for (auto it = m_ActiveIndexList.begin(); it != m_ActiveIndexList.end(); ++it)
    {
      os << indent.GetNextIndent() << *it << ' ';
    }
    os << "] ";
    os << indent << "CenterIsActive: " << (m_CenterIsActive ? "On" : "Off") << std::endl;
  }
  const IndexListType &
  GetActiveIndexList() const
  {
    return m_ActiveIndexList;
  }
  typename IndexListType::size_type
  GetActiveIndexListSize() const
  {
    return m_ActiveIndexList.size();
  }

  ImagePointer
  GetImagePointer() const
  {
    return m_Base.GetImagePointer();
  }
  const RegionType &
  GetRegion() const
  {
    return m_Base.GetRegion();
  }
  IndexType
  GetIndex() const
  {
    return m_Base.GetIndex();
  }
  PixelType
  GetCenterPixel() const
  {
    return m_Base.GetCenterPixel();
  }

  template <bool VCopy = IsConst, std::enable_if_t<!VCopy, int> = 0>
  void
  SetPixel(NeighborIndexType i, const PixelType & v)
  {
    m_Base.SetPixel(i, v);
  }

  template <bool VCopy = IsConst, std::enable_if_t<!VCopy, int> = 0>
  void
  SetCenterPixel(const PixelType & v)
  {
    m_Base.SetCenterPixel(v);
  }

private:
  void
  ActivateIndex(NeighborIndexType n)
  {
    // Insert n while keeping m_ActiveIndexList sorted and deduplicated.
    auto it = m_ActiveIndexList.begin();
    if (m_ActiveIndexList.empty())
    {
      m_ActiveIndexList.push_front(n);
    }
    else
    {
      while (it != m_ActiveIndexList.end() && n > *it)
      {
        ++it;
      }
      if (it == m_ActiveIndexList.end() || n != *it)
      {
        m_ActiveIndexList.insert(it, n);
      }
    }
    if (n == m_Base.GetCenterNeighborhoodIndex())
    {
      m_CenterIsActive = true;
    }
  }

  void
  DeactivateIndex(NeighborIndexType n)
  {
    auto it = m_ActiveIndexList.begin();
    if (m_ActiveIndexList.empty())
    {
      return;
    }
    while (n != *it)
    {
      ++it;
      if (it == m_ActiveIndexList.end())
      {
        return;
      }
    }
    m_ActiveIndexList.erase(it);
    if (n == m_Base.GetCenterNeighborhoodIndex())
    {
      m_CenterIsActive = false;
    }
  }

  Base          m_Base{};
  IndexListType m_ActiveIndexList{};
  bool          m_CenterIsActive{ false };
};

template <typename TImage, typename TBC = ZeroFluxNeumannBoundaryCondition<TImage>>
using ConstShapedNeighborhoodIterator2 = ShapedNeighborhoodIteratorBase<TImage, TBC, /*IsConst=*/true>;

template <typename TImage, typename TBC = ZeroFluxNeumannBoundaryCondition<TImage>>
using ShapedNeighborhoodIterator2 = ShapedNeighborhoodIteratorBase<TImage, TBC, /*IsConst=*/false>;

} // namespace itk

#endif
