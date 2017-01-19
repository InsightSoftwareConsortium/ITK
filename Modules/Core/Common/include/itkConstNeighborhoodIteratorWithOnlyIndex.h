/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
#ifndef itkConstNeighborhoodIteratorWithOnlyIndex_h
#define itkConstNeighborhoodIteratorWithOnlyIndex_h

#include <vector>
#include <cstring>
#include <iostream>
#include "itkImage.h"
#include "itkNeighborhood.h"
#include "itkMacro.h"

namespace itk
{
/** \class ConstNeighborhoodIteratorWithOnlyIndex
 *
 * \brief Index-only version of ConstNeighborhoodIterator, defining iteration of a local
 * N-dimensional neighborhood of indecies across an itk::Image or itk::ImageBase.
 *
 * ConstNeighborhoodIteratorWithOnlyIndex implements the index-only methods of
 * NeighborhoodIterator. No image data is accessed, so this iterator can be used
 * with type itk::ImageBase.  It serves as a base class from which other iterators
 * can be derived. See NeighborhoodIterator for more complete information.
 *
 * The parent class itk::Neighborhood is declared with 'char' as its first
 * template parameter, because the pixel type is not used in this class.
 *
 * \ingroup ImageIterators
 *
 * Other index-only iterators:
 *
 * \sa ImageConstIteratorWithOnlyIndex
 * \sa ConstNeighborhoodIteratorWithOnlyIndex
 *
 * Other iterators:
 *
 * \sa ImageIterator \sa NeighborhoodIterator
 *
 * \sa Neighborhood
 *
 * \ingroup ITKCommon
 *
 */
template< typename TImage >
class ITK_TEMPLATE_EXPORT ConstNeighborhoodIteratorWithOnlyIndex:
  public Neighborhood< char, TImage::ImageDimension >
{
public:

  /** Type used to refer to space dimensions */
  typedef unsigned int                  DimensionValueType;

  /** Save the image dimension. */
  itkStaticConstMacro(Dimension, DimensionValueType, TImage::ImageDimension);

  typedef char                          DummyNeighborhoodPixelType;

  /** Standard class typedefs. */
  typedef ConstNeighborhoodIteratorWithOnlyIndex Self;
  typedef Neighborhood< DummyNeighborhoodPixelType, itkGetStaticConstMacro(Dimension) >    Superclass;

  /** Inherit typedefs from superclass */
  typedef typename Superclass::OffsetType      OffsetType;
  typedef typename Superclass::RadiusType      RadiusType;
  typedef typename Superclass::SizeType        SizeType;
  typedef typename Superclass::Iterator        Iterator;
  typedef typename Superclass::ConstIterator   ConstIterator;

  /** Typedef support for common objects */
  typedef TImage                                     ImageType;
  typedef typename TImage::RegionType                RegionType;
  typedef Index< itkGetStaticConstMacro(Dimension) > IndexType;
  typedef Neighborhood< DummyNeighborhoodPixelType, itkGetStaticConstMacro(Dimension) > NeighborhoodType;

  /** Type used to refer to the elements in the list of neighbor pixels. */
  typedef typename NeighborhoodType::NeighborIndexType  NeighborIndexType;

  /** Default constructor */
  ConstNeighborhoodIteratorWithOnlyIndex();

  /** Virtual destructor */
  virtual ~ConstNeighborhoodIteratorWithOnlyIndex() {}

  /** Copy constructor */
  ConstNeighborhoodIteratorWithOnlyIndex(const ConstNeighborhoodIteratorWithOnlyIndex &);

  /** Constructor which establishes the region size, neighborhood, and image
   * over which to walk. */
  ConstNeighborhoodIteratorWithOnlyIndex(const SizeType & radius, const ImageType *ptr, const RegionType & region);

  /** Assignment operator */
  Self & operator=(const Self & orig);

  /** Standard itk print method */
  virtual void PrintSelf(std::ostream &, Indent) const;

  /** Computes the internal, N-d offset of a pixel array position n from
   * (0,0, ..., 0) in the "upper-left" corner of the neighborhood. */
  OffsetType ComputeInternalIndex(NeighborIndexType n) const;

  /** Returns the array of upper loop bounds used during iteration. */
  IndexType GetBound() const
  {
    return m_Bound;
  }

  /** Returns the loop bound used to define the edge of a single
   * dimension in the itk::Image region. */
  IndexValueType GetBound(NeighborIndexType n) const
  {
    return m_Bound[n];
  }

  /** Returns a smartpointer to the image on which this iterator operates. */
  const ImageType * GetImagePointer(void) const
  {
    return m_ConstImage;
  }

  /** Returns the N-dimensional index of the iterator's position in
   * the image. */
  virtual IndexType GetIndex(void) const
  {
    return m_Loop;
  }

  /** Returns the image index for neighbor pixel at offset o from the center of
      the neighborhood. */
  virtual IndexType GetIndex(const OffsetType & o) const
  {
    return ( this->GetIndex() + o );
  }

  /** Returns the image index for neighbor pixel at index i in the
      neighborhood. */
  virtual IndexType GetIndex(NeighborIndexType i) const
  {
    return ( this->GetIndex() + this->GetOffset(i) );
  }

  /**  Returns the region of iteration. */
  RegionType GetRegion() const
  {
    return m_Region;
  }

  /** Returns the N-dimensional starting index of the iterator's position on
   * the image. */
  IndexType GetBeginIndex() const
  {
    return m_BeginIndex;
  }

  /** Returns a bounding box for the region spanned by this neighborhood
      represented by an itk::ImageRegion */
  RegionType GetBoundingBoxAsImageRegion() const;

  /** Virtual method for rewinding the iterator to its beginning image index.
   * This is useful for writing functions which take neighborhood iterators
   * of arbitrary type and must use virtual functions. */
  virtual void GoToBegin();

  /** Virtual method for sending the iterator to one past the last index in its
   * region. */
  virtual void GoToEnd();

  /** Initializes the iterator to walk a particular image and a particular
   * region of that image. */
  virtual void Initialize(const SizeType & radius, const ImageType *ptr, const RegionType & region);

  /** Virtual method for determining whether the iterator is at the
   * beginning of its iteration region. */
  virtual bool IsAtBegin() const
  {
    return ( this->GetIndex() == m_BeginIndex );
  }

  /** Virtual method for determining whether the iterator has reached the
   * end of its iteration region. */
  virtual bool IsAtEnd() const;

  /** Increments the pointers in the ConstNeighborhoodIteratorWithOnlyIndex,
   * wraps across boundaries automatically, accounting for
   * the disparity in the buffer size and the region size of the
   * image. */
  Self & operator++();

  /** Decrements the pointers in the ConstNeighborhoodIteratorWithOnlyIndex,
   * wraps across boundaries automatically, accounting for
   * the disparity in the buffer size and the region size of the
   * image. */
  Self & operator--();

  /** Returns a boolean == comparison of the current location/index
   * of two ConstNeighborhoodIteratorWithOnlyIndexs of like
   * dimensionality.  The radii of the iterators are ignored. */
  bool operator==(const Self & it) const
  {
    return it.GetIndex() == this->GetIndex();
  }

  /** Returns a boolean != comparison of the current location/index
   * of two ConstNeighborhoodIteratorWithOnlyIndexs of like
   * dimensionality.  The radii of the iterators are ignored. */
  bool operator!=(const Self & it) const
  {
    return it.GetIndex() != this->GetIndex();
  }

  /** Returns a boolean < comparison of the  current location/index of
   * two ConstNeighborhoodIteratorWithOnlyIndexs
   * of like dimensionality.  The radii of the iterators are ignored.
   * The comparison progresses by dimension starting from the
   * greatest. */
  bool operator<(const Self & it) const;

  /** Returns a boolean <= comparison of the  current location/index of
   * two ConstNeighborhoodIteratorWithOnlyIndexs
   * of like dimensionality.  The radii of the iterators are ignored.
   * The comparison progresses by dimension starting from the
   * greatest. */
  bool operator<=(const Self & it) const;

  /** Returns a boolean > comparison of the  current location/index of
   * two ConstNeighborhoodIteratorWithOnlyIndexs
   * of like dimensionality.  The radii of the iterators are ignored.
   * The comparison progresses by dimension starting from the
   * greatest. */
  bool operator>(const Self & it) const;

  /** Returns a boolean >= comparison of the  current location/index of
   * two ConstNeighborhoodIteratorWithOnlyIndexs
   * of like dimensionality.  The radii of the iterators are ignored.
   * The comparison progresses by dimension starting from the
   * greatest. */
  bool operator>=(const Self & it) const;

  /** This method positions the iterator at an indexed location in the
   * image. SetLocation should _NOT_ be used to update the position of the
   * iterator during iteration, only for initializing it to a position
   * prior to iteration.  This method is not optimized for speed. */
  void SetLocation(const IndexType & position)
  {
    this->SetLoop(position);
  }

  /** Addition of an itk::Offset.  Note that this method does not do any bounds
   * checking.  Adding an offset that moves the iterator out of its assigned
   * region will produce undefined results. */
  Self & operator+=(const OffsetType &);

  /** Subtraction of an itk::Offset. Note that this method does not do any
   *  bounds checking.  Subtracting an offset that moves the iterator out
   * of its assigned region will produce undefined results. */
  Self & operator-=(const OffsetType &);

  /** Distance between two iterators */
  OffsetType operator-(const Self & b)
  {
    return m_Loop - b.m_Loop;
  }

  /** Returns false if the iterator overlaps region boundaries, true
   * otherwise.  Also updates an internal boolean array indicating
   * which of the iterator's faces are out of bounds. */
  bool InBounds() const;

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
  bool IndexInBounds(const NeighborIndexType n, OffsetType & internalIndex, OffsetType & offset ) const;

  /** */
  void NeedToUseBoundaryConditionOn()
  {
    this->SetNeedToUseBoundaryCondition(true);
  }

  void NeedToUseBoundaryConditionOff()
  {
    this->SetNeedToUseBoundaryCondition(false);
  }

  void SetNeedToUseBoundaryCondition(bool b)
  {
    m_NeedToUseBoundaryCondition = b;
  }

  bool GetNeedToUseBoundaryCondition() const
  {
    return m_NeedToUseBoundaryCondition;
  }

protected:

  /** Default method for setting the coordinate location of the iterator.
   * Loop indices correspond to the actual Image region index. */
  virtual void SetLoop(const IndexType & p)
  {
    m_Loop = p; m_IsInBoundsValid = false;
  }

  /** Virtual method for setting internal loop boundaries.  This
   * method must be defined in each subclass because
   * each subclass may handle loop boundaries differently. */
  virtual void SetBound(const SizeType &);

  /** Default method for setting the first index of the
   * iteration region. */
  virtual void SetBeginIndex(const IndexType & start)
  {
    m_BeginIndex = start;
  }

  /** Default method for setting the last index of the
   * iteration region. */
  virtual void SetEndIndex();

  /** The starting index for iteration within the itk::Image region
   * on which this ConstNeighborhoodIteratorWithOnlyIndex is defined. */
  IndexType m_BeginIndex;

  /** An array of upper looping boundaries used during iteration. */
  IndexType m_Bound;

  /** The image on which iteration is defined. */
  typename ImageType::ConstPointer m_ConstImage;

  /** The end index for iteration within the itk::Image region
   * on which this ConstNeighborhoodIteratorWithOnlyIndex is defined. */
  IndexType m_EndIndex;

  /** Array of loop counters used during iteration. */
  IndexType m_Loop;

  /** The region over which iteration is defined. */
  RegionType m_Region;

  /** Denotes which of the iterators dimensional sides spill outside
   * region of interest boundaries. */
  mutable bool m_InBounds[Dimension];

  /** Denotes if iterator is entirely within bounds */
  mutable bool m_IsInBounds;

  /** Is the m_InBounds and m_IsInBounds variables up to date? Set to
   * false whenever the iterator is repositioned.  Set to true within
   * InBounds(). */
  mutable bool m_IsInBoundsValid;

  /** Lower threshold of in-bounds loop counter values. */
  IndexType m_InnerBoundsLow;

  /** Upper threshold of in-bounds loop counter values. */
  IndexType m_InnerBoundsHigh;

  /** Does the specified region need to worry about boundary conditions? */
  bool m_NeedToUseBoundaryCondition;
};

template< typename TImage >
inline ConstNeighborhoodIteratorWithOnlyIndex< TImage >
operator+(const ConstNeighborhoodIteratorWithOnlyIndex< TImage > & it,
          const typename ConstNeighborhoodIteratorWithOnlyIndex< TImage >
          ::OffsetType & ind)
{
  ConstNeighborhoodIteratorWithOnlyIndex< TImage > ret;
  ret = it;
  ret += ind;
  return ret;
}

template< typename TImage >
inline ConstNeighborhoodIteratorWithOnlyIndex< TImage >
operator+(const typename ConstNeighborhoodIteratorWithOnlyIndex< TImage >
          ::OffsetType & ind,
          const ConstNeighborhoodIteratorWithOnlyIndex< TImage > & it)
{  return ( it + ind ); }

template< typename TImage >
inline ConstNeighborhoodIteratorWithOnlyIndex< TImage >
operator-(const ConstNeighborhoodIteratorWithOnlyIndex< TImage > & it,
          const typename ConstNeighborhoodIteratorWithOnlyIndex< TImage >
          ::OffsetType & ind)
{
  ConstNeighborhoodIteratorWithOnlyIndex< TImage > ret;
  ret = it;
  ret -= ind;
  return ret;
}
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkConstNeighborhoodIteratorWithOnlyIndex.hxx"
#endif

#endif
