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
#ifndef itkNeighborhood_h
#define itkNeighborhood_h

#include <iostream>
#include "itkNeighborhoodAllocator.h"
#include "itkIndent.h"
#include "itkSliceIterator.h"
#include "vnl/vnl_vector.h"
#include "itkOffset.h"
#include <vector>

namespace itk
{
/** \class Neighborhood
 * \brief A light-weight container object for storing an N-dimensional
 * neighborhood of values.
 *
 * This class serves as the base class for several other Itk objects such as
 * itk::NeighborhoodOperator and itk::NeighborhoodIterator.  Its purpose is to
 * store values and their relative spatial locations.
 *
 * A Neighborhood has an N-dimensional \em radius.  The radius is defined
 * separately for each dimension as the number of pixels that the neighborhood
 * extends outward from the center pixel.  For example, a 2D Neighborhood
 * object with a radius of 2x3 has sides of length 5x7.  Neighborhood objects
 * always have an unambiguous center because their side lengths are always odd.
 *
 * \sa Neighborhood
 * \sa NeighborhoodIterator
 *
 * \ingroup Operators
 * \ingroup ImageIterators
 * \ingroup ITKCommon
 */

template< typename TPixel, unsigned int VDimension = 2,
          typename TAllocator = NeighborhoodAllocator< TPixel > >
class ITK_TEMPLATE_EXPORT Neighborhood
{
public:
  /** Standard class typedefs. */
  typedef Neighborhood Self;

  /** External support for allocator type. */
  typedef TAllocator AllocatorType;

  /** External support for dimensionality. */
  itkStaticConstMacro(NeighborhoodDimension, unsigned int, VDimension);

  /** Run-time type information (and related methods). */
  itkTypeMacroNoParent(Neighborhood);

  /** External support for pixel type. */
  typedef TPixel PixelType;

  /** Iterator typedef support. Note the naming is intentional, i.e.,
  * AllocatorType::iterator and AllocatorType::const_iterator, because the
  * allocator may be a vnl object or other type, which uses this form. */
  typedef typename AllocatorType::iterator       Iterator;
  typedef typename AllocatorType::const_iterator ConstIterator;

  /** Size and value typedef support. */
  typedef::itk::Size< VDimension >         SizeType;
  typedef typename SizeType::SizeValueType SizeValueType;

  /** Radius typedef support. */
  typedef::itk::Size< VDimension > RadiusType;

  /** Offset type used to reference neighbor locations */
  typedef Offset< VDimension > OffsetType;

  /** External slice iterator type typedef support. */
  typedef SliceIterator< TPixel, Self > SliceIteratorType;

  /** Type used to refer to space dimensions */
  typedef unsigned int                  DimensionValueType;

  /** Type used to refer to the elements of the pixel list
   * that are part of the neighborhood. */
  typedef SizeValueType                 NeighborIndexType;

  /** Default constructor. */
  Neighborhood()
  {
    m_Radius.Fill(0);
    m_Size.Fill(0);
    for ( DimensionValueType i = 0; i < VDimension; i++ )
      {
      m_StrideTable[i] = 0;
      }
  }

  /** Default destructor. */
  virtual ~Neighborhood() {}

  /** Copy constructor. */
  Neighborhood(const Self & other);

  /** Assignment operator. */
  Self & operator=(const Self & other);

  /** Comparison operator. */
  bool
  operator==(const Self & other) const
  {
    return ( m_Radius == other.m_Radius
             && m_Size   == other.m_Size
             && m_DataBuffer == other.m_DataBuffer );
  }

  /** Not Equal operator. */
  bool operator!=(const Self & other) const
  {
    return ( m_Radius != other.m_Radius
             || m_Size   != other.m_Size
             || m_DataBuffer != other.m_DataBuffer );
  }

  /** Returns the radius of the neighborhood. */
  const SizeType GetRadius() const
  { return m_Radius; }

  /** Returns the radius of the neighborhood along a specified
   * dimension. */
  SizeValueType GetRadius(DimensionValueType n) const
  { return m_Radius[n]; }

  /** Returns the size (total length) of the neighborhood along
   * a specified dimension. */
  SizeValueType GetSize(DimensionValueType n) const
  { return m_Size[n]; }

  /** Returns the size (total length of sides) of the neighborhood. */
  SizeType GetSize() const
  { return m_Size; }

  /** Returns the stride length for the specified dimension. Stride
   * length is the number of pixels between adjacent pixels along the
   * given dimension. */
  OffsetValueType GetStride(DimensionValueType axis) const
  { return ( axis < VDimension ) ? m_StrideTable[axis] : 0;  }

  /** STL-style iterator support. */
  Iterator End()
  { return m_DataBuffer.end(); }
  Iterator Begin()
  { return m_DataBuffer.begin(); }
  ConstIterator End() const
  { return m_DataBuffer.end(); }
  ConstIterator Begin() const
  { return m_DataBuffer.begin(); }

  /** More STL-style support. */
  NeighborIndexType Size() const
  { return m_DataBuffer.size(); }

  /** Pass-through data access methods to the buffer. */
  TPixel & operator[](NeighborIndexType i)
  { return m_DataBuffer[i]; }
  const TPixel & operator[](NeighborIndexType i) const
  { return m_DataBuffer[i]; }
  TPixel & GetElement(NeighborIndexType i)
  { return m_DataBuffer[i]; }

  /** Returns the element at the center of the neighborhood. */
  TPixel GetCenterValue() const
  { return ( this->operator[]( ( this->Size() ) >> 1 ) ); }

  /** Sets the radius for the neighborhood, calculates size from the
   * radius, and allocates storage. */
  void SetRadius(const SizeType &);

  /** Sets the radius for the neighborhood. Overloaded to support an unsigned
   * long array. */
  void SetRadius(const SizeValueType *rad)
  {
    SizeType s;
    std::copy(rad,
              rad+VDimension,
              s.m_Size);
    this->SetRadius(s);
  }

  /** Overloads SetRadius to allow a single long integer argument
   * that is used as the radius of all the dimensions of the
   * Neighborhood (resulting in a "square" neighborhood). */
  void SetRadius(const SizeValueType);

  /** Standard itk object method. */
  void Print(std::ostream & os) const
  { this->PrintSelf( os, Indent(0) );  }

  /** Returns a reference to the data buffer structure. */
  AllocatorType & GetBufferReference()
  { return m_DataBuffer; }
  const AllocatorType & GetBufferReference() const
  { return m_DataBuffer; }

  /** Get pixel value by offset */
  TPixel & operator[](const OffsetType & o)
  { return this->operator[]( this->GetNeighborhoodIndex(o) ); }
  const TPixel & operator[](const OffsetType & o) const
  { return this->operator[]( this->GetNeighborhoodIndex(o) ); }

  /** Returns the itk::Offset from the center of the Neighborhood to
      the requested neighbor index. */
  OffsetType GetOffset(NeighborIndexType i) const
  { return m_OffsetTable[i]; }

  virtual NeighborIndexType GetNeighborhoodIndex(const OffsetType &) const;

  NeighborIndexType GetCenterNeighborhoodIndex() const
  {
    return static_cast< NeighborIndexType >( this->Size() / 2 );
  }

  std::slice GetSlice(unsigned int) const;

protected:
  /** Sets the length along each dimension. */
  void SetSize()
  {
    for ( DimensionValueType i = 0; i < VDimension; ++i )
      {
      m_Size[i] = m_Radius[i] * 2 + 1;
      }
  }

  /** Allocates the neighborhood's memory buffer. */
  virtual void Allocate(NeighborIndexType i)
  { m_DataBuffer.set_size(i); }

  /** Standard itk object method. */
  virtual void PrintSelf(std::ostream &, Indent) const;

  /** Computes the entries for the stride table */
  virtual void ComputeNeighborhoodStrideTable();

  /** Fills entries into the offset lookup table. Called once on
      initialization. */
  virtual void ComputeNeighborhoodOffsetTable();

private:
  /** Number of neighbors to include (symmetrically) along each axis.
   * A neighborhood will always have odd-length axes (m_Radius[n]*2+1). */
  SizeType m_Radius;

  /** Actual length of each dimension, calculated from m_Radius.
   * A neighborhood will always have odd-length axes (m_Radius[n]*2+1). */
  SizeType m_Size;

  /** The buffer in which data is stored. */
  AllocatorType m_DataBuffer;

  /** A lookup table for keeping track of stride lengths in a neighborhood
      i.e. the memory offsets between pixels along each dimensional axis */
  OffsetValueType m_StrideTable[VDimension];

  /** */
  std::vector< OffsetType > m_OffsetTable;
};

template< typename TPixel, unsigned int VDimension, typename TContainer >
std::ostream & operator<<(std::ostream & os, const Neighborhood< TPixel, VDimension, TContainer > & neighborhood)
{
  os << "Neighborhood:" << std::endl;
  os << "    Radius:" << neighborhood.GetRadius() << std::endl;
  os << "    Size:" << neighborhood.GetSize() << std::endl;
  os << "    DataBuffer:" << neighborhood.GetBufferReference() << std::endl;

  return os;
}
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNeighborhood.hxx"
#endif

/*
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNeighborhood.hxx"
#endif
*/

#endif
