/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhood.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkNeighborhood_h
#define __itkNeighborhood_h

#include <iostream>
#include "itkNeighborhoodAllocator.h"
#include "itkSize.h"
#include "itkIndent.h"
#include "itkSliceIterator.h"
#include "vnl/vnl_vector.h"

namespace itk {
  
/**
 * \class Neighborhood
 * \brief 
 *
 *
 * \sa Neighborhood
 * \sa NeighborhoodIterator
 */

template<class TPixel, unsigned int VDimension = 2,
         class TAllocator = NeighborhoodAllocator<TPixel> >
class ITK_EXPORT Neighborhood 
{
public:
  /**
   * Standard "Self" typedef
   */
  typedef Neighborhood Self;

  /**
   * External support for allocator type
   */
  typedef TAllocator AllocatorType;

  /**
   * External support for dimensionality
   */
  enum { NeighborhoodDimension = VDimension };
  
  /**
   * External support for pixel type
   */
  typedef TPixel PixelType;
  
  /**
   * Iterator typedef support.
   */
  typedef typename AllocatorType::iterator Iterator;
  typedef typename AllocatorType::const_iterator ConstIterator;

  /**
   * Size typedef support
   */
  typedef Size<VDimension> SizeType;

  /**
   * External slice iterator type typedef support
   */
  typedef SliceIterator<TPixel, Self> SliceIteratorType;
  
  /**
   * Default constructor.
   */
  Neighborhood() {}

  /**
   * Default destructor.
   */
  virtual ~Neighborhood() {}
    
  /**
   * Copy constructor
   */
  Neighborhood(const Self& other)
  {
    m_Radius     = other.m_Radius;
    m_Size       = other.m_Size;
    m_DataBuffer = other.m_DataBuffer;
  }

  /**
   * Assignment operator
   */
  Self &operator=(const Self& other)
  {
    m_Radius     = other.m_Radius;
    m_Size       = other.m_Size;
    m_DataBuffer = other.m_DataBuffer;
    return *this;
  }
  
  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(Neighborhood, );

  /**
   * Returns the radius of the neighborhood.
   */
  const SizeType GetRadius() const
  {    return m_Radius;   }

  /**
   * Returns the radius of the neighborhood along a specified
   * dimension.
   */
  unsigned long GetRadius(const unsigned long n) const
  { return m_Radius[n]; }

  /**
   * Returns the size (total length) of the neighborhood along
   * a specified dimension.
   */
  unsigned long GetSize(const unsigned long n) const
  { return m_Size[n]; }

  /**
   * Returns the size (total length of sides) of the neighborhood.
   */
  const SizeType GetSize() const
  { return m_Size; }
  
  /**
   * Returns the stride length for the specified dimension. Stride
   * length is the number of pixels between adjacent pixels along the
   * given dimension.
   */
  unsigned long GetStride(const unsigned long) const;
  
  /**
   * STL-style iterator support.
   */
  Iterator End()
  { return m_DataBuffer.end(); }

  Iterator Begin()
  { return m_DataBuffer.begin(); }
  
  const ConstIterator End() const
  { return m_DataBuffer.end(); }

  const ConstIterator Begin() const
  { return m_DataBuffer.begin(); }

  /**
   * More STL-style support
   */
  unsigned int Size() const
  { return m_DataBuffer.size(); }
  
  /**
   * Pass-through data access methods to the buffer.
   */
  TPixel &operator[](unsigned int i)
  { return m_DataBuffer[i]; }

  const TPixel &operator[](unsigned int i) const
  { return m_DataBuffer[i]; }
  
  /**
   * Sets the radius for the neighborhood, calculates size from the
   * radius, and allocates storage.
   */
  void SetRadius(const SizeType &);

  /**
   * Sets the radius for the neighborhood. Overloaded to support an unsigned
   * long array.
   */
  void SetRadius(const unsigned long *rad)
  {
    SizeType s;
    memcpy(s.m_Size, rad, sizeof(unsigned long) * VDimension);
    this->SetRadius(s);
  }
  
  /**
   * Overloads SetRadius to allow a single long integer argument
   * that is used as the radius of all the dimensions of the
   * Neighborhood (resulting in a "square" neighborhood).
   */
  void SetRadius(const unsigned long);

  /**
   * Standard itk object method.
   */
  void Print(std::ostream& os)
  { this->PrintSelf(os, Indent(0));  }

  /**
   * Returns a reference to the data buffer structure.
   */
  AllocatorType &GetBufferReference()
  { return m_DataBuffer; }
  
  const AllocatorType &GetBufferReference() const
  { return m_DataBuffer; }

  /**
   * Begin and end methods to be used internally to access data buffers.
   * These are needed internally by iterators that subclass the neighborhood
   * container to distinguish from their own End() and Begin() methods which
   * have a different function.
   */
  Iterator end()
  { return m_DataBuffer.end(); }

  Iterator begin()
  { return m_DataBuffer.begin(); }
  
  const ConstIterator end() const
  { return m_DataBuffer.end(); }

  const ConstIterator begin() const
  { return m_DataBuffer.begin(); }
  
protected:
  /**
   * Sets the length along each dimension.
   */
  void SetSize()
  {
    for (unsigned int i=0; i<VDimension; ++i)
      {
        m_Size[i] = m_Radius[i]*2+1;
      }
  }

  /**
   * Allocates the neighborhood's memory buffer.
   *
   */
  virtual void Allocate(unsigned int i)
  { m_DataBuffer.resize(i); }

  /**
   * Standard itk object method.
   */
  virtual void PrintSelf(std::ostream&, Indent) const;
  
private:
  /**
   * Number of neighbors to include (symmetrically) along each axis.
   * A neighborhood will always have odd-length axes (m_Radius[n]*2+1).
   */
  SizeType m_Radius;

   /**
   * Actual length of each dimension, calculated from m_Radius.
   * A neighborhood will always have odd-length axes (m_Radius[n]*2+1).
   */
  SizeType m_Size;

  /**
   * The buffer in which data is stored.
   */
  AllocatorType m_DataBuffer;
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNeighborhood.txx"
#endif

#endif
