/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhoodBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkNeighborhoodBase_h
#define __itkNeighborhoodBase_h

#include "itkMacro.h"
#include "itkSliceIterator.h"
#include <valarray>

namespace itk {
  
/**
 * \class NeighborhoodBase
 * \brief Base class for Neighborhood and NeighborhoodIterator classes.
 *
 * NeighborhoodBase is the base class for Neighborhood and
 * NeighborhoodIterator classes. The class manages the size of the
 * neighborhood and defines iterators for accessing the data contained
 * within a neighborhood.
 *
 * \sa Neighborhood
 * \sa NeighborhoodIterator
 */

template<class TPixel, unsigned int VDimension = 2>
class NeighborhoodBase : public std::valarray<TPixel>
{
public:

  /**
   * Standard "Self" typedef
   */
  typedef NeighborhoodBase Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef std::valarray<TPixel>  Superclass;

  /**
   * Iterator typedef support.
   */
  typedef TPixel * Iterator;

  /**
   * Const iterator typedef support.
   */
  typedef const TPixel * ConstIterator;

  /**
   * Slice iterator typedef support.
   */
  typedef SliceIterator<TPixel, Self, VDimension> SliceIterator;
  
  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(NeighborhoodBase, std::valarray);

  /**
   * Returns the radius of the neighborhood.
   */
  const unsigned long *GetRadius() const
  {
    return m_Radius;
  }

  /**
   * Returns the radius of the neighborhood along a specified
   * dimension.
   */
  unsigned long GetRadius(const unsigned long &n) const
  {
    return m_Radius[n];
  }

  /**
   * Returns the size (total length) of the neighborhood along
   * a specified dimension.
   */
  unsigned long GetSize(const unsigned long &n) const
  {
    return m_Size[n];
  }

  /**
   * Returns the size (total length of sides) of the neighborhood.
   */
  const unsigned long *GetSize() const
  {
    return m_Size;
  }
  
  /**
   * Returns the stride length for the specified dimension. Stride
   * length is the number of pixels between adjacent pixels along the
   * given dimension.
   */
  unsigned long GetStride(const unsigned long &) const;
  
  /**
   * Limited STL-style iterator support, returns an iterator that
   * points to one past the last  element of the neighborhood's memory buffer.
   */
  Iterator End()
  {
    return this->end();
  }

  /**
   * STL-style iterator support, returns an iterator that
   * points to the beginning element of the neighborhood's memory buffer.
   */
  Iterator Begin()
  {
    return this->begin();
  }

  /**
   * STL-style iterator support, returns an iterator that
   * points to the beginning element of the neighborhood's memory buffer.
   */
  Iterator begin() // --- Note: This is a workaround for the ?incomplete?
  {                //           implementation of valarray on the GNU
                   //           2.95.1 gcc compiler.  jc 10-05-00     
    return &( this->operator[](0) );
  }

  /**
   * STL-style iterator support, returns an iterator that
   * points one past the last element of the neighborhood's memory buffer.
   */
  Iterator end()  // --- Note:  This is a workaround for the ?incomplete?
  {               //            implementation of valarray on the GNU
                  //            2.95.1 gcc compiler.  jc 10-05-00 
    return &( this->operator[](this->size()-1) )+1;
  }

  /**
   * Sets the radius for the neighborhood, calculates size from the
   * radius, and allocates storage.
   */
  void SetRadius(const unsigned long *);

  /**
   * Overloads SetRadius to allow a single long integer argument
   * that is used as the radius of all the dimensions of the
   * Neighborhood (resulting in a "square" neighborhood).
   */
  void SetRadius(const unsigned long &);
   
  /**
   * Prints some information about the neighborhood for debugging
   * purposes.
   */
  virtual void Print();
  
protected:
  /**
   * Sets the length along each dimension.
   */
  void SetSize()
  {
    for (int i=0; i<VDimension; ++i)
      {
        m_Size[i] = m_Radius[i]*2+1;
      }
  }

  /**
   * Allocates the neighborhood's memory buffer.
   *
   */
  virtual void Allocate(const unsigned int &i)
  {
    this->resize(i);
  }

private:
  /**
   * Number of neighbors to include (symmetrically) along each axis.
   * A neighborhood will always have odd-length axes (m_Radius[n]*2+1).
   */
  unsigned long  m_Radius[VDimension];

   /**
   * Actual length of each dimension, calculated from m_Radius.
   * A neighborhood will always have odd-length axes (m_Radius[n]*2+1).
   */
  unsigned long m_Size[VDimension];
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNeighborhoodBase.txx"
#endif

#endif
