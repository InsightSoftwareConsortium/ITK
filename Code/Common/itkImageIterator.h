/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageIterator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
  ==========================================================================*/

/**
 * itkImageIterator is a templated class to represent a multi-dimensional
 * iterator. itkImageIterator is templated over the dimension of the image
 * and the data type of the image.
 *
 * For efficiency sake, itkImageIterator does not define a default constructor,
 * a copy constructor, or an operator=. We rely on the compiler to provide
 * efficient bitwise copies.
 *
 * Note that itkImageIterator knows about the organization of the image data.
 * This class may need to be wholly contained by itkImage.
 *
 */


#ifndef __itkImageIterator_h
#define __itkImageIterator_h

#include <memory.h>
#include "itkIndex.h"

template<class TPixel, unsigned int TImageDimension=2>
class itkImageIterator {
public:
  /** 
   * Index typedef support 
   */
  typedef itkIndex<TImageDimension> Index;

  /**
   * Get the dimension (size) of the index.
   */
  static unsigned int GetImageIteratorDimension() 
    {return TImageDimension;}

  /**
   * Add two indices. This method models a random access ImageIterator.
   * No bounds checking is performed.
   */
  bool
  operator!=(const itkImageIterator<TPixel,TImageDimension> &it) const
    {
    return true;
    }

  /**
   * Add two indices. This method models a random access ImageIterator.
   * No bounds checking is performed.
   */
  const itkImageIterator<TPixel, TImageDimension>
  operator+(const Index &vec)
    {
    itkImageIterator<TPixel, TImageDimension> result( *this ); // copy all the ivars
    result.m_Index = m_Index + vec;  
    result.ComputeOffset();
    return result;
    }

  /**
   * Increment an index by an index. This method models a random access
   * ImageIterator. No bounds checking is performed.
   */
  const itkImageIterator<TPixel, TImageDimension> &
  operator+=(const Index &vec)
    {
    m_Index += vec;
    this->ComputeOffset();
    return *this;
    }

  /**
   * Subtract two indices. This method models a random access ImageIterator.
   * No bounds checking is performed.
   */
  const itkImageIterator<TPixel, TImageDimension>
  operator-(const Index &vec)
    {
    itkImageIterator<TPixel, TImageDimension> result( *this ); // copy all the ivars
    result.m_Index = m_Index - vec;
    result.ComputeOffset();
    return result;
    }

  /**
   * Decrement an index by an index. This method models a random access
   * ImageIterator. No bounds checking is performed.
   */
  const itkImageIterator<TPixel, TImageDimension> &
  operator-=(const Index &vec)
    {
    m_Index -= vec;
    this->ComputeOffset();
    return *this;
    }

  /**
   * Add a delta to the fastest moving dimension of the iterator's index. 
   * This is an optimized method to be used in the inner most loops. 
   * This method models a random access ImageIterator. No bounds checking 
   * is performed.
   */
  const itkImageIterator<TPixel, TImageDimension>
  operator+(long delta)
    {
    itkImageIterator<TPixel, TImageDimension> result( *this ); // copy all the ivars
    result.m_Index[TImageDimension-1] += delta;
    result.m_Offset += delta;
    return result;
    }

  /**
   * Increment the fastest moving dimension of the iterator's index by a delta.
   * This is an optimized method to be used in the inner most loops. This 
   * method models a random access ImageIterator. No bounds checking is 
   * performed.
   */
  const itkImageIterator<TPixel, TImageDimension> &
  operator+=(long delta)
    {
    m_Index[TImageDimension-1] += delta;
    m_Offset += delta;
    return *this;
    }

  /**
   * Subtract a delta from the fastest moving dimension of the iterator's
   * index. This is an optimized method to be used in the inner most loops. 
   * This method models a random access ImageIterator. No bounds checking 
   * is performed.
   */
  const itkImageIterator<TPixel, TImageDimension>
  operator-(long delta)
    {
    itkImageIterator<TPixel, TImageDimension> result( *this ); // copy all the ivars
    result.m_Index[TImageDimension-1] -= delta;
    result.m_Offset -= delta;
    return result;
    }

  /**
   * Decrement the fastest moving dimension of the iterator's index by a delta.
   * This is an optimized method to be used in the inner most loops. This 
   * method models a random access ImageIterator. No bounds checking is 
   * performed.
   */
  const itkImageIterator<TPixel, TImageDimension> &
  operator-=(long delta)
    {
    m_Index[TImageDimension-1] -= delta;
    m_Offset -= delta;
    return *this;
    }

  /**
   * Increment (prefix) the fastest moving dimension of the iterator's index.
   * This is an optimized method to be used in the inner most loops. This 
   * method models a random access ImageIterator. No bounds checking is 
   * performed. The prefix form of this operator is more efficient.
   * \sa operator++(int)
   */
  const itkImageIterator<TPixel, TImageDimension> &
  operator++()
    {
    m_Index[TImageDimension-1]++;
    m_Offset++;
    return *this;
    }

  /**
   * Increment (postfix) the fastest moving dimension of the iterator's index.
   * This is an optimized method to be used in the inner most loops. This 
   * method models a random access ImageIterator. No bounds checking is 
   * performed. The prefix form of this operator is more efficient.
   * \sa operator++()
   */
  const itkImageIterator<TPixel, TImageDimension> 
  operator++(int)
    {
    itkImageIterator<TPixel, TImageDimension> result( *this ); // copy all ivars
    m_Index[TImageDimension-1]++;
    m_Offset++;
    return result;
    }

  /**
   * Decrement (prefix) the fastest moving dimension of the iterator's index.
   * This is an optimized method to be used in the inner most loops. This 
   * method models a random access ImageIterator. No bounds checking is 
   * performed. The prefix form of this operator is more efficient.
   * \sa operator--(int)
   */
  const itkImageIterator<TPixel, TImageDimension> &
  operator--()
    {
    m_Index[TImageDimension-1]--;
    m_Offset--;
    return *this;
    }

  /**
   * Decrement (postfix) the fastest moving dimension of the iterator's index.
   * This is an optimized method to be used in the inner most loops. This 
   * method models a random access ImageIterator. No bounds checking is 
   * performed. The prefix form of this operator is more efficient.
   * \sa operator--()
   */
  const itkImageIterator<TPixel, TImageDimension> 
  operator--(int)
    {
    itkImageIterator<TPixel,  TImageDimension> result( *this ); // copy all ivars
    m_Index[TImageDimension-1]--;
    m_Offset--;
    return result;
    }

  /**
   * Add two indices and perform a bounds check. This method models a
   * random access ImageIterator. If the iterator would be outside of the
   * bounds, an exception (itkBoundsError) is thrown.
   * \sa itkClampedImageIterator
   */
  const itkImageIterator<TPixel, TImageDimension> Add(const Index &vec);

  /**
   * Increment an index by an index and perform a bounds check.
   * This method models a random access ImageIterator. If the iterator would
   * be outside of the bounds, and exception (itkBoundsError) is thrown.
   * \sa itkClampedImageIterator
   */
  const itkImageIterator<TPixel, TImageDimension> &Increment(const Index &vec);

  /**
   * Subtract two indices and perform a bounds check. This method models a
   * random access ImageIterator. If the iterator would be outside the bounds,
   * an exception (itkBoundsError) is thrown.
   * \sa itkClampedImageIterator
   */
  const itkImageIterator<TPixel, TImageDimension> Subtract(const Index &vec);

  /**
   * Decrement an index by an index and perform a bounds check.  This method
   * models a random access ImageIterator. If the iterator would be outside
   * the bounds, an exception (itkBoundsError) is thrown.
   * \sa itkClampedImageIterator
   */
  const itkImageIterator<TPixel, TImageDimension> &Decrement(const Index &vec);
  
  /**
   * Get the index. This provides a read only reference to the index.
   * \sa SetIndex
   */
  const Index &GetIndex() const 
    { return m_Index; }

  /**
   * Set the index. No bounds checking is performed.
   * \sa GetIndex
   */
  void SetIndex(const Index &ind)
    { m_Index = ind; this->ComputeOffset(); }


  /**
   *
   */
  void SetPointer( TPixel* ptr)
    { m_Image = ptr; m_Offset = 0;}
  void SetImageSize( const unsigned long *size)
    { memcpy(m_ImageSize, size, TImageDimension*sizeof(unsigned long)); }
  void SetRegionSize( const unsigned long *size)
    { memcpy(m_RegionSize, size, TImageDimension*sizeof(unsigned long)); }
  void SetImageIndexOrigin( const long *origin)
    { memcpy(m_ImageIndexOrigin, origin, TImageDimension*sizeof(long)); }
  void SetRegionIndexOrigin( const long *origin)
    { memcpy(m_RegionIndexOrigin, origin, TImageDimension*sizeof(long)); }

  const unsigned long *GetImageSize() const 
    { return m_ImageSize; }

  /**
   * Dereference the iterator, returns a reference to the pixel. Used to set
   * or get the value referenced by the index.
   */
  TPixel& operator*() const  
    { return *( m_Image + m_Offset ); }
  
protected:
  /**
   * Compute the offset from the beginning of the image to the pixel under
   * the iterator.
   */
  void ComputeOffset() 
  {
    unsigned long prod=1;
    const unsigned long *index = m_Index.GetIndex();

    // data is arranged as [][][][slice][row][col]
    m_Offset = 0;
    for (int i=TImageDimension-1; i >= 0; i--)
      {
      m_Offset += prod*(index[i] - m_ImageIndexOrigin[i]);
      prod *= m_ImageSize[i];
      }
  }

protected: //made protected so other iterators can access 
  Index          m_Index;
  unsigned long  m_Offset;
  TPixel        *m_Image;
  unsigned long  m_ImageSize[TImageDimension];
  unsigned long  m_RegionSize[TImageDimension];
  long           m_ImageIndexOrigin[TImageDimension];
  long           m_RegionIndexOrigin[TImageDimension];
};


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageIterator.cxx"
#endif

#endif 
