/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageIterator.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

// #include "itkImageIterator.h"

/**
   * Add two indices and perform a bounds check. This method models a
   * random access ImageIterator. If the iterator would be outside of the
   * bounds, an exception (itkBoundsError) is thrown.
   * \sa itkClampedImageIterator
   */
template<class T, unsigned int TImageDimension>
const itkImageIterator<T, TImageDimension>
itkImageIterator<T, TImageDimension>
::Add(const Index &vec)
{
  itkImageIterator<T, TImageDimension> result( *this ); // copy all the ivars
  result.m_Index = m_Index + vec;

  const long *tmpIndex = result.m_Index.GetIndex();
  for (unsigned int i=0; i < TImageDimension; i++)
    {
    if (tmpIndex[i] < m_RegionIndexOrigin[i])
      {
      throw itkBoundsError;
      }
    else if (tmpIndex[i] >= m_RegionIndexOrigin[i] + m_RegionSize[i])
      {
      throw itkBoundsError;
      }
    }
  result.ComputeOffset();
  return result;
}

/**
   * Increment an index by an index and perform a bounds check.
   * This method models a random access ImageIterator. If the iterator would
   * be outside of the bounds, and exception (itkBoundsError) is thrown.
   */
template<class T, unsigned int TImageDimension>
const itkImageIterator<T, TImageDimension> &
itkImageIterator<T, TImageDimension>
::Increment(const Index &vec)
{
  m_Index += vec;

  const long *tmpIndex = m_Index.GetIndex();
  for (unsigned int i=0; i < TImageDimension; i++)
    {
    if (tmpIndex[i] < m_RegionIndexOrigin[i])
      {
      throw itkBoundsError;
      }
    else if (tmpIndex[i] >= m_RegionIndexOrigin[i] + m_RegionSize[i])
      {
      throw itkBoundsError;
      }
    }
  this->ComputeOffset();
  return *this;
}

/**
   * Subtract two indices and perform a bounds check. This method models a
   * random access ImageIterator. If the iterator would be outside the bounds,
   * an exception (itkBoundsError) is thrown.
   */
template<class T, unsigned int TImageDimension>
const itkImageIterator<T, TImageDimension>
itkImageIterator<T, TImageDimension>
::Subtract(const Index &vec)
{
  itkImageIterator<T, TImageDimension> result( *this ); // copy all the ivars
  result.m_Index = m_Index - vec;

  const long *tmpIndex = result.m_Index.GetIndex();
  for (unsigned int i=0; i < TImageDimension; i++)
    {
    if (tmpIndex[i] < m_RegionIndexOrigin[i])
      {
      throw itkBoundsError;
      }
    else if (tmpIndex[i] >= m_RegionIndexOrigin[i] + m_RegionSize[i])
      {
      throw itkBoundsError;
      }
    }
  result.ComputeOffset();
  return result;
}

/**
   * Decrement an index by an index and perform a bounds check.  This method
   * models a random access ImageIterator. If the iterator would be outside
   * the bounds, an exception (itkBoundsError) is thrown.
   */
template<class T, unsigned int TImageDimension>
const itkImageIterator<T, TImageDimension> &
itkImageIterator<T, TImageDimension>
::Decrement(const Index &vec)
{
  m_Index -= vec;

  const long *tmpIndex = m_Index.GetIndex();
  for (unsigned int i=0; i < TImageDimension; i++)
    {
    if (tmpIndex[i] < m_RegionIndexOrigin[i])
      {
      throw itkBoundsError;
      }
    else if (tmpIndex[i] >= m_RegionIndexOrigin[i] + m_RegionSize[i])
      {
      throw itkBoundsError;
      }
    }
  this->ComputeOffset();
  return *this;
}



