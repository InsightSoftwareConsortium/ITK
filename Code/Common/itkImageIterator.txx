/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageIterator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

// #include "itkImageIterator.h"

namespace itk
{

/**
 * Add two indices and perform a bounds check. This method models a
 * random access ImageIterator. If the iterator would be outside of the
 * bounds, an exception (BoundsError) is thrown.
 * \sa ClampedImageIterator
 */
template<class TPixel, unsigned int TImageDimension>
const ImageIterator<TPixel, TImageDimension>
ImageIterator<TPixel, TImageDimension>
::Add(const Index &vec)
{
  ImageIterator<TPixel, TImageDimension> result( *this ); // copy all the ivars
  result.m_Index = m_Index + vec;

  const long *tmpIndex = result.m_Index.GetIndex();
  for (unsigned int i=0; i < TImageDimension; i++)
    {
    if (tmpIndex[i] < m_RegionIndexOrigin[i])
      {
      throw BoundsError;
      }
    else if (tmpIndex[i] >= m_RegionIndexOrigin[i] + m_RegionSize[i])
      {
      throw BoundsError;
      }
    }
  result.ComputeOffset();
  return result;
}

/**
   * Increment an index by an index and perform a bounds check.
   * This method models a random access ImageIterator. If the iterator would
   * be outside of the bounds, and exception (BoundsError) is thrown.
   */
template<class TPixel, unsigned int TImageDimension>
const ImageIterator<TPixel, TImageDimension> &
ImageIterator<TPixel, TImageDimension>
::Increment(const Index &vec)
{
  m_Index += vec;

  const long *tmpIndex = m_Index.GetIndex();
  for (unsigned int i=0; i < TImageDimension; i++)
    {
    if (tmpIndex[i] < m_RegionIndexOrigin[i])
      {
      throw BoundsError;
      }
    else if (tmpIndex[i] >= m_RegionIndexOrigin[i] + m_RegionSize[i])
      {
      throw BoundsError;
      }
    }
  this->ComputeOffset();
  return *this;
}

/**
   * Subtract two indices and perform a bounds check. This method models a
   * random access ImageIterator. If the iterator would be outside the bounds,
   * an exception (BoundsError) is thrown.
   */
template<class TPixel, unsigned int TImageDimension>
const ImageIterator<TPixel, TImageDimension>
ImageIterator<TPixel, TImageDimension>
::Subtract(const Index &vec)
{
  ImageIterator<TPixel, TImageDimension> result( *this ); // copy all the ivars
  result.m_Index = m_Index - vec;
  
  const long *tmpIndex = result.m_Index.GetIndex();
  for (unsigned int i=0; i < TImageDimension; i++)
    {
    if (tmpIndex[i] < m_RegionIndexOrigin[i])
      {
      throw BoundsError;
      }
    else if (tmpIndex[i] >= m_RegionIndexOrigin[i] + m_RegionSize[i])
      {
      throw BoundsError;
      }
    }
  result.ComputeOffset();
  return result;
}

/**
   * Decrement an index by an index and perform a bounds check.  This method
   * models a random access ImageIterator. If the iterator would be outside
   * the bounds, an exception (BoundsError) is thrown.
   */
template<class TPixel, unsigned int TImageDimension>
const ImageIterator<TPixel, TImageDimension> &
ImageIterator<TPixel, TImageDimension>
::Decrement(const Index &vec)
{
  m_Index -= vec;

  const long *tmpIndex = m_Index.GetIndex();
  for (unsigned int i=0; i < TImageDimension; i++)
    {
    if (tmpIndex[i] < m_RegionIndexOrigin[i])
      {
      throw BoundsError;
      }
    else if (tmpIndex[i] >= m_RegionIndexOrigin[i] + m_RegionSize[i])
      {
      throw BoundsError;
      }
    }
  this->ComputeOffset();
  return *this;
}

} // namespace itk
