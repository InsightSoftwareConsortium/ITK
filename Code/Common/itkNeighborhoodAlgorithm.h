/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhoodAlgorithm.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

#ifndef __itkNeighborhoodAlgorithm_h
#define __itkNeighborhoodAlgorithm_h

#include "itkImage.h"
#include "itkNeighborhood.h"
#include "itkNeighborhoodOperator.h"
#include "itkExceptionObject.h"

namespace itk
{
  
namespace NeighborhoodAlgorithm
{      
/**
 * \class ScalarOperation
 *
 * Parent class for scalar operation function objects on image neighborhoods.
 * Scalar operations are defined in this context as operations that return a
 * scalar value. Defines the interface for operations that use all of the
 * neighborhood data, and operations that use only a slice of the
 * neighborhood data.
 *
 * An inner product with an array of coefficients, for example, is an operation 
 * that can be defined on a neighborhood (or neighborood iterator).  The sliced
 * variation might allow the inner product with an array of coefficients
 * applied only to the elements centered along a dimensional axis of the
 * neighborhood.
 */
template<class TContainer, class TArray>
struct ITK_EXPORT ScalarOperation
{
  typedef typename TContainer::PixelType InternalType;
  typedef typename TArray::PixelType ScalarType;
  
  /**
   * Defines the operation on the container.
   */
  virtual ScalarType operator()(TContainer &, TArray &) const= 0;
  
  /**
   * Defines the operation for a slice of the container.
   */
  virtual ScalarType operator()(std::slice &, TContainer &, TArray &) const= 0;
};

/**
 * \class InnerProduct
 * Inner product operation between a scalar itkNeighborhood and an array of
 * coefficients.
 */
template<class TContainer, class TArray>
struct ITK_EXPORT InnerProduct : public ScalarOperation<TContainer, TArray>
{
  typedef ScalarOperation<TContainer, TArray> Superclass;
  typedef typename Superclass::ScalarType ScalarType;
  virtual ScalarType operator()(TContainer &, TArray &) const;
  virtual ScalarType operator()(std::slice &, TContainer &, TArray &) const;
};

/**
 * \class VectorComponentInnerProduct
 *
 * This object allows vector-valued data composed of scalar elements to be
 * treated as scalar data in an inner product operation.  The component used
 * as the scalar data ("visible" by the algorithm) is specified by the
 * m_VisibleComponent member variable. 
 */
template<class TContainer, class TArray>
struct ITK_EXPORT VectorComponentInnerProduct
  : public ScalarOperation<TContainer, TArray>
{
  typedef ScalarOperation<TContainer, TArray> Superclass;
  typedef typename Superclass::ScalarType ScalarType;
  typedef typename TContainer::PixelType VectorType;
  enum { VectorDimension = VectorType::VectorDimension };

  /**
   * Constructor initializes the "visible" component of the vector-valued data
   * to element 0.
   */
  VectorComponentInnerProduct() : m_VisibleComponent(0) {}
  
  virtual ScalarType operator()(TContainer &, TArray &) const;
  virtual ScalarType operator()(std::slice &, TContainer &, TArray &)  const;

  /**
   * The element number of the vector-valued data that is visible to the
   * algorithm.
   */
  unsigned int m_VisibleComponent;
  
  /**
   * Sets the visible component of the vector-valued data.
   */
  void SetVisibleComponent(unsigned int v) { m_VisibleComponent = v; }
};

/**
 * \class IteratorInnerProduct
 *
 * Defines the inner product between pixels in an image neighborhood
 * and an array of coefficients directly on the itkNeighborhoodIterator
 * that references the image neighborhood.  This increases efficiency in
 * algorithms that iterate neighborhoods over many pixels in an image.
 *
 * This algorithm does no bounds checking.
 */
template<class TIterator, class TArray>
struct ITK_EXPORT IteratorInnerProduct : public ScalarOperation<TIterator, TArray>
{

  typedef ScalarOperation<TIterator, TArray> Superclass;
  typedef typename Superclass::ScalarType ScalarType;
  virtual ScalarType operator()(TIterator &, TArray &) const;
  virtual ScalarType operator()(std::slice &, TIterator &, TArray &) const;
};

/**
 * \class BoundsCheckingIteratorInnerProduct
 *
 * Defines the inner product between pixels in an image neighborhood
 * and an array of coefficients directly on the itkNeighborhoodIterator
 * that references the image neighborhood.  This increases efficiency in
 * algorithms that iterate neighborhoods over many pixels in an image.
 *
 * This algorithm performs bounds checking by calling InBounds() on the
 * iterator.  Boundary conditions are resolved by dereferencing the iterator
 * before performing the inner product operation.  It is the iterator itself
 * that determines how to handle the boundary conditions.  The iterator must
 * support calls to
 *   bool InBounds()
 * and
 *   Neighborhood GetNeighborhood()
 *
 */
template<class TIterator, class TArray>
struct ITK_EXPORT BoundsCheckingIteratorInnerProduct
  : public ScalarOperation<TIterator, TArray>
{
  typedef ScalarOperation<TIterator, TArray> Superclass;
  typedef typename Superclass::ScalarType ScalarType;
  typedef typename TIterator::ImageType ImageType;
  typedef typename ImageType::PixelType PixelType;
  enum { Dimension = ImageType::ImageDimension };
  
  virtual ScalarType operator()(TIterator &, TArray &) const;
  virtual ScalarType operator()(std::slice &, TIterator &, TArray &) const;
};

/**
 * \class VectorComponentIteratorInnerProduct
 * 
 * This object allows vector-valued data composed of scalar elements to be
 * treated as scalar data in an inner product operation.  The component used
 * as the scalar data ("visible" by the algorithm) is specified by the
 * m_VisibleComponent member variable.
 
 * Defines the inner product directly on the itkNeighborhoodIterator
 * that references the image neighborhood.  This increases efficiency in
 * algorithms that iterate neighborhoods over many pixels in an image.
 *
 */
template<class TIterator, class TArray>
struct ITK_EXPORT VectorComponentIteratorInnerProduct
  : public ScalarOperation<TIterator, TArray>
{
  typedef ScalarOperation<TIterator, TArray> Superclass;
  typedef typename Superclass::ScalarType ScalarType;
  typedef typename TIterator::ImageType ImageType;
  typedef typename ImageType::PixelType VectorType;
  enum { VectorDimension = VectorType::VectorDimension };

  VectorComponentIteratorInnerProduct() : m_VisibleComponent(0) {}
  
  virtual ScalarType operator()(TIterator &, TArray &) const;
  virtual ScalarType operator()(std::slice &, TIterator &, TArray &) const;

  unsigned int m_VisibleComponent;
  void SetVisibleComponent(unsigned int v) { m_VisibleComponent = v; }
};

/**
 * \class BoundsCheckingVectorComponentIteratorInnerProduct
 *
 * This object allows vector-valued data composed of scalar elements to be
 * treated as scalar data in an inner product operation.  The component used
 * as the scalar data ("visible" by the algorithm) is specified by the
 * m_VisibleComponent member variable.
 *
 * Defines the inner product directly on the itkNeighborhoodIterator
 * that references the image neighborhood.  This increases efficiency in
 * algorithms that iterate neighborhoods over many pixels in an image.

 * This algorithm performs bounds checking by calling InBounds() on the
 * iterator.  Boundary conditions are resolved by dereferencing the iterator
 * before performing the inner product operation.  It is the iterator itself
 * that determines how to handle the boundary conditions.  The iterator must
 * support calls to
 *   bool InBounds()
 * and
 *   Neighborhood GetNeighborhood()
 */
template<class TIterator, class TArray>
struct ITK_EXPORT BoundsCheckingVectorComponentIteratorInnerProduct
  : public VectorComponentIteratorInnerProduct<TIterator, TArray>
{
  typedef ScalarOperation<TIterator, TArray> Superclass;
  typedef typename Superclass::ScalarType ScalarType;
  typedef typename TIterator::ImageType ImageType;
  enum { Dimension = ImageType::ImageDimension };

  virtual ScalarType operator()(TIterator &, TArray &) const;
  virtual ScalarType operator()(std::slice &, TIterator &, TArray &) const;
};

/**
 * \class ApplyOperatorToEach
 *
 * A function object that applies a vector of coefficients to an image
 * neighborhood iterator for every pixel in that iterator's iteration region.
 * Templated over operation (method of applying the coefficients) and iterator
 * type. 
 */
template<class TOperation, class TIterator>
struct ITK_EXPORT ApplyOperatorToEach
{
  typedef typename TIterator::ScalarValueType ScalarType;
  typedef typename TIterator::ImageType ImageType;
  typedef typename ImageType::PixelType PixelType;
  typedef typename ImageType::ScalarValueType ScalarValueType;
  enum { ImageDimension = ImageType::ImageDimension };

  void operator()(ImageType *, ImageType *,
                  Neighborhood<ScalarValueType, ImageDimension> &) const;
};

/**
 * \class CalculateOutputWrapOffsetModifiers
 * 
 * Helper class for setting up itkNeighborhoodIterator output
 * buffers. Calculates the necessary modifiers to synchronize input and output
 * iteration between images with equal RequestedRegion sizes but unequal
 * BufferedRegion sizes.
 * 
 */
template<class TImage>
struct ITK_EXPORT CalculateOutputWrapOffsetModifiers
{
  typedef Offset<TImage::ImageDimension> OffsetType;
  OffsetType operator()(TImage *, TImage *) const;
};
  
} // end namespace NeighborhoodAlgorithm
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNeighborhoodAlgorithm.txx"
#endif

#endif
