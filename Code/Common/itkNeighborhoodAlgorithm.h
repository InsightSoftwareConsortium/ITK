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
#include "itkNeighborhoodOperator.h"
#include "itkExceptionObject.h"

namespace itk
{
  
namespace NeighborhoodAlgorithm
{      

template<class TContainer>
struct ScalarOperation
{
  typedef typename TContainer::PixelType InternalType;
  typedef typename TContainer::ScalarValueType ScalarType;

  /**
   *
   */
  virtual ScalarType operator()(std::slice &, TContainer &, 
                                     std::valarray<ScalarType>&) const= 0;
  
  /**
   *
   */
  virtual ScalarType operator()(TContainer &,
                                     std::valarray<ScalarType>&) const= 0;
};


/**
 *
 */
template<class TContainer>
struct InnerProduct : public ScalarOperation<TContainer>
{
  virtual ScalarType operator()(TContainer &,
                                     std::valarray<ScalarType>& ) const;
  virtual ScalarType operator()(std::slice &, TContainer &,
                                     std::valarray<ScalarType>&)  const;
};

/**
 *
 */
template<class TContainer>
struct VectorComponentInnerProduct
  : public ScalarOperation<TContainer>
{
  typedef typename TContainer::PixelType VectorType;
  enum { VectorDimension = VectorType::VectorDimension };

  VectorComponentInnerProduct() : m_VisibleComponent(0) {}
  
  virtual ScalarType operator()(TContainer &,
                                     std::valarray<ScalarType>& ) const;
  virtual ScalarType operator()(std::slice &, TContainer &,
                                     std::valarray<ScalarType>&)  const;

  unsigned int m_VisibleComponent;
  void SetVisibleComponent(unsigned int v) { m_VisibleComponent = v; }
};

/**
 *
 */
template<class TIterator>
struct IteratorInnerProduct : public ScalarOperation<TIterator>
{
  virtual ScalarType operator()(TIterator &,
                                     std::valarray<ScalarType>& ) const;
  virtual ScalarType operator()(std::slice &, TIterator &,
                                     std::valarray<ScalarType>&)  const;
};

/**

 */
template<class TIterator>
struct BoundsCheckingIteratorInnerProduct
  : public ScalarOperation<TIterator>
{
  typedef typename TIterator::ImageType ImageType;
  typedef typename ImageType::PixelType PixelType;
  enum { Dimension = ImageType::ImageDimension };
  
  virtual ScalarType operator()(TIterator &,
                                     std::valarray<ScalarType>& ) const;
  virtual ScalarType operator()(std::slice &, TIterator &,
                                     std::valarray<ScalarType>&)  const;
};

template<class TIterator>
struct VectorComponentIteratorInnerProduct
  : public ScalarOperation<TIterator>
{
  typedef typename TIterator::ImageType ImageType;
  typedef typename ImageType::PixelType VectorType;
  enum { VectorDimension = VectorType::VectorDimension };

  VectorComponentIteratorInnerProduct() : m_VisibleComponent(0) {}
  
  virtual ScalarType operator()(TIterator &,
                                     std::valarray<ScalarType>& ) const;
  virtual ScalarType operator()(std::slice &, TIterator &,
                                     std::valarray<ScalarType>&)  const;

  unsigned int m_VisibleComponent;
  void SetVisibleComponent(unsigned int v) { m_VisibleComponent = v; }
};

template<class TIterator>
struct BoundsCheckingVectorComponentIteratorInnerProduct
  : public VectorComponentIteratorInnerProduct<TIterator>
{
  enum { Dimension = ImageType::ImageDimension };

  virtual ScalarType operator()(TIterator &,
                                     std::valarray<ScalarType>& ) const;
  virtual ScalarType operator()(std::slice &, TIterator &,
                                     std::valarray<ScalarType>&)  const;
};

template<class TOperation, class TIterator>
struct ApplyOperatorToEach
{
  typedef typename TIterator::ImageType ImageType;
  typedef typename ImageType::PixelType PixelType;
  typedef typename ImageType::ScalarValueType ScalarValueType;
  enum { ImageDimension = ImageType::ImageDimension };

  void operator()(ImageType *, ImageType *,
                  Neighborhood<ScalarValueType, ImageDimension> &) const;
};


template<class TImage>
struct CalculateOutputWrapOffsetModifiers
{
  inline void operator()(long int *, TImage *, TImage *) const;
};
  
} // end namespace NeighborhoodAlgorithm
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNeighborhoodAlgorithm.txx"
#endif

#endif
