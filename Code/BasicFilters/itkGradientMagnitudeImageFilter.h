/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradientMagnitudeImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkGradientMagnitudeImageFilter_h
#define __itkGradientMagnitudeImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkNeighborhoodIterator.h"
#include "itkImage.h"

namespace itk
{

template<class TInnerProduct, class TIterator>
struct GradientMagnitudeStrategy
{
  typedef typename TIterator::ImageType ImageType;
  typedef typename ImageType::PixelType PixelType;
  typedef typename ImageType::ScalarValueType ScalarValueType;
  enum {ImageDimension = ImageType::ImageDimension };
  
  GradientMagnitudeStrategy() {}
  void operator()(ImageType *, ImageType *) const;
};

/**
 * \class GradientMagnitudeImageFilter
 * \brief Computes the gradient magnitude of an image region at each pixel.
 *
 * \sa Image
 * \sa Neighborhood
 * \sa NeighborhoodOperator
 * \sa NeighborhoodIterator
 */

template <class TPixel, unsigned int VDimension=2>
class ITK_EXPORT GradientMagnitudeImageFilter :
    public ImageToImageFilter< Image<TPixel, VDimension>,
                               Image<TPixel, VDimension> > 
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef GradientMagnitudeImageFilter Self;

  /**
   * Standard super class typedef support.
   */
  typedef ImageToImageFilter< Image<TPixel, VDimension>,
    Image<TPixel, VDimension> > Superclass;
  
  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Image type typedef support
   */
  typedef Image<TPixel, VDimension> ImageType;
  
  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro(GradientMagnitudeImageFilter, ImageToImageFilter);
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Standard pipeline method.
   */
  void GenerateData();

protected:
  GradientMagnitudeImageFilter() {}
  virtual ~GradientMagnitudeImageFilter() {}
  GradientMagnitudeImageFilter(const Self&) {}
  void operator=(const Self&) {}
};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGradientMagnitudeImageFilter.txx"
#endif

#endif
