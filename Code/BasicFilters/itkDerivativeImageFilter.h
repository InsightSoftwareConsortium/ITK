/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDerivativeImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkDerivativeImageFilter_h
#define __itkDerivativeImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImage.h"

namespace itk
{
/**
 * \class DerivativeImageFilter
 * \brief Computes the directional derivative of an image.
 * The directional derivative at each pixel location is computed by convolution
 * with a first-order derivative operator.
 *
 * \sa Image
 * \sa Neighborhood
 * \sa NeighborhoodOperator
 * \sa NeighborhoodIterator
 */

template <class TInputImage, class TOutputImage>
class ITK_EXPORT DerivativeImageFilter :
    public ImageToImageFilter< TInputImage, TOutputImage > 
{
public:
  /**
   * Standard "Self" & Superclass typedef.
   */
  typedef DerivativeImageFilter Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;

  /**
   * Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same.
   */
  typedef typename TOutputImage::PixelType OutputPixelType;
  typedef typename TOutputImage::InternalPixelType OutputInternalPixelType;
  typedef typename TInputImage::PixelType InputPixelType;
  typedef typename TInputImage::InternalPixelType InputInternalPixelType;
  enum { ImageDimension = TOutputImage::ImageDimension };
  
  /**
   * Image typedef support
   */
  typedef TInputImage  InputImageType;
  typedef TOutputImage OutputImageType;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro(DerivativeImageFilter, ImageToImageFilter);
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Standard pipeline method.
   */
  void GenerateData();

  /**
   * Standard get/set macros for filter parameters.
   */
  itkSetMacro(Order, unsigned int);
  itkGetMacro(Order, unsigned int);
  itkSetMacro(Direction, unsigned int);
  itkGetMacro(Direction, unsigned int);
  
protected:
  DerivativeImageFilter() {}
  virtual ~DerivativeImageFilter() {}
  DerivativeImageFilter(const Self&) {}
  void operator=(const Self&) {}

private:
  /**
   * The order of the derivative.
   */
  unsigned int m_Order;

  /**
   * The direction of the derivative.
   */
  unsigned int m_Direction;

};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDerivativeImageFilter.txx"
#endif

#endif
