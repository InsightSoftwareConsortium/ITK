/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFilterImageDerivative.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkFilterImageDerivative_h
#define __itkFilterImageDerivative_h

#include "itkFilterImageToImage.h"
#include "itkImage.h"

namespace itk
{
/**
 * \class FilterImageDerivative
 * \brief Computes the directional derivative of an image.
 * The directional derivative at each pixel location is computed by convolution
 * with a first-order derivative operator.
 *
 * \sa Image
 * \sa Neighborhood
 * \sa NeighborhoodOperator
 * \sa NeighborhoodIterator
 */

template <class TPixel, unsigned int VDimension=2>
class ITK_EXPORT FilterImageDerivative :
    public FilterImageToImage< Image<TPixel, VDimension>,
                               Image<TPixel, VDimension> > 
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef FilterImageDerivative Self;

  /**
   * Standard super class typedef support.
   */
  typedef FilterImageToImage< Image<TPixel, VDimension>,
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
  itkTypeMacro(FilterImageDerivative, FilterImageToImage);
  
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
  FilterImageDerivative() {}
  virtual ~FilterImageDerivative() {}
  FilterImageDerivative(const Self&) {}
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
#include "itkFilterImageDerivative.txx"
#endif

#endif
