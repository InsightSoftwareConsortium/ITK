/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFilterImageGradientMagnitude.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkFilterImageGradientMagnitude_h
#define __itkFilterImageGradientMagnitude_h

#include "itkFilterImageToImage.h"
#include "itkNeighborhoodIterator.h"
#include "itkImage.h"

namespace itk
{
/**
 * \class FilterImageGradientMagnitude
 * \brief Computes the gradient magnitude of an image region at each pixel.
 *
 * \sa Image
 * \sa Neighborhood
 * \sa NeighborhoodOperator
 * \sa NeighborhoodIterator
 */

template <class TPixel, unsigned int VDimension=2>
class ITK_EXPORT FilterImageGradientMagnitude :
    public FilterImageToImage< Image<TPixel, VDimension>,
                               Image<TPixel, VDimension> > 
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef FilterImageGradientMagnitude Self;

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
  itkTypeMacro(FilterImageGradientMagnitude, FilterImageToImage);
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Standard pipeline method.
   */
  void GenerateData();

protected:
  FilterImageGradientMagnitude() {}
  virtual ~FilterImageGradientMagnitude() {}
  FilterImageGradientMagnitude(const Self&) {}
  void operator=(const Self&) {}

  /**
   * Computes the gradient magnitude by convolving along each axial direction
   * with a first-order derivative operator.
   */
  void GradientMagnitude(NeighborhoodIterator<TPixel, VDimension>*,
                         ImageType*, ImageType*, bool);
};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFilterImageGradientMagnitude.txx"
#endif

#endif
