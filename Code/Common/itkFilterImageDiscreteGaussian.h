/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFilterImageDiscreteGaussian.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkFilterImageDiscreteGaussian_h
#define __itkFilterImageDiscreteGaussian_h

#include "itkFilterImageToImage.h"
#include "itkImage.h"

namespace itk
{
/**
 * \class FilterImageDiscreteGaussian
 * \brief Blurs an image by separable convolution with discrete gaussian kernels.
 * This filter performs Gaussian blurring by separable convolution of an image
 * and a discrete Gaussian operator (kernel).
 *
 * See GaussianOperator for references regarding the theory behind the discrete
 * Gaussian function used in this kerenel.
 *
 * \sa GaussianOperator
 * \sa Image
 * \sa Neighborhood
 * \sa NeighborhoodOperator
 */

template <class TPixel, unsigned int VDimension=2>
class ITK_EXPORT FilterImageDiscreteGaussian :
    public FilterImageToImage< Image<TPixel, VDimension>,
                               Image<TPixel, VDimension> > 
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef FilterImageDiscreteGaussian Self;

  /**
   * Standard super class typedef support.
   */
  typedef FilterImageToImage< Image<TPixel, VDimension>,
    Image<TPixel, VDimension> > Superclass;
  
  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self> Pointer;

  /**
   * Image type typedef support
   */
  typedef Image<TPixel, VDimension> ImageType;
  
  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro(FilterImageDiscreteGaussian, FilterImageToImage);
  
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
  itkSetVectorMacro(Variance, float, VDimension);
  itkGetVectorMacro(Variance, const float, VDimension);
  itkSetVectorMacro(MaximumError, float, VDimension);
  itkGetVectorMacro(MaximumError, const float, VDimension);

  /**
   * Convenience get/set methods for setting all dimensional parameters to the
   * same values.
   */
  void SetVariance(const float v)
  {
    float vArray[VDimension];
    for (int i = 0; i<VDimension; ++i) { vArray[i] = v; }
    this->SetVariance(vArray);
  }
  void SetMaximumError(const float v)
  {
    float vArray[VDimension];
    for (int i = 0; i<VDimension; ++i) { vArray[i] = v; }
    this->SetMaximumError(vArray);
  }
  
protected:
  FilterImageDiscreteGaussian()
  {
    this->SetVariance(0.0f);
    this->SetMaximumError(0.01f);
  }
  virtual ~FilterImageDiscreteGaussian() {}
  FilterImageDiscreteGaussian(const Self&) {}
  void operator=(const Self&) {}

  static void ImageRegionCopy(Image<TPixel, VDimension> *, Image<TPixel,
                              VDimension> *);

private:
  /**
   * The variance of the gaussian blurring kernel in each dimensional direction.
   */
  float m_Variance[VDimension];

  /**
   * The maximum error of the gaussian blurring kernel in each dimensional
   * direction. For definition of maximum error, see GaussianOperator.
   * \sa GaussianOperator
   */
  float m_MaximumError[VDimension];  
};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFilterImageDiscreteGaussian.txx"
#endif

#endif
