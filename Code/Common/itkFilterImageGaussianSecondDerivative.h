/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFilterImageGaussianSecondDerivative.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkFilterImageGaussianSecondDerivative_h
#define __itkFilterImageGaussianSecondDerivative_h

#include "itkFilterImageGaussian.h"

namespace itk
{

/** \class FilterImageGaussianSecondDerivative
 * \brief Convolve an image with a kernel approximating the
 *        first derivative of a Gaussian.
 *
 * FilterImageGaussianSecondDerivative convolves an image with a kernel
 * approximating the second derivative of a Gaussian.  This class implements
 * the recursive filtering method proposed by R.Deriche in IEEE-PAMI Vol.12,
 * No.1, January 1990, pp 78-87.  
 */
template <class TInputImage, class TOutputImage, class TComputation>
class ITK_EXPORT FilterImageGaussianSecondDerivative:
  public FilterImageGaussian<TInputImage,TOutputImage,TComputation>
{

public:

  /**
   * Standard "Self" typedef.
   */
  typedef FilterImageGaussianSecondDerivative  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef FilterImageGaussian<TInputImage,TOutputImage,TComputation> 
          Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>                   Pointer;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

protected:

  FilterImageGaussianSecondDerivative() {};
  
  virtual ~FilterImageGaussianSecondDerivative() {};
  
  FilterImageGaussianSecondDerivative(const Self&) {}
  
  void operator=(const Self&) {}
  

  /**
   * Set up the coefficients of the filter to approximate a specific kernel.
   * typically it can be used to approximate a Gaussian or one of its
   * derivatives.
   */
  virtual void SetUp(void);
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFilterImageGaussianSecondDerivative.txx"
#endif

#endif




