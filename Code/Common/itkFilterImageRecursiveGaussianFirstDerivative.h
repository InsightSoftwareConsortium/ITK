/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFilterImageRecursiveGaussianFirstDerivative.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkFilterImageRecursiveGaussianFirstDerivative_h
#define __itkFilterImageRecursiveGaussianFirstDerivative_h

#include "itkFilterImageRecursiveGaussian.h"

namespace itk
{
  
/** \class FilterImageRecursiveGaussianFirstDerivative
 * \brief Convolve an image with a kernel approximating the
 *        first derivative of a Gaussian.
 *
 * itkFilterImageRecursiveGaussianFirstDerivative convolves an image with a kernel
 * approximating the first derivative of a Gaussian.  This class implements
 * the recursive filtering method proposed by R.Deriche in IEEE-PAMI Vol.12,
 * No.1, January 1990, pp 78-87.  
 */
template <class TInputImage, class TOutputImage, class TComputation>
class ITK_EXPORT FilterImageRecursiveGaussianFirstDerivative:
    public FilterImageRecursiveGaussian<TInputImage,TOutputImage,TComputation>
{

public:
  
  /**
   * Standard "Self" typedef.
   */
  typedef FilterImageRecursiveGaussianFirstDerivative  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef FilterImageRecursiveGaussian<TInputImage,TOutputImage,TComputation> 
          Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>                  Pointer;
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);  
  
protected:

  FilterImageRecursiveGaussianFirstDerivative() {};
  
  virtual ~FilterImageRecursiveGaussianFirstDerivative() {};  
  
  FilterImageRecursiveGaussianFirstDerivative(const Self&) {}

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
#include "itkFilterImageRecursiveGaussianFirstDerivative.txx"
#endif

#endif




