/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRecursiveGaussianImageFilterFirstDerivative.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkRecursiveGaussianImageFilterFirstDerivative_h
#define __itkRecursiveGaussianImageFilterFirstDerivative_h

#include "itkRecursiveGaussianImageFilter.h"

namespace itk
{
  
/** \class RecursiveGaussianImageFilterFirstDerivative
 * \brief Convolve an image with a kernel approximating the
 *        first derivative of a Gaussian.
 *
 * itkRecursiveGaussianImageFilterFirstDerivative convolves an image with a kernel
 * approximating the first derivative of a Gaussian.  This class implements
 * the recursive filtering method proposed by R.Deriche in IEEE-PAMI Vol.12,
 * No.1, January 1990, pp 78-87.  
 */
template <class TInputImage, class TOutputImage, class TComputation>
class ITK_EXPORT RecursiveGaussianImageFilterFirstDerivative:
    public RecursiveGaussianImageFilter<TInputImage,TOutputImage,TComputation>
{

public:
  
  /**
   * Standard "Self" typedef.
   */
  typedef RecursiveGaussianImageFilterFirstDerivative  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef RecursiveGaussianImageFilter<TInputImage,TOutputImage,TComputation> 
          Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>                  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);  
  
protected:

  RecursiveGaussianImageFilterFirstDerivative() {};
  
  virtual ~RecursiveGaussianImageFilterFirstDerivative() {};  
  
  RecursiveGaussianImageFilterFirstDerivative(const Self&) {}

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
#include "itkRecursiveGaussianImageFilterFirstDerivative.txx"
#endif

#endif




