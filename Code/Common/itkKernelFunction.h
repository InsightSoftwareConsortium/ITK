/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKernelFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/
#ifndef _itkKernelFunction_h
#define _itkKernelFunction_h

#include "vnl/vnl_math.h"

namespace itk
{

/**
 * \class KernelFunction
 * \brief Kernel used for kernel function/density estimation.
 */
class ITK_EXPORT KernelFunction : public Object
{
public:  
  /**
   * Standard "Self" typedef.
   */
  typedef KernelFunction Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef Object Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;

  /**
   * Evaluate the function.
   */
  virtual double Evaluate (const double u) = 0;

protected:  
  KernelFunction(){};  
  ~KernelFunction(){};

};

/**
 * \class GaussianKernelFunction
 * \brief Gaussian kernel used for kernel function/density estimation.
 */
class ITK_EXPORT GaussianKernelFunction : public KernelFunction
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef GaussianKernelFunction Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef KernelFunction Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);  

  /**
   * Evaluate the function.
   */
  inline double Evaluate (const double u)
  {
    return ( exp( -0.5 * vnl_math_sqr( u ) ) * m_Factor );
  }

protected:

  GaussianKernelFunction()
  { 
    m_Factor = 1.0 / vnl_math_sqrt( 2.0 * vnl_math::pi ); 
  };

  ~GaussianKernelFunction(){};

private:
  double m_Factor;

};

} // namespace itk

#endif