/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConjugateGradientOptimizer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkConjugateGradientOptimizer_h
#define __itkConjugateGradientOptimizer_h

#include "itkNonLinearOptimizer.h"
#include "vnl/algo/vnl_conjugate_gradient.h"

namespace itk
{
  
/** \class ConjugateGradientOptimizer
 * \brief Wrap of the vnl_conjugate_gradient to be adapted for Registration
 *
 */

template <class TMetric>
class ITK_EXPORT ConjugateGradientOptimizer : 
    public NonLinearOptimizer<TMetric> 

{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef ConjugateGradientOptimizer  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef   NonLinearOptimizer<TMetric> Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;



 /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro( ConjugateGradientOptimizer, 
      NonLinearOptimizer );


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  

  /**
   * Method for getting access to the internal optimizer
   */
  vnl_conjugate_gradient & GetOptimizer(void);


protected:

  ConjugateGradientOptimizer();
  virtual ~ConjugateGradientOptimizer() {};
  ConjugateGradientOptimizer(const Self&) {}
  void operator=(const Self&) {}

  vnl_conjugate_gradient     m_ConjugateGradient;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkConjugateGradientOptimizer.txx"
#endif

#endif



