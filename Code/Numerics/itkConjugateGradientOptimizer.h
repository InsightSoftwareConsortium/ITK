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

#include "itkSingleValuedNonLinearVnlOptimizer.h"
#include "vnl/algo/vnl_conjugate_gradient.h"

namespace itk
{
  
/** \class ConjugateGradientOptimizer
 * \brief Wrap of the vnl_conjugate_gradient 
 *
 */

  
template <class TCostFunction>
class ITK_EXPORT ConjugateGradientOptimizer : 
        public SingleValuedNonLinearVnlOptimizer< TCostFunction >

{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef ConjugateGradientOptimizer  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef SingleValuedNonLinearVnlOptimizer<TCostFunction> Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;


  /**
   * Internal Optimizer Type
   */
  typedef   vnl_conjugate_gradient InternalOptimizerType;


  /**
   *  Parameters type.
   *  it defines a position in the optimization search space
   */
  typedef typename TCostFunction::ParametersType ParametersType;


  /**
   *  Measure type.
   *  it defines a type used to return the cost function value 
   */
  typedef typename TCostFunction::MeasureType MeasureType;


  /**
   *  Derivative type.
   *  it defines a type used to return the cost function derivative 
   */
  typedef typename TCostFunction::DerivativeType DerivativeType;



 /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro( ConjugateGradientOptimizer, 
      SingleValuedNonLinearOptimizer );


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  

  /**
   * Method for getting access to the internal optimizer
   */
  vnl_conjugate_gradient & GetOptimizer(void);
 

  /**
   * Start optimization with an initial value
   */
  void StartOptimization( void );


protected:

  ConjugateGradientOptimizer();
  virtual ~ConjugateGradientOptimizer() {};
  ConjugateGradientOptimizer(const Self&) {}
  void operator=(const Self&) {}

private:

  /**
   *  The vnl optimization method for Conjugate Gradient
   */
  InternalOptimizerType     m_ConjugateGradient;


};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkConjugateGradientOptimizer.txx"
#endif

#endif



