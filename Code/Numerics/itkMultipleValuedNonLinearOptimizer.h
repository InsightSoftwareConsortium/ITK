/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultipleValuedNonLinearOptimizer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkMultipleValuedNonLinearOptimizer_h
#define __itkMultipleValuedNonLinearOptimizer_h

#include "itkNonLinearOptimizer.h"
#include "vnl/vnl_least_squares_function.h"
#include "itkExceptionObject.h"

namespace itk
{
  
/** \class MultipleValuedNonLinearOptimizer
 * \brief This class is a base for the Optimization methods that 
 * optimize a single valued function.
 *
 */

  
template <class TCostFunction>
class ITK_EXPORT MultipleValuedNonLinearOptimizer : 
        public NonLinearOptimizer < TCostFunction >
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef MultipleValuedNonLinearOptimizer  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef   NonLinearOptimizer Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;


  /**
   *  Parameters type.
   *  it defines a position in the optimization search space
   */
  typedef typename TCostFunction::ParametersType ParametersType;


  /**
   *  Measure type.
   *  it defines a type used to return the cost function value 
   */
  typedef typename TCostFunction::MeasureType   MeasureType;


  /**
   *  Derivative type.
   *  it defines a type used to return the cost function derivative 
   */
  typedef typename TCostFunction::DerivativeType DerivativeType;


 /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro( MultipleValuedNonLinearOptimizer, 
      NonLinearOptimizer );


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  

protected:

  MultipleValuedNonLinearOptimizer() {};
  virtual ~MultipleValuedNonLinearOptimizer() {};
  MultipleValuedNonLinearOptimizer(const Self&) {}
  void operator=(const Self&) {}

};

} // end namespace itk

#endif



