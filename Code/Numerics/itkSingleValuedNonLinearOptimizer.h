/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSingleValuedNonLinearOptimizer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkSingleValuedNonLinearOptimizer_h
#define __itkSingleValuedNonLinearOptimizer_h

#include "itkNonLinearOptimizer.h"


namespace itk
{
  
/** \class SingleValuedNonLinearOptimizer
 * \brief This class is a base for the Optimization methods that 
 * optimize a single valued function.
 *
 */

  
template <class TCostFunction>
class ITK_EXPORT SingleValuedNonLinearOptimizer : 
    public NonLinearOptimizer< TCostFunction >

{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef SingleValuedNonLinearOptimizer  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef NonLinearOptimizer< TCostFunction > Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro( SingleValuedNonLinearOptimizer, 
      NonLinearOptimizer );

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  

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


 
protected:


  SingleValuedNonLinearOptimizer() {};
  virtual ~SingleValuedNonLinearOptimizer() {}
  SingleValuedNonLinearOptimizer(const Self&) {}
  void operator=(const Self&) {}
 
};

} // end namespace itk


#endif



