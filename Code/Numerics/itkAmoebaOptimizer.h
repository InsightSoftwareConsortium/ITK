/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAmoebaOptimizer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkAmoebaOptimizer_h
#define __itkAmoebaOptimizer_h

#include "itkSingleValuedNonLinearVnlOptimizer.h"
#include "vnl/algo/vnl_amoeba.h"
#include "vnl/vnl_cost_function.h"
#include "itkExceptionObject.h"




namespace itk
{
  
/** \class AmoebaOptimizer
 * \brief Wrap of the vnl_amoeba algorithm
 *
 */

template <class TCostFunction>
class ITK_EXPORT AmoebaOptimizer : 
    public SingleValuedNonLinearVnlOptimizer<TCostFunction>

{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef AmoebaOptimizer  Self;

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
   * Run-time type information (and related methods).
   */
  itkTypeMacro( AmoebaOptimizer, 
      NonLinearOptimizer );


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * InternalParameters typedef.
   */
  typedef   vnl_vector<double>     InternalParametersType;


  /**
   * InternalMeasure typedef.
   */
  typedef   vnl_vector<double>     InternalMeasureType;


  /**
   * InternalGradient typedef.
   */
  typedef   vnl_matrix<double>     InternalDerivativeType;


  /**
   *  ParametersType typedef.
   *  it defines a position in the optimization search space
   */
  typedef typename TCostFunction::ParametersType    ParametersType;


  /**
   *  MeasureType typedef.
   *  it defines a type used to return the cost function value 
   */
  typedef typename TCostFunction::MeasureType         MeasureType;


  /**
   *  GradientType typedef.
   *  it defines a type used to return the cost function derivative 
   */
  typedef typename TCostFunction::DerivativeType      DerivativeType;


  /**
   * Internal Optimizer Type
   */
  typedef   vnl_amoeba     InternalOptimizerType;


  /**
   * Vnl Cost Function Adaptor Type
   */
  typedef typename Superclass::VnlCostFunctionAdaptor     
                                   VnlCostFunctionAdaptorType;

  /**
   * Method for getting access to the internal optimizer
   */
  vnl_amoeba & GetOptimizer(void);


  /**
   * Start optimization with an initial value
   */
  void StartOptimization( void );
  

protected:

  AmoebaOptimizer();
  virtual ~AmoebaOptimizer() {};
  AmoebaOptimizer(const Self&) {}
  void operator=(const Self&) {}

  InternalOptimizerType             m_Amoeba;

  VnlCostFunctionAdaptorType        m_CostFunction;


};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkAmoebaOptimizer.txx"
#endif

#endif



