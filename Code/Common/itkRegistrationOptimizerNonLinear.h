/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegistrationOptimizerNonLinear.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkRegistrationOptimizerNonLinear_h
#define __itkRegistrationOptimizerNonLinear_h

#include "itkRegistrationOptimizer.h"
#include "vnl/vnl_cost_function.h"

namespace itk
{
  
/** \class RegistrationOptimizerNonLinear
 * \brief Wrap of the vnl_nonlinear_minimizer to be adapted for Registration
 *
 */

template <class TMetric>
class ITK_EXPORT RegistrationOptimizerNonLinear : 
    public RegistrationOptimizer<TMetric> 

{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef RegistrationOptimizerNonLinear  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef   RegistrationOptimizer<TMetric> Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;



 /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro( RegistrationOptimizerNonLinear, 
      RegistrationOptimizer );


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  

  /**
   * Method for setting the Metric
   */
  void SetMetric( TMetric * metric );
  

  /**
   * Adapter for the Metric to be used as a cost function
   */
  class MetricCostFunction : public vnl_cost_function {
  public:
    MetricCostFunction() {};
    virtual ~MetricCostFunction() {};
    
    virtual double f( const vnl_vector<double> & parameters ) {
      return m_Metric->GetMatchMeasure( parameters );
    }
    
    virtual void gradf(const vnl_vector<double> & parameters,
                             vnl_vector<double> & gradient ) {
      m_Metric->GetMatchMeasureDerivatives();
    }
    
    virtual void compute(const vnl_vector<double> & x,
                               double * f, 
                               vnl_vector<double> * g ) {
      // delegate the computation to the Metric
      m_Metric->GetMatchMeasure( parameters );
      m_Metric->GetMatchMeasureDerivatives();
    }
    
    void SetMetric( TMetric * metric ) {
      m_Metric = metric;
    }
    
  private :
      MetricPointer   m_Metric;
      
  };

protected:

  RegistrationOptimizerNonLinear();
  virtual ~RegistrationOptimizerNonLinear() {};
  RegistrationOptimizerNonLinear(const Self&) {}
  void operator=(const Self&) {}

  MetricCostFunction          m_MetricCostFunction;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRegistrationOptimizerNonLinear.txx"
#endif

#endif



