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
#include "vnl/vnl_cost_function.h"
#include "itkExceptionObject.h"

namespace itk
{
  
/** \class SingleValuedNonLinearOptimizer
 * \brief This class is a base for the Optimization methods that 
 * optimize a single valued function.
 *
 */

  
template <class TCostFunction>
class ITK_EXPORT SingleValuedNonLinearOptimizer : public NonLinearOptimizer 

{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef SingleValuedNonLinearOptimizer  Self;

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
   * Run-time type information (and related methods).
   */
  itkTypeMacro( SingleValuedNonLinearOptimizer, 
      NonLinearOptimizer );

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
  /**
   * VectorType typedef.
   */
  typedef   vnl_vector<double>     VectorType;

  /** \class VnlCostFunction
   * \brief Adaptor between the CostFunction and the vnl_cost_function classes
   *
   */

  class VnlCostFunctionAdaptor : public vnl_cost_function
  {
  public:
    VnlCostFunctionAdaptor():vnl_cost_function(TCostFunction::SpaceDimension) 
      { m_CostFunction = 0; }    

      void SetCostFunction( TCostFunction * costFunction ) 
        { m_CostFunction = costFunction; }
      

      /** 
       *  Delegate computation of the value to the CostFunction
       */
      virtual double f( const VectorType & parameters ) {
        if( !m_CostFunction )
        {
          throw ExceptionObject();
        }
        return m_CostFunction->GetValue( parameters );
      }
      
      /** 
       *  Delegate computation of the gradient to the CostFunction
       */
      virtual void gradf(const VectorType & parameters,
                               VectorType & gradient ) {
        if( !m_CostFunction )
        {
          throw ExceptionObject();
        }
        gradient = m_CostFunction->GetDerivative( parameters );
      }
      
      /** 
       *  Delegate computation of value and gradient to the CostFunction
       */
      virtual void compute(const VectorType & x,
                                 double * f, 
                                 VectorType * g ) {
        // delegate the computation to the CostFunction
        *f = m_CostFunction->GetValue( x );
        *g = m_CostFunction->GetDerivative( x );
      }
 
  private:
      TCostFunction   * m_CostFunction;
  };  // end of Class CostFunction


  /**
   * Set the cost Function of type TCostFunction
   */
  void SetCostFunction( TCostFunction * costFunction ) 
    { m_CostFunctionAdaptor.SetCostFunction( costFunction ); }
    
  

protected:

  SingleValuedNonLinearOptimizer();
  virtual ~SingleValuedNonLinearOptimizer() {}
  SingleValuedNonLinearOptimizer(const Self&) {}
  void operator=(const Self&) {}

protected:

  VnlCostFunctionAdaptor            m_CostFunctionAdaptor;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSingleValuedNonLinearOptimizer.txx"
#endif

#endif



