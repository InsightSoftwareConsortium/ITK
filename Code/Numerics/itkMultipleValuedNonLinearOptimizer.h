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
class ITK_EXPORT MultipleValuedNonLinearOptimizer : public NonLinearOptimizer 

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
   * VectorType typedef.
   */
  typedef   vnl_vector<double>     VectorType;

 /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro( MultipleValuedNonLinearOptimizer, 
      NonLinearOptimizer );


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  

  /** \class VnlCostFunction
   * \brief Adaptor between the CostFunction and the
   * vnl_least_squares_function classes
   *
   */

  class VnlCostFunctionAdaptor : public vnl_least_squares_function 
  {
  public:
    VnlCostFunctionAdaptor():
        vnl_least_squares_function(TCostFunction::SpaceDimension,
                                   TCostFunction::RangeDimension) 
      { m_CostFunction = 0; }    

      void SetCostFunction( TCostFunction * costFunction ) 
        { m_CostFunction = costFunction; }
      

      /** 
       *  Delegate computation of the value to the CostFunction
       */
      virtual void f( const VectorType & parameters, VectorType & output ) {
        if( !m_CostFunction )
        {
          throw ExceptionObject();
        }
        m_CostFunction->GetValue( parameters, output );
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
      }
      
      /** 
       *  Delegate computation of value and gradient to the CostFunction
       */
      virtual void compute(const VectorType & x,
                                 VectorType * f, 
                                 VectorType * g ) {
        // delegate the computation to the CostFunction
        m_CostFunction->GetValue( x, *f );
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

  MultipleValuedNonLinearOptimizer();
  virtual ~MultipleValuedNonLinearOptimizer() {};
  MultipleValuedNonLinearOptimizer(const Self&) {}
  void operator=(const Self&) {}

protected:

  VnlCostFunctionAdaptor            m_CostFunctionAdaptor;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMultipleValuedNonLinearOptimizer.txx"
#endif

#endif



