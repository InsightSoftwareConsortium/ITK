/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSingleValuedNonLinearVnlOptimizer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkSingleValuedNonLinearVnlOptimizer_h
#define __itkSingleValuedNonLinearVnlOptimizer_h

#include "itkSingleValuedNonLinearOptimizer.h"
#include "vnl/vnl_cost_function.h"


namespace itk
{
  
/** \class SingleValuedNonLinearVnlOptimizer
 * \brief This class is a base for the Optimization methods that 
 * optimize a single valued function.
 *
 */

  
template <class TCostFunction>
class ITK_EXPORT SingleValuedNonLinearVnlOptimizer : 
          public SingleValuedNonLinearOptimizer<TCostFunction> 

{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef SingleValuedNonLinearVnlOptimizer  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef   SingleValuedNonLinearOptimizer<TCostFunction> Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Dimension of the Search Space
   */
  enum { SpaceDimension = TCostFunction::SpaceDimension };
    
 /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro( SingleValuedNonLinearVnlOptimizer, 
                SingleValueNonLinearOptimizer );

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
  /**
   * InternalParametersType typedef.
   */
  typedef   vnl_vector<double>     InternalParametersType;


  /**
   * InternalMeasureType typedef.
   */
  typedef   double                 InternalMeasureType;


  /**
   * InternalGradientType typedef.
   */
  typedef   vnl_vector<double>     InternalDerivativeType;


  /**
   * ParametersType typedef.
   */
  typedef typename TCostFunction::ParametersType    ParametersType;


  /**
   * MeasureType typedef.
   */
  typedef typename TCostFunction::MeasureType         MeasureType;


  /**
   * GradientType typedef.
   */
  typedef typename TCostFunction::DerivativeType      DerivativeType;


  /** \class VnlCostFunction
   * \brief Adaptor between the CostFunction and the vnl_cost_function classes
   *
   */

  class VnlCostFunctionAdaptor : public vnl_cost_function
  {
  public:
    VnlCostFunctionAdaptor():vnl_cost_function(SpaceDimension) 
      { m_CostFunction = 0; }    

      void SetCostFunction( TCostFunction * costFunction ) 
        { m_CostFunction = costFunction; }
      

      /** 
       *  Delegate computation of the value to the CostFunction
       */
      virtual InternalMeasureType f( const InternalParametersType & inparameters ) {
        if( !m_CostFunction )
        {
          throw ExceptionObject();
        }
        ParametersType parameters;
        ConvertParameters( inparameters, parameters );
        const InternalMeasureType value = 
          (InternalMeasureType)m_CostFunction->GetValue( parameters );
        return value;
      }
      
      /** 
       *  Delegate computation of the gradient to the CostFunction
       */
      virtual void gradf(const InternalParametersType   & inparameters,
                               InternalDerivativeType   & gradient ) {
        if( !m_CostFunction )
        {
          throw ExceptionObject();
        }
        ParametersType parameters;
        ConvertParameters( inparameters, parameters );
        typename DerivativeType::Pointer externalGradient = 
                      m_CostFunction->GetDerivative( parameters );
        ConvertGradient( externalGradient, gradient);
      }
      
      /** 
       *  Delegate computation of value and gradient to the CostFunction
       */
      virtual void compute(const InternalParametersType   & x,
                                 InternalMeasureType      * f, 
                                 InternalDerivativeType   * g   ) {
        // delegate the computation to the CostFunction

        ParametersType parameters;
        ConvertParameters( x, parameters );

        *f = (InternalMeasureType)m_CostFunction->GetValue( parameters );

        typename DerivativeType::Pointer externalGradient = 
                                      m_CostFunction->GetDerivative( parameters );

        ConvertGradient( externalGradient, *g );    
      }
 
      /**
       *  Convert internal Parameters (vnl_Vector) 
       *  into VectorContainer type
       */
      static void ConvertParameters( const InternalParametersType & input,
                                           ParametersType         & output )
      {
        for( unsigned int i=0; i<SpaceDimension; i++)
        {
          output[i] = input[i]; 
        }
      }

      /**
       *  Convert external Parameters VectorContainer 
       *  into internal type (vnl_Vector)
       */
      static void ConvertParameters(const  ParametersType         & input,
                                           InternalParametersType & output )
      {
        for( unsigned int i=0; i<SpaceDimension; i++ ) 
        {
          output[i] = input[i];
        }
      }
      
      /**
       *  Convert external derviative measures (VectorContainer) 
       *  into internal type (vnl_Vector)
       */
      void ConvertGradient(const DerivativeType & input,
                           InternalDerivativeType & output )
      {
        for( unsigned int i=0; i<SpaceDimension; i++ ) 
        {
          output[i] = input[i];
        }
      }

  private:

    typename TCostFunction::Pointer   m_CostFunction;

  };  // end of Class CostFunction


  /**
   * Set the cost Function of type TCostFunction
   */
  void SetCostFunction( TCostFunction * costFunction ) 
    { m_CostFunctionAdaptor.SetCostFunction( costFunction ); }
    

protected:

  SingleValuedNonLinearVnlOptimizer();
  virtual ~SingleValuedNonLinearVnlOptimizer() {};
  SingleValuedNonLinearVnlOptimizer(const Self&) {}
  void operator=(const Self&) {}

protected:

  VnlCostFunctionAdaptor            m_CostFunctionAdaptor;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSingleValuedNonLinearVnlOptimizer.txx"
#endif

#endif



