/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSingleValuedNonLinearVnlOptimizer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

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
 * \ingroup Numerics Optimizers
 */
template <class TCostFunction>
class ITK_EXPORT SingleValuedNonLinearVnlOptimizer : 
          public SingleValuedNonLinearOptimizer<TCostFunction> 
{
public:
  /** Standard class typedefs. */
  typedef SingleValuedNonLinearVnlOptimizer  Self;
  typedef   SingleValuedNonLinearOptimizer<TCostFunction> Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro( SingleValuedNonLinearVnlOptimizer, 
                SingleValueNonLinearOptimizer );

  /** Dimension of the Search Space */
  enum { SpaceDimension = TCostFunction::SpaceDimension };
    
  /** InternalParametersType typedef. */
  typedef   vnl_vector<double>     InternalParametersType;

  /** InternalMeasureType typedef. */
  typedef   double                 InternalMeasureType;

  /** InternalGradientType typedef. */
  typedef   vnl_vector<double>     InternalDerivativeType;

  /**  ParametersType typedef.
   *  It defines a position in the optimization search space. */
  typedef typename TCostFunction::ParametersType    ParametersType;

  /**  MeasureType typedef.
   *  It defines a type used to return the cost function value. */
  typedef typename TCostFunction::MeasureType         MeasureType;

  /**  GradientType typedef.
   *  It defines a type used to return the cost function derivative.  */
  typedef typename TCostFunction::DerivativeType      DerivativeType;

  /** \class VnlCostFunction
   * \brief Adaptor between the CostFunction and the vnl_cost_function classes
   */
  class VnlCostFunctionAdaptor : public vnl_cost_function
  {
  public:
    VnlCostFunctionAdaptor():vnl_cost_function(SpaceDimension) 
      { m_CostFunction = 0; }    

    void SetCostFunction( TCostFunction * costFunction ) 
      { m_CostFunction = costFunction; }
      
    /**  Delegate computation of the value to the CostFunction. */
    virtual InternalMeasureType f( const InternalParametersType & inparameters ) {
      if( !m_CostFunction )
        {
        throw ExceptionObject(__FILE__, __LINE__);
        }
      ParametersType parameters(SpaceDimension);
      ConvertInternalToExternalParameters( inparameters, parameters );
      const InternalMeasureType value = 
        (InternalMeasureType)m_CostFunction->GetValue( parameters );
      return value;
    }
      
    /**  Delegate computation of the gradient to the costfunction.  */
    virtual void gradf(const InternalParametersType   & inparameters,
                       InternalDerivativeType   & gradient ) {
      if( !m_CostFunction )
        {
        throw ExceptionObject(__FILE__, __LINE__);
        }
      ParametersType parameters(SpaceDimension);
      ConvertInternalToExternalParameters( inparameters, parameters );
      DerivativeType externalGradient = 
        m_CostFunction->GetDerivative( parameters );
      ConvertExternalToInternalGradient( externalGradient, gradient);
    }
      
    /**  Delegate computation of value and gradient to the costfunction.     */
    virtual void compute(const InternalParametersType   & x,
                         InternalMeasureType      * f, 
                         InternalDerivativeType   * g   ) {
      // delegate the computation to the CostFunction
      ParametersType parameters(SpaceDimension);
      ConvertInternalToExternalParameters( x, parameters );

      *f = (InternalMeasureType)m_CostFunction->GetValue( parameters );

      DerivativeType externalGradient = 
        m_CostFunction->GetDerivative( parameters );

      ConvertExternalToInternalGradient( externalGradient, *g );    
    }
 
    /**  Convert internal Parameters (vnl_Vector) 
     *  into VectorContainer type.  */
    static void ConvertInternalToExternalParameters( 
      const InternalParametersType & input,
      ParametersType         & output )
      {
        for( unsigned int i=0; i<SpaceDimension; i++)
          {
          output[i] = input[i]; 
          }
      }

    /**  Convert external Parameters VectorContainer 
     *  into internal type (vnl_Vector).  */
    static void ConvertExternalToInternalParameters(
      const  ParametersType         & input,
      InternalParametersType & output )
      {
        for( unsigned int i=0; i<SpaceDimension; i++ ) 
          {
          output[i] = input[i];
          }
      }
      
    /**  Convert external derviative measures (VectorContainer) 
     *  into internal type (vnl_Vector).  */
    void ConvertExternalToInternalGradient(
      const DerivativeType         & input,
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

  /** Set the cost Function of type TCostFunction. */
  void SetCostFunction( TCostFunction * costFunction ) 
    { m_CostFunctionAdaptor.SetCostFunction( costFunction ); }

protected:
  SingleValuedNonLinearVnlOptimizer();
  virtual ~SingleValuedNonLinearVnlOptimizer() {};

  VnlCostFunctionAdaptor            m_CostFunctionAdaptor;

private:
  SingleValuedNonLinearVnlOptimizer(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSingleValuedNonLinearVnlOptimizer.txx"
#endif

#endif



