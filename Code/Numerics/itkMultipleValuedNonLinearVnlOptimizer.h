/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultipleValuedNonLinearVnlOptimizer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMultipleValuedNonLinearVnlOptimizer_h
#define __itkMultipleValuedNonLinearVnlOptimizer_h


#include "vnl/vnl_least_squares_function.h"
#include "itkMultipleValuedNonLinearOptimizer.h"
#include "itkExceptionObject.h"

namespace itk
{
  
/** \class MultipleValuedNonLinearVnlOptimizer
 * \brief This class is a base for the Optimization methods that 
 * optimize a single valued function.
 *
 * \ingroup Numerics Optimizers
 */
template <class TCostFunction>
class ITK_EXPORT MultipleValuedNonLinearVnlOptimizer : 
          public MultipleValuedNonLinearOptimizer< TCostFunction > 
{
public:
  /** Standard class typedefs. */
  typedef MultipleValuedNonLinearVnlOptimizer  Self;
  typedef   MultipleValuedNonLinearOptimizer< TCostFunction > Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
    
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro( MultipleValuedNonLinearOptimizer, NonLinearOptimizer );

  /** Dimension of the search space */
  enum { 
    SpaceDimension = TCostFunction::SpaceDimension,
    RangeDimension = TCostFunction::RangeDimension
    };
  
  /** InternalParameters typedef. */
  typedef   vnl_vector<double>     InternalParametersType;

  /** InternalMeasure typedef. */
  typedef   vnl_vector<double>     InternalMeasureType;

  /** InternalGradient typedef. */
  typedef   vnl_matrix<double>     InternalDerivativeType;

  /**  ParametersType typedef.
   *  It defines a position in the optimization search space. */
  typedef typename TCostFunction::ParametersType    ParametersType;

  /**  MeasureType typedef.
   *  It defines a type used to return the cost function value. */
  typedef typename TCostFunction::MeasureType         MeasureType;

  /**  GradientType typedef.
   *  It defines a type used to return the cost function derivative. */
  typedef typename TCostFunction::DerivativeType      DerivativeType;
 
  /** \class VnlCostFunction
   * \brief Adaptor between the CostFunction and the
   * vnl_least_squares_function classes
   */
  class VnlCostFunctionAdaptor : public vnl_least_squares_function 
  {
  public:
    VnlCostFunctionAdaptor():
        vnl_least_squares_function(SpaceDimension,
                                   RangeDimension,
                                   no_gradient) 
      { m_CostFunction = 0; }    

    void SetCostFunction( TCostFunction * costFunction ) 
      { m_CostFunction = costFunction; }

    /**  Delegate computation of the value to the CostFunction     */
    virtual void f( const InternalParametersType & parameters, 
                    InternalMeasureType    & output      )
      {
        if( !m_CostFunction )
          {
          throw ExceptionObject(__FILE__, __LINE__);
          }

        ParametersType externalParameters;
        ConvertInternalToExternalParameters( parameters, externalParameters ); 
        ConvertExternalToInternalMeasure( 
          m_CostFunction->GetValue( externalParameters ),output);
      }

    /**  Delegate computation of the gradient to the CostFunction     */
    virtual void gradf(const InternalParametersType & parameters,
                       InternalDerivativeType & derivative ) 
      {
        if( !m_CostFunction )
          {
          throw ExceptionObject(__FILE__, __LINE__);
          }

        ParametersType externalParameters; 
        ConvertInternalToExternalParameters( parameters, externalParameters );

        ConvertGradient( m_CostFunction->GetDerivative( externalParameters ),
                         derivative );
      }
    /**  Delegate computation of value and gradient to the CostFunction     */
    virtual void compute(const InternalParametersType & x,
                         InternalMeasureType * f, 
                         InternalDerivativeType * g ) 
      {
        // delegate the computation to the CostFunction

        ParametersType externalParameters; 
        ConvertInternalToExternalParameters( x, externalParameters );
        ConvertExternalToInternalMeasure(m_CostFunction->GetValue( externalParameters ), *f);
        ConvertGradient(m_CostFunction->GetDerivative( externalParameters ), *g ); 
      }

    /**  Convert internal Parameters (vnl_Vector) 
     *  into VectorContainer type     */
    static void ConvertInternalToExternalParameters( 
      const InternalParametersType & input,
      ParametersType         & output )
      {
        for( unsigned int i=0; i<SpaceDimension; i++)
          { output[i] = input[i]; }
      }

    /**  Convert external Parameters VectorContainer 
     *  into internal type (vnl_Vector)     */
    static void ConvertExternalToInternalParameters(
      const  ParametersType         & input,
      InternalParametersType & output )
      {
        for( unsigned int i=0; i<SpaceDimension; i++ ) 
          { output[i] = input[i]; }
      }
     
    /**  Convert external Gradient (itkMatrix) 
       *  into internal gradient type (vnl_Matrix)     */
    void ConvertGradient(const DerivativeType & input,
                         InternalDerivativeType & output )
      { output = input; }

    /**  Convert external Measure (itkPoint) 
      *  into internal Measure type (vnl_Vector)    */
    void ConvertExternalToInternalMeasure(
      const MeasureType         & input,
      InternalMeasureType & output )
      {
        for( unsigned int i=0; i<RangeDimension; i++ )
          { output[i] = input[i]; }
      }

    /**  Convert internal Measure (vnl_vector) 
      *  into external Measure type (itkPoint)    */
    void ConvertInternalToExternalMeasure(
      const InternalMeasureType & input,
      MeasureType         & output )
      {
        for( unsigned int i=0; i<RangeDimension; i++ )
          { output[i] = input[i]; }
      }

private:
    typename TCostFunction::Pointer   m_CostFunction;
};  // end of Class CostFunction


  /** Set the cost Function of type TCostFunction */
  void SetCostFunction( TCostFunction * costFunction ) 
    { m_CostFunctionAdaptor.SetCostFunction( costFunction ); }
    
  

protected:
  MultipleValuedNonLinearVnlOptimizer();
  virtual ~MultipleValuedNonLinearVnlOptimizer() {};

  VnlCostFunctionAdaptor            m_CostFunctionAdaptor;

private:
  MultipleValuedNonLinearVnlOptimizer(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMultipleValuedNonLinearVnlOptimizer.txx"
#endif

#endif
