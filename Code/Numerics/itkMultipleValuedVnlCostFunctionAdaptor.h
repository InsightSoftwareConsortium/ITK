/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultipleValuedVnlCostFunctionAdaptor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMultipleValuedVnlCostFunctionAdaptor_h
#define __itkMultipleValuedVnlCostFunctionAdaptor_h

#include "itkMultipleValuedCostFunction.h"
#include "vnl/vnl_least_squares_function.h"


namespace itk
{
  
/** \class MultipleValuedVnlCostFunctionAdaptor
 * \brief This class is an Adaptor that allows to pass 
 * itk::MultipleValuedCostFunctions to vnl_optimizers expecting
 * a vnl_cost_function.
 * 
 * This class returns a single valued.
 *
 * \ingroup Numerics Optimizers
 */
class MultipleValuedVnlCostFunctionAdaptor : 
          public vnl_least_squares_function
{
public:

  /** InternalParametersType typedef. */
  typedef   vnl_vector<double>     InternalParametersType;

  /** InternalMeasureType typedef. */
  typedef   vnl_vector<double>     InternalMeasureType;

  /** InternalGradientType typedef. */
  typedef   vnl_matrix<double>     InternalDerivativeType;

  /** MeasureType of the MultipleValuedCostFunction */
  typedef MultipleValuedCostFunction::MeasureType   MeasureType;

  /** Parameters of the MultipleValuedCostFunction */
  typedef MultipleValuedCostFunction::ParametersType ParametersType;

  /** Derivatives of the MultipleValuedCostFunction */
  typedef MultipleValuedCostFunction::DerivativeType DerivativeType;


  /** Constructor with size */
  MultipleValuedVnlCostFunctionAdaptor( unsigned int spaceDimension,
                                        unsigned int numberOfValues );

  /** Set the CostFunction deriving from MultipleValuedCostFunction */
  void SetCostFunction( MultipleValuedCostFunction * costFunction )
          { m_CostFunction = costFunction; }
    
  /** Get the CostFunction deriving from MultipleValuedCostFunction */
  const MultipleValuedCostFunction * GetCostFunction( void ) const
          { return m_CostFunction; }
    
  /**  Delegate computation of the value to the CostFunction. */
  virtual void f( const InternalParametersType & inparameters,
                        InternalMeasureType    & measures      );
    
  /**  Delegate computation of the gradient to the costFunction.  */
  virtual void gradf(const InternalParametersType   & inparameters,
                           InternalDerivativeType   & gradient );
    
  /**  Delegate computation of value and gradient to the costFunction.     */
  virtual void compute(const InternalParametersType   & x,
                             InternalMeasureType      * f, 
                             InternalDerivativeType   * g   );

  /**  Convert internal Parameters into external type */
  static void ConvertInternalToExternalParameters( 
                            const InternalParametersType & input,
                                  ParametersType         & output );

  /**  Convert external Parameters into internal type  */
  static void ConvertExternalToInternalParameters(
                            const  ParametersType         & input,
                                   InternalParametersType & output );
    
  /**  Convert external derviative measures  into internal type */
  void ConvertExternalToInternalGradient(
                            const DerivativeType         & input,
                                  InternalDerivativeType & output );

  /**  Convert external measures  into internal type */
  void ConvertExternalToInternalMeasures(
                            const MeasureType         & input,
                                  InternalMeasureType & output );

  /**  Define if the Cost function should provide a customized 
       Gradient computation or the gradient can be computed internally
       using a default approach  */
  void SetUseGradient(bool);
  void UseGradientOn()  { this->SetUseGradient( true  ); };
  void UseGradientOff() { this->SetUseGradient( false ); };
  bool GetUseGradient() const;

private:

  MultipleValuedCostFunction::Pointer   m_CostFunction;

};  // end of Class CostFunction

    
} // end namespace itk


#endif



