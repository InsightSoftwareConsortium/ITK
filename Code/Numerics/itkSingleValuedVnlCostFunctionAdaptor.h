/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSingleValuedVnlCostFunctionAdaptor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSingleValuedVnlCostFunctionAdaptor_h
#define __itkSingleValuedVnlCostFunctionAdaptor_h

#include "itkSingleValuedCostFunction.h"
#include "vnl/vnl_cost_function.h"


namespace itk
{
  
/** \class SingleValuedVnlCostFunctionAdaptor
 * \brief This class is an Adaptor that allows to pass 
 * itk::SingleValuedCostFunctions to vnl_optimizers expecting
 * a vnl_cost_function.
 * 
 * This class returns a single valued.
 *
 * \ingroup Numerics Optimizers
 */
class SingleValuedVnlCostFunctionAdaptor : 
    public vnl_cost_function
{
public:

  /** InternalParametersType typedef. */
  typedef   vnl_vector<double>     InternalParametersType;

  /** InternalMeasureType typedef. */
  typedef   double                 InternalMeasureType;

  /** InternalGradientType typedef. */
  typedef   vnl_vector<double>     InternalDerivativeType;

  /** Parameters of the SingleValuedCostFunction */
  typedef SingleValuedCostFunction::ParametersType ParametersType;

  /** Derivatives of the SingleValuedCostFunction */
  typedef SingleValuedCostFunction::DerivativeType DerivativeType;

  /** Scales typedef */
  typedef Array<double>             ScalesType;

  /** Constructor with size */
  SingleValuedVnlCostFunctionAdaptor(unsigned int spaceDimension);

  /** Set the CostFunction deriving from SingleValuedCostFunction */
  void SetCostFunction( SingleValuedCostFunction * costFunction )
  { m_CostFunction = costFunction; }
    
  /** Get the CostFunction deriving from SingleValuedCostFunction */
  const SingleValuedCostFunction * GetCostFunction( void ) const
  { return m_CostFunction; }
    
  /**  Delegate computation of the value to the CostFunction. */
  virtual InternalMeasureType f( const InternalParametersType & inparameters ) const;
    
  /**  Delegate computation of the gradient to the costFunction.  */
  virtual void gradf(const InternalParametersType   & inparameters,
                     InternalDerivativeType   & gradient );
    
  /**  Delegate computation of value and gradient to the costFunction.     */
  virtual void compute(const InternalParametersType   & x,
                       InternalMeasureType      * f, 
                       InternalDerivativeType   * g   );
    
  /**  Convert external derviative measures into internal type   */
  void ConvertExternalToInternalGradient(
    const DerivativeType         & input,
    InternalDerivativeType & output );

  /** Set current parameters scaling. */
  void SetScales(const ScalesType & scales);

private:

  SingleValuedCostFunction::Pointer   m_CostFunction;
  bool                    m_ScalesInitialized;
  ScalesType              m_Scales;

};  // end of Class CostFunction

    
} // end namespace itk


#endif



