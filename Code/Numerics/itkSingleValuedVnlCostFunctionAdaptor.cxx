/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSingleValuedVnlCostFunctionAdaptor.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkSingleValuedVnlCostFunctionAdaptor.h"
#include "itkExceptionObject.h"


namespace itk
{
  
/**  Constructor.  */
SingleValuedVnlCostFunctionAdaptor 
::SingleValuedVnlCostFunctionAdaptor(unsigned int spaceDimension):
                             vnl_cost_function(spaceDimension) 
{ 
}    

    


/**  Delegate computation of the value to the CostFunction. */
SingleValuedVnlCostFunctionAdaptor::InternalMeasureType 
SingleValuedVnlCostFunctionAdaptor
::f( const InternalParametersType & inparameters ) 
{
  if( !m_CostFunction )
    {
    ExceptionObject ex;
    ex.SetLocation(__FILE__);
    ex.SetDescription("Attempt to use a SingleValuedVnlCostFunctionAdaptor without any CostFunction plugged in");
    throw ex;
    }

  ParametersType parameters( inparameters.size() );
  ConvertInternalToExternalParameters( inparameters, parameters );
  const InternalMeasureType value = 
    static_cast<InternalMeasureType>( m_CostFunction->GetValue( parameters ));

  return value;
}
  


/**  Delegate computation of the gradient to the costfunction.  */
void 
SingleValuedVnlCostFunctionAdaptor
::gradf(  const InternalParametersType   & inparameters,
                InternalDerivativeType   & gradient       ) 
{
  if( !m_CostFunction )
    {
    ExceptionObject ex;
    ex.SetLocation(__FILE__);
    ex.SetDescription("Attempt to use a SingleValuedVnlCostFunctionAdaptor 
        without any CostFunction plugged in");
    throw ex;
    }

  ParametersType parameters( inparameters.size() );
  ConvertInternalToExternalParameters( inparameters, parameters );
  DerivativeType externalGradient = 
                      m_CostFunction->GetDerivative( parameters );
  ConvertExternalToInternalGradient( externalGradient, gradient);

}
  


/**  Delegate computation of value and gradient to the costfunction.     */
void 
SingleValuedVnlCostFunctionAdaptor
::compute( const InternalParametersType   & x,
                 InternalMeasureType      * f, 
                 InternalDerivativeType   * g   )
{
  // delegate the computation to the CostFunction
  ParametersType parameters( x.size() );
  ConvertInternalToExternalParameters( x, parameters );

  *f = static_cast<InternalMeasureType>(
                      m_CostFunction->GetValue( parameters ) );

  DerivativeType externalGradient = 
                        m_CostFunction->GetDerivative( parameters );

  ConvertExternalToInternalGradient( externalGradient, *g );    
}



/**  Convert internal Parameters into external type.  */
void 
SingleValuedVnlCostFunctionAdaptor
::ConvertInternalToExternalParameters( const InternalParametersType & input,
                                             ParametersType         & output )
{
  const unsigned int size = input.size();
  for( unsigned int i=0; i<size; i++ )
    {
    output[i] = input[i]; 
    }
}




/**  Convert external Parameters into internal type  */
void 
SingleValuedVnlCostFunctionAdaptor
::ConvertExternalToInternalParameters( const  ParametersType         & input,
                                              InternalParametersType & output )
{
  const unsigned int size = input.size();
  for( unsigned int i=0; i<size; i++ ) 
    {
    output[i] = input[i];
    }
}
  

/**  Convert external derviative measures into internal type  */
void 
SingleValuedVnlCostFunctionAdaptor
::ConvertExternalToInternalGradient( const DerivativeType         & input,
                                           InternalDerivativeType & output )
{
  const unsigned int size = input.size();
  for( unsigned int i=0; i<size; i++ ) 
    {
    output[i] = input[i];
    }
}


} // end namespace itk





