/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSingleValuedVnlCostFunctionAdaptor.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
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
  m_ScalesInitialized =false;
}    

    
/** Set current parameters scaling. */
void
SingleValuedVnlCostFunctionAdaptor
::SetScales(const ScalesType & scales)
{
  m_Scales = scales;
  m_ScalesInitialized = true;
}   


/**  Delegate computation of the value to the CostFunction. */
SingleValuedVnlCostFunctionAdaptor::InternalMeasureType 
SingleValuedVnlCostFunctionAdaptor
::f( const InternalParametersType & inparameters ) const
{
  if( !m_CostFunction )
    {
    itkGenericExceptionMacro(<<"Attempt to use a SingleValuedVnlCostFunctionAdaptor without any CostFunction plugged in");
    }

  // Use scales if they are provided
 ParametersType parameters(inparameters.size());
  if(m_ScalesInitialized)
    {
    for(unsigned int i=0;i<parameters.size();i++)
      {
      parameters[i] = inparameters[i]/m_Scales[i];
      }
    }
  else
    {
    parameters.SetData(const_cast<double*>(inparameters.data_block()));
    }

  const InternalMeasureType value = static_cast<InternalMeasureType>( m_CostFunction->GetValue( parameters ));
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
    itkGenericExceptionMacro("Attempt to use a SingleValuedVnlCostFunctionAdaptor without any CostFunction plugged in");
    }

   // Use scales if they are provided
  DerivativeType externalGradient;
  ParametersType parameters(inparameters.size()); 
  if(m_ScalesInitialized)
    {
    for(unsigned int i=0;i<parameters.size();i++)
      {
      parameters[i] = inparameters[i]/m_Scales[i];
      }
    }
  else
    {
    parameters.SetData(const_cast<double*>(inparameters.data_block()));
    }
    
  m_CostFunction->GetDerivative( parameters, externalGradient );
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
  DerivativeType externalGradient;
  ParametersType parameters( x.size());
  double   measure;
  if(m_ScalesInitialized)
    {
    for(unsigned int i=0;i<parameters.size();i++)
      {
      parameters[i] = x[i]/m_Scales[i];
      }
    }
  else
    {
    parameters.SetData(const_cast<double*>(x.data_block()));
    }
  
  m_CostFunction->GetValueAndDerivative( parameters, measure, externalGradient );
  ConvertExternalToInternalGradient( externalGradient, *g );
  *f = static_cast<InternalMeasureType>( measure );  
}
  

/**  Convert external derviative measures into internal type  */
void 
SingleValuedVnlCostFunctionAdaptor
::ConvertExternalToInternalGradient( const DerivativeType         & input,
                                     InternalDerivativeType & output )
{
  const unsigned int size = input.size();
  output = InternalDerivativeType(size);
  for( unsigned int i=0; i<size; i++ ) 
    {
    output[i] = input[i];
    }
}


} // end namespace itk





