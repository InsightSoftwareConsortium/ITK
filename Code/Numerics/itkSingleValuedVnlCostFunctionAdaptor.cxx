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
  m_ScalesInitialized = false;
  m_NegateCostFunction = false;
  m_Reporter = Object::New();
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
::f( const InternalParametersType & inparameters )
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

  InternalMeasureType value = static_cast<InternalMeasureType>( m_CostFunction->GetValue( parameters ));

  if( m_NegateCostFunction )
    {
    value *= -1.0;
    }

  // Notify observers. This is used for overcoming the limitaion of VNL
  // optimizers of not providing callbacks per iteration.
  m_CachedValue = value;
  m_CachedCurrentParameters = parameters;
  this->ReportIteration(); 
    
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
    
  m_CostFunction->GetDerivative( parameters, m_CachedDerivative );
  this->ConvertExternalToInternalGradient( m_CachedDerivative, gradient);

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
  if( g ) // sometimes Vnl doesn't pass a valid pointer
    {
    this->ConvertExternalToInternalGradient( externalGradient, *g );
    }
  if( f ) // paranoids have longer lives...
    {
    if( !m_NegateCostFunction )
      {
      *f = static_cast<InternalMeasureType>( measure );  
      }
    else
      {
      *f = static_cast<InternalMeasureType>( - measure );  
      }
    }
}
  

/**  Convert external derviative measures into internal type  */
void 
SingleValuedVnlCostFunctionAdaptor
::ConvertExternalToInternalGradient( const DerivativeType   & input,
                                     InternalDerivativeType & output ) const
{
  const unsigned int size = input.size();
  output = InternalDerivativeType(size);
  for( unsigned int i=0; i<size; i++ ) 
    {
    if( !m_NegateCostFunction )
      {
      output[i] = input[i];
      }
    else
      {
      output[i] = -input[i];
      }
    }
}


/**  Set whether the cost function should be negated or not. This is useful for
 * adapting optimizers that are only minimizers. */
void 
SingleValuedVnlCostFunctionAdaptor
::SetNegateCostFunction( bool flag )
{
  m_NegateCostFunction = flag;
}
                                   
/**  Returns whether the cost function is going to be negated or not. 
 *   This is useful for adapting optimizers that are only minimizers. */
bool 
SingleValuedVnlCostFunctionAdaptor
::GetNegateCostFunction() const
{
  return m_NegateCostFunction;
}

/**  Returns whether the cost function is going to be negated or not. 
 *   This is useful for adapting optimizers that are only minimizers. */
void 
SingleValuedVnlCostFunctionAdaptor
::ReportIteration() const
{
  this->m_Reporter->InvokeEvent( IterationEvent() );
}
 


/**  Connects a Command/Observer to the internal reporter class.
 *   This is useful for reporting iteration event to potential observers. */
unsigned long 
SingleValuedVnlCostFunctionAdaptor
::AddObserver(const EventObject & event, Command * command) const
{
  return m_Reporter->AddObserver( event, command );
}


/**  Return the cached value of the cost function */
const SingleValuedVnlCostFunctionAdaptor::MeasureType &
SingleValuedVnlCostFunctionAdaptor
::GetCachedValue() const
{
  return m_CachedValue;
}


/**  Return the cached value of the cost function derivative */
const SingleValuedVnlCostFunctionAdaptor::DerivativeType &
SingleValuedVnlCostFunctionAdaptor
::GetCachedDerivative() const
{
  return m_CachedDerivative;
}

/**  Return the cached value of the parameters used for computing the function */
const SingleValuedVnlCostFunctionAdaptor::ParametersType &
SingleValuedVnlCostFunctionAdaptor
::GetCachedCurrentParameters() const
{
  return m_CachedCurrentParameters;
}




} // end namespace itk





