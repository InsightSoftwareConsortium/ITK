/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#include "itkSingleValuedVnlCostFunctionAdaptor.h"

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
  m_CachedValue = NumericTraits< MeasureType >::ZeroValue();
  m_CachedDerivative.Fill(0);
}

/** Set current parameters scaling. */
void
SingleValuedVnlCostFunctionAdaptor
::SetScales(const ScalesType & scales)
{
  //Only the inverse is used computes the inverse at each iteration.
  //provides 1 commone place where the inverse can be computes
  //and validated.
  m_InverseScales.SetSize(scales.GetSize());

  for( unsigned int i = 0; i < scales.size(); ++i )
    {
    if ( scales[i] <= NumericTraits<double>::epsilon() )
      {
      itkGenericExceptionMacro("ERROR: Scales must have value greater than epsilon! Scale[" << i << "] = " << scales[i] );
      }
    m_InverseScales[i] = NumericTraits<double>::OneValue() / scales[i];
    }
  m_ScalesInitialized = true;
}

/**  Delegate computation of the value to the CostFunction. */
SingleValuedVnlCostFunctionAdaptor::InternalMeasureType
SingleValuedVnlCostFunctionAdaptor
::f(const InternalParametersType & inparameters)
{
  if ( !m_CostFunction )
    {
    itkGenericExceptionMacro(
      << "Attempt to use a SingleValuedVnlCostFunctionAdaptor without any CostFunction plugged in");
    }

  // Use scales if they are provided
  ParametersType parameters( inparameters.size() );
  if ( m_ScalesInitialized )
    {
    const ScalesType & invScales = this->GetInverseScales();
    for ( unsigned int i = 0; i < parameters.size(); i++ )
      {
      parameters[i] = inparameters[i] * invScales[i];
      }
    }
  else
    {
    parameters.SetDataSameSize( const_cast< double * >( inparameters.data_block() ), false );
    }

  InternalMeasureType value = static_cast< InternalMeasureType >( m_CostFunction->GetValue(parameters) );

  if ( m_NegateCostFunction )
    {
    value *= -1.0;
    }

  // Notify observers. This is used for overcoming the limitaion of VNL
  // optimizers of not providing callbacks per iteration.
  m_CachedValue = value;
  m_CachedCurrentParameters = parameters;
  this->ReportIteration( FunctionEvaluationIterationEvent() );

  return value;
}

/**  Delegate computation of the gradient to the costfunction.  */
void
SingleValuedVnlCostFunctionAdaptor
::gradf(const InternalParametersType   & inparameters,
        InternalDerivativeType   & gradient)
{
  if ( !m_CostFunction )
    {
    itkGenericExceptionMacro("Attempt to use a SingleValuedVnlCostFunctionAdaptor without any CostFunction plugged in");
    }

  // Use scales if they are provided
  ParametersType parameters( inparameters.size() );
  if ( m_ScalesInitialized )
    {
    const ScalesType & invScales = this->GetInverseScales();
    for ( unsigned int i = 0; i < parameters.size(); i++ )
      {
      parameters[i] = inparameters[i] * invScales[i];
      }
    }
  else
    {
    parameters.SetDataSameSize( const_cast< double * >( inparameters.data_block() ), false );
    }

  m_CostFunction->GetDerivative(parameters, m_CachedDerivative);
  this->ConvertExternalToInternalGradient(m_CachedDerivative, gradient);

  // Notify observers. This is used for overcoming the limitaion of VNL
  // optimizers of not providing callbacks per iteration.
  // Note that m_CachedDerivative is already loaded in the GetDerivative()
  // above.
  m_CachedCurrentParameters = parameters;
  this->ReportIteration( GradientEvaluationIterationEvent() );
}

/**  Delegate computation of value and gradient to the costfunction.     */
void
SingleValuedVnlCostFunctionAdaptor
::compute(const InternalParametersType   & x,
          InternalMeasureType      *fun,
          InternalDerivativeType   *g)
{
  // delegate the computation to the CostFunction
  ParametersType parameters( x.size() );
  double         measure;

  if ( m_ScalesInitialized )
    {
    const ScalesType & invScales = this->GetInverseScales();
    for ( unsigned int i = 0; i < parameters.size(); i++ )
      {
      parameters[i] = x[i] * invScales[i];
      }
    }
  else
    {
    parameters.SetDataSameSize( const_cast< double * >( x.data_block() ), false );
    }

  m_CostFunction->GetValueAndDerivative(parameters, measure, m_CachedDerivative);
  if ( g ) // sometimes Vnl doesn't pass a valid pointer
    {
    this->ConvertExternalToInternalGradient(m_CachedDerivative, *g);
    }
  if ( fun ) // paranoids have longer lives...
    {
    if ( !m_NegateCostFunction )
      {
      *fun = static_cast< InternalMeasureType >( measure );
      }
    else
      {
      *fun = static_cast< InternalMeasureType >( -measure );
      }
    // Notify observers. This is used for overcoming the limitaion of VNL
    // optimizers of not providing callbacks per iteration.
    // Note that m_CachedDerivative is already loaded in the GetDerivative()
    // above.
    m_CachedValue = *fun;
    }
  m_CachedCurrentParameters = parameters;
  this->ReportIteration( FunctionAndGradientEvaluationIterationEvent() );
}

/**  Convert external derviative measures into internal type  */
void
SingleValuedVnlCostFunctionAdaptor
::ConvertExternalToInternalGradient(const DerivativeType   & input,
                                    InternalDerivativeType & output) const
{
  const unsigned int size = input.size();

  output = InternalDerivativeType(size);
  const ScalesType & invScales = this->GetInverseScales();
  for ( unsigned int i = 0; i < size; i++ )
    {
    if ( !m_NegateCostFunction )
      {
      output[i] = input[i];
      }
    else
      {
      output[i] = -input[i];
      }

    if ( m_ScalesInitialized )
      {
      output[i] *= invScales[i];
      }
    }
}

/**  Set whether the cost function should be negated or not. This is useful for
 * adapting optimizers that are only minimizers. */
void
SingleValuedVnlCostFunctionAdaptor
::SetNegateCostFunction(bool flag)
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

/**  This method reports iterations events. It is intended to
 *   help monitoring the progress of the optimization process. */
void
SingleValuedVnlCostFunctionAdaptor
::ReportIteration(const EventObject & event) const
{
  this->m_Reporter->InvokeEvent(event);
}

/**  Connects a Command/Observer to the internal reporter class.
 *   This is useful for reporting iteration event to potential observers. */
unsigned long
SingleValuedVnlCostFunctionAdaptor
::AddObserver(const EventObject & event, Command *command) const
{
  return m_Reporter->AddObserver(event, command);
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

/**  Return the cached value of the parameters used for computing the function
  */
const SingleValuedVnlCostFunctionAdaptor::ParametersType &
SingleValuedVnlCostFunctionAdaptor
::GetCachedCurrentParameters() const
{
  return m_CachedCurrentParameters;
}
} // end namespace itk
