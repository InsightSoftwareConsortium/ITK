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
#include "itkMultipleValuedVnlCostFunctionAdaptor.h"

namespace itk
{
/**  Constructor.  */
MultipleValuedVnlCostFunctionAdaptor
::MultipleValuedVnlCostFunctionAdaptor(
  unsigned int spaceDimension, unsigned int numberOfValues):
  vnl_least_squares_function(spaceDimension, numberOfValues)
{
  this->m_ScalesInitialized = false;
  this->m_Reporter = Object::New();
}

/** Set current parameters scaling. */
void
MultipleValuedVnlCostFunctionAdaptor
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
void
MultipleValuedVnlCostFunctionAdaptor
::f(const InternalParametersType & inparameters,
    InternalMeasureType    & measures)
{
  if ( !this->m_CostFunction )
    {
    ExceptionObject ex;
    ex.SetLocation(__FILE__);
    ex.SetDescription("Attempt to use a MultipleValuedVnlCostFunctionAdaptor without any CostFunction plugged in");
    throw ex;
    }

  ParametersType parameters( inparameters.size() );
  // Use scales if they are provided
  if ( this->m_ScalesInitialized )
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

  measures = this->m_CostFunction->GetValue(parameters);

  // Notify observers. This is used for overcoming the limitaion of VNL
  // optimizers of not providing callbacks per iteration.
  m_CachedValue = measures;
  m_CachedCurrentParameters = parameters;
  this->ReportIteration( FunctionEvaluationIterationEvent() );
}

/**  Delegate computation of the gradient to the costfunction.  */
void
MultipleValuedVnlCostFunctionAdaptor
::gradf(const InternalParametersType   & inparameters,
        InternalDerivativeType   & gradient)
{
  if ( !this->m_CostFunction )
    {
    ExceptionObject ex;
    ex.SetLocation(__FILE__);
    ex.SetDescription("Attempt to use a MultipleValuedVnlCostFunctionAdaptor without any CostFunction plugged in");
    throw ex;
    }

  DerivativeType externalGradient;
  ParametersType parameters( inparameters.size() );
  if ( this->m_ScalesInitialized )
    {
    const ScalesType & invScales = this->GetInverseScales();
    for ( unsigned int i = 0; i < parameters.size(); i++ )
      {
      parameters[i] = inparameters[i] *  invScales[i];
      }
    }
  else
    {
    parameters.SetDataSameSize( const_cast< double * >( inparameters.data_block() ), false );
    }

  this->m_CostFunction->GetDerivative(parameters, externalGradient);
  this->ConvertExternalToInternalGradient(externalGradient, gradient);
}

/**  Delegate computation of value and gradient to the costfunction.     */
void
MultipleValuedVnlCostFunctionAdaptor
::compute(const InternalParametersType   & x,
          InternalMeasureType      *ff,
          InternalDerivativeType   *g)
{
  // delegate the computation to the CostFunction
  DerivativeType externalGradient;
  ParametersType parameters( x.size() );

  if ( this->m_ScalesInitialized )
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

  *ff = static_cast< InternalMeasureType >(
    this->m_CostFunction->GetValue(parameters) );
  this->m_CostFunction->GetDerivative(parameters, externalGradient);

  this->ConvertExternalToInternalGradient(externalGradient, *g);

  // Notify observers. This is used for overcoming the limitaion of VNL
  // optimizers of not providing callbacks per iteration.
  // Note that m_CachedDerivative is already loaded in the GetDerivative()
  // above.
  m_CachedValue = *ff;
  m_CachedCurrentParameters = parameters;
  this->ReportIteration( FunctionAndGradientEvaluationIterationEvent() );
}

/**  Convert external derviative measures into internal type  */
void
MultipleValuedVnlCostFunctionAdaptor
::ConvertExternalToInternalGradient(const DerivativeType         & input,
                                    InternalDerivativeType & output)
{
  const unsigned int rows = input.rows();
  const unsigned int cols = input.cols();

  const ScalesType & invScales = this->GetInverseScales();
  for ( unsigned int i = 0; i < rows; i++ )
    {
    for ( unsigned int j = 0; j < cols; j++ )
      {
      output(j, i) = input(i, j);

      if ( this->m_ScalesInitialized )
        {
        output(j, i) *= invScales[i];
        }
      }
    }
}

/**  Convert external Measures into internal type  */
void
MultipleValuedVnlCostFunctionAdaptor
::ConvertExternalToInternalMeasures(const MeasureType         & input,
                                    InternalMeasureType & output)
{
  const unsigned int size = input.size();

  for ( unsigned int i = 0; i < size; i++ )
    {
    output[i] = input[i];
    }
}

/**  Define if the cost function will provide a Gradient computation */
void
MultipleValuedVnlCostFunctionAdaptor
::SetUseGradient(bool useGradient)
{
  // delegate the task to the base class
  this->vnl_least_squares_function::use_gradient_ = useGradient;
}

/**  Return true if the cost function will provide a Gradient computation */
bool
MultipleValuedVnlCostFunctionAdaptor
::GetUseGradient() const
{
  // delegate the task to the base class
  return this->vnl_least_squares_function::has_gradient();
}

/**  This method reports iterations events. It is intended to
 *   help monitoring the progress of the optimization process. */
void
MultipleValuedVnlCostFunctionAdaptor
::ReportIteration(const EventObject & event) const
{
  this->m_Reporter->InvokeEvent(event);
}

/**  Connects a Command/Observer to the internal reporter class.
 *   This is useful for reporting iteration event to potential observers. */
unsigned long
MultipleValuedVnlCostFunctionAdaptor
::AddObserver(const EventObject & event, Command *command) const
{
  return this->m_Reporter->AddObserver(event, command);
}

/**  Return the cached value of the cost function */
const MultipleValuedVnlCostFunctionAdaptor::MeasureType &
MultipleValuedVnlCostFunctionAdaptor
::GetCachedValue() const
{
  return m_CachedValue;
}

/**  Return the cached value of the cost function derivative */
const MultipleValuedVnlCostFunctionAdaptor::DerivativeType &
MultipleValuedVnlCostFunctionAdaptor
::GetCachedDerivative() const
{
  return m_CachedDerivative;
}

/**  Return the cached value of the parameters used for computing the function
  */
const MultipleValuedVnlCostFunctionAdaptor::ParametersType &
MultipleValuedVnlCostFunctionAdaptor
::GetCachedCurrentParameters() const
{
  return m_CachedCurrentParameters;
}
} // end namespace itk
