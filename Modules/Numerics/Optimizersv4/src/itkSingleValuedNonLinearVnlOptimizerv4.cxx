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
#include "itkSingleValuedNonLinearVnlOptimizerv4.h"

namespace itk
{
SingleValuedNonLinearVnlOptimizerv4
::SingleValuedNonLinearVnlOptimizerv4()
{
  this->m_CostFunctionAdaptor = ITK_NULLPTR;
  this->m_Command = CommandType::New();
  this->m_Command->SetCallbackFunction(this,  &SingleValuedNonLinearVnlOptimizerv4::IterationReport);

  this->m_CachedCurrentPosition.Fill( NumericTraits<DerivativeType::ValueType>::ZeroValue() );
  this->m_CachedDerivative.Fill( NumericTraits<DerivativeType::ValueType>::ZeroValue() );
}

SingleValuedNonLinearVnlOptimizerv4
::~SingleValuedNonLinearVnlOptimizerv4()
{
  if ( this->m_CostFunctionAdaptor )
    {
    delete this->m_CostFunctionAdaptor;
    this->m_CostFunctionAdaptor = ITK_NULLPTR;
    }
}

void
SingleValuedNonLinearVnlOptimizerv4
::StartOptimization(bool doOnlyInitialization )
{
  // Perform some verification, check scales.
  Superclass::StartOptimization( doOnlyInitialization );

  this->m_CurrentIteration = 0;

  // Verify adaptor
  if( this->m_CostFunctionAdaptor == ITK_NULLPTR )
    {
    itkExceptionMacro("CostFunctionAdaptor has not been set.");
    }

  // If the user provides the scales and they're not identity, then we set.
  // Otherwise we don't set them for computation speed.
  // These are managed at the optimizer level, but
  // applied at the cost-function adaptor level because that's
  // where the per-iteration results of the vnl optimizer are accessible.
  if ( ! this->GetScalesAreIdentity() )
    {
    ScalesType scales = this->GetScales();
    this->GetNonConstCostFunctionAdaptor()->SetScales(scales);
    }
}

void
SingleValuedNonLinearVnlOptimizerv4
::SetCostFunctionAdaptor(CostFunctionAdaptorType *adaptor)
{
  if ( this->m_CostFunctionAdaptor == adaptor )
    {
    return;
    }

  if ( this->m_CostFunctionAdaptor )
    {
    delete this->m_CostFunctionAdaptor;
    }

  this->m_CostFunctionAdaptor = adaptor;

  this->m_CostFunctionAdaptor->AddObserver(IterationEvent(), this->m_Command);
}

const SingleValuedNonLinearVnlOptimizerv4::CostFunctionAdaptorType *
SingleValuedNonLinearVnlOptimizerv4
::GetCostFunctionAdaptor(void) const
{
  return this->m_CostFunctionAdaptor;
}

SingleValuedNonLinearVnlOptimizerv4::CostFunctionAdaptorType *
SingleValuedNonLinearVnlOptimizerv4
::GetCostFunctionAdaptor(void)
{
  return this->m_CostFunctionAdaptor;
}

SingleValuedNonLinearVnlOptimizerv4::CostFunctionAdaptorType *
SingleValuedNonLinearVnlOptimizerv4
::GetNonConstCostFunctionAdaptor(void) const
{
  return this->m_CostFunctionAdaptor;
}

void
SingleValuedNonLinearVnlOptimizerv4
::IterationReport(const EventObject & event)
{
  const CostFunctionAdaptorType *adaptor = this->GetCostFunctionAdaptor();

  this->m_CurrentMetricValue = adaptor->GetCachedValue();
  this->m_CachedDerivative = adaptor->GetCachedDerivative();
  this->m_CachedCurrentPosition = adaptor->GetCachedCurrentParameters();
  this->InvokeEvent(event);
}

void
SingleValuedNonLinearVnlOptimizerv4
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Cached Derivative: " << this->m_CachedDerivative << std::endl;
  os << indent << "Cached current positiion: " << this->m_CachedCurrentPosition << std::endl;
  os << "Command observer " << this->m_Command.GetPointer() << std::endl;
  os << "Cost Function adaptor" << this->m_CostFunctionAdaptor << std::endl;
}
} // end namespace itk
