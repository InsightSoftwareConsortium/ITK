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
#ifndef _itkSingleValuedNonLinearVnlOptimizer_hxx
#define _itkSingleValuedNonLinearVnlOptimizer_hxx

#include "itkSingleValuedNonLinearVnlOptimizer.h"

namespace itk
{
/** Constructor */
SingleValuedNonLinearVnlOptimizer
::SingleValuedNonLinearVnlOptimizer()
{
  m_CostFunctionAdaptor = ITK_NULLPTR;
  m_Maximize = false;
  m_Command = CommandType::New();
  m_Command->SetCallbackFunction(this,
                                 &SingleValuedNonLinearVnlOptimizer::IterationReport);
  m_CachedValue = 0;
  m_CachedCurrentPosition.Fill(0);
  m_CachedDerivative.Fill(0);
}

/** Destructor */
SingleValuedNonLinearVnlOptimizer
::~SingleValuedNonLinearVnlOptimizer()
{
  delete m_CostFunctionAdaptor;
  m_CostFunctionAdaptor = ITK_NULLPTR;
}

void
SingleValuedNonLinearVnlOptimizer
::SetCostFunctionAdaptor(CostFunctionAdaptorType *adaptor)
{
  if ( m_CostFunctionAdaptor == adaptor )
    {
    return;
    }

  delete m_CostFunctionAdaptor;

  m_CostFunctionAdaptor = adaptor;

  m_CostFunctionAdaptor->AddObserver(IterationEvent(), m_Command);
}

const SingleValuedNonLinearVnlOptimizer::CostFunctionAdaptorType *
SingleValuedNonLinearVnlOptimizer
::GetCostFunctionAdaptor(void) const
{
  return m_CostFunctionAdaptor;
}

SingleValuedNonLinearVnlOptimizer::CostFunctionAdaptorType *
SingleValuedNonLinearVnlOptimizer
::GetCostFunctionAdaptor(void)
{
  return m_CostFunctionAdaptor;
}

/** The purpose of this method is to get around the lack of
 *  const-correctness in VNL cost-functions and optimizers */
SingleValuedNonLinearVnlOptimizer::CostFunctionAdaptorType *
SingleValuedNonLinearVnlOptimizer
::GetNonConstCostFunctionAdaptor(void) const
{
  return m_CostFunctionAdaptor;
}

/** The purpose of this method is to get around the lack of iteration reporting
 * in VNL optimizers. By interfacing directly with the ITK cost function
 * adaptor we are generating here Iteration Events. Note the iteration events
 * here are produce PER EVALUATION of the metric, not per real iteration of the
 * vnl optimizer. Optimizers that evaluate the metric multiple times at each
 * iteration will generate a lot more of Iteration events here. */
void
SingleValuedNonLinearVnlOptimizer
::IterationReport(const EventObject & event)
{
  const CostFunctionAdaptorType *adaptor = this->GetCostFunctionAdaptor();

  m_CachedValue = adaptor->GetCachedValue();
  m_CachedDerivative = adaptor->GetCachedDerivative();
  m_CachedCurrentPosition = adaptor->GetCachedCurrentParameters();
  this->InvokeEvent(event);
}

/**
 * PrintSelf
 */
void
SingleValuedNonLinearVnlOptimizer
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Maximize flag: "
     << ( m_Maximize ? "On" : "Off" ) << std::endl;
  os << indent << "Cached Value: " << m_CachedValue << std::endl;
  os << indent << "Cached Derivative: " << m_CachedDerivative << std::endl;
  os << indent << "Cached current positiion: "
     << m_CachedCurrentPosition << std::endl;
  os << "Command observer " << m_Command.GetPointer() << std::endl;
  os << "Cost Function adaptor" << m_CostFunctionAdaptor << std::endl;
}
} // end namespace itk

#endif
