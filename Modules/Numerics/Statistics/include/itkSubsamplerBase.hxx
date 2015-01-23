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
#ifndef itkSubsamplerBase_hxx
#define itkSubsamplerBase_hxx
#include "itkSubsamplerBase.h"

namespace itk {
namespace Statistics {

template <typename TSample>
SubsamplerBase<TSample>
::SubsamplerBase()
{
  m_Sample = ITK_NULLPTR;
  m_RequestMaximumNumberOfResults = true;
  m_CanSelectQuery = true;
  m_Seed =  0;
}

template <typename TSample>
typename LightObject::Pointer
SubsamplerBase<TSample>
::InternalClone() const
{
  typename LightObject::Pointer loPtr = Superclass::InternalClone();

  typename Self::Pointer rval =
    dynamic_cast<Self *>(loPtr.GetPointer());
  if(rval.IsNull())
    {
    itkExceptionMacro(<< "downcast to type "
                      << this->GetNameOfClass()
                      << " failed.");
    }
  rval->SetSample(this->GetSample());
  rval->m_RequestMaximumNumberOfResults = this->m_RequestMaximumNumberOfResults;
  rval->m_CanSelectQuery = this->m_CanSelectQuery;
  rval->SetSeed(this->m_Seed);
  return loPtr;
}

template <typename TSample>
void
SubsamplerBase<TSample>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Request maximum number of results: " << m_RequestMaximumNumberOfResults
     << std::endl;
  os << indent << "Can select query index during search: " << m_CanSelectQuery << std::endl;
  os << indent << "seed: " << m_Seed << std::endl;

  if (m_Sample)
  {
    os << indent << "Sample: " << m_Sample << std::endl;
  }
  else
  {
    os << indent << "Sample is ITK_NULLPTR" << std::endl;
  }

  os << std::endl;
}

}// end namespace Statistics
}// end namespace itk

#endif
