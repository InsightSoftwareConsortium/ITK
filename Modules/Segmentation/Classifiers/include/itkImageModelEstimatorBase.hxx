/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkImageModelEstimatorBase_hxx
#define itkImageModelEstimatorBase_hxx

#include "itkCommand.h"
#include "itkPrintHelper.h"

namespace itk
{
template <typename TInputImage, typename TMembershipFunction>
ImageModelEstimatorBase<TInputImage, TMembershipFunction>::ImageModelEstimatorBase()

  = default;

template <typename TInputImage, typename TMembershipFunction>
void
ImageModelEstimatorBase<TInputImage, TMembershipFunction>::Update()
{
  GenerateData();
}

template <typename TInputImage, typename TMembershipFunction>
void
ImageModelEstimatorBase<TInputImage, TMembershipFunction>::GenerateData()
{
  this->EstimateModels();
}

template <typename TInputImage, typename TMembershipFunction>
void
ImageModelEstimatorBase<TInputImage, TMembershipFunction>::PrintSelf(std::ostream & os, Indent indent) const
{
  using namespace print_helper;

  Superclass::PrintSelf(os, indent);

  os << indent << "NumberOfModels: " << m_NumberOfModels << std::endl;
  os << indent << "MembershipFunctions: " << m_MembershipFunctions << std::endl;

  itkPrintSelfObjectMacro(InputImage);
}

template <typename TInputImage, typename TMembershipFunction>
unsigned int
ImageModelEstimatorBase<TInputImage, TMembershipFunction>::AddMembershipFunction(MembershipFunctionPointer function)
{
  m_MembershipFunctions.push_back(function);
  return static_cast<unsigned int>(m_MembershipFunctions.size());
}
} // namespace itk

#endif
