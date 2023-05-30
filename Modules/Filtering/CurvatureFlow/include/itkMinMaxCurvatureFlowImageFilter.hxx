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
#ifndef itkMinMaxCurvatureFlowImageFilter_hxx
#define itkMinMaxCurvatureFlowImageFilter_hxx


#include "itkMacro.h"

namespace itk
{

template <typename TInputImage, typename TOutputImage>
MinMaxCurvatureFlowImageFilter<TInputImage, TOutputImage>::MinMaxCurvatureFlowImageFilter()
{
  m_StencilRadius = 2;

  typename MinMaxCurvatureFlowFunctionType::Pointer cffp;
  cffp = MinMaxCurvatureFlowFunctionType::New();

  this->SetDifferenceFunction(static_cast<FiniteDifferenceFunctionType *>(cffp.GetPointer()));
}

template <typename TInputImage, typename TOutputImage>
void
MinMaxCurvatureFlowImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "StencilRadius: " << static_cast<typename NumericTraits<RadiusValueType>::PrintType>(m_StencilRadius)
     << std::endl;
}

template <typename TInputImage, typename TOutputImage>
void
MinMaxCurvatureFlowImageFilter<TInputImage, TOutputImage>::InitializeIteration()
{
  // update variables in the equation object
  auto * f = dynamic_cast<MinMaxCurvatureFlowFunctionType *>(this->GetDifferenceFunction().GetPointer());

  if (!f)
  {
    itkExceptionMacro("DifferenceFunction not of type MinMaxCurvatureFlowFunction");
  }

  f->SetStencilRadius(m_StencilRadius);
  this->Superclass::InitializeIteration();
}
} // end namespace itk

#endif
