/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkIntermodesThresholdImageFilter_hxx
#define itkIntermodesThresholdImageFilter_hxx

#include "itkIntermodesThresholdImageFilter.h"

namespace itk
{

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
IntermodesThresholdImageFilter<TInputImage, TOutputImage, TMaskImage>::IntermodesThresholdImageFilter()
{
  m_Calculator = IntermodesCalculatorType::New();
  this->SetCalculator(m_Calculator);
  m_Calculator->SetMaximumSmoothingIterations(10000);
  m_Calculator->SetUseInterMode(true);
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
void
IntermodesThresholdImageFilter<TInputImage, TOutputImage, TMaskImage>::SetMaximumSmoothingIterations(
  SizeValueType maxSmoothingIterations)
{
  m_Calculator->SetMaximumSmoothingIterations(maxSmoothingIterations);
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
SizeValueType
IntermodesThresholdImageFilter<TInputImage, TOutputImage, TMaskImage>::GetMaximumSmoothingIterations()
{
  return m_Calculator->GetMaximumSmoothingIterations();
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
void
IntermodesThresholdImageFilter<TInputImage, TOutputImage, TMaskImage>::SetUseInterMode(bool useIntermode)
{
  m_Calculator->SetUseInterMode(useIntermode);
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
bool
IntermodesThresholdImageFilter<TInputImage, TOutputImage, TMaskImage>::GetUseInterMode()
{
  return m_Calculator->GetUseInterMode();
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
typename IntermodesThresholdImageFilter<TInputImage, TOutputImage, TMaskImage>::CalculatorType *
IntermodesThresholdImageFilter<TInputImage, TOutputImage, TMaskImage>::GetCalculator()
{
  return dynamic_cast<CalculatorType *>(m_Calculator.GetPointer());
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
void
IntermodesThresholdImageFilter<TInputImage, TOutputImage, TMaskImage>::SetCalculator(CalculatorType * calculator)
{
  m_Calculator = dynamic_cast<IntermodesCalculatorType *>(calculator);
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
void
IntermodesThresholdImageFilter<TInputImage, TOutputImage, TMaskImage>::VerifyPreconditions() ITKv5_CONST
{
  Superclass::VerifyPreconditions();

  if (m_Calculator.IsNull())
  {
    itkExceptionMacro(<< "No threshold calculator set.");
  }
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
void
IntermodesThresholdImageFilter<TInputImage, TOutputImage, TMaskImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  itkPrintSelfObjectMacro(Calculator);
}

} // end namespace itk
#endif
