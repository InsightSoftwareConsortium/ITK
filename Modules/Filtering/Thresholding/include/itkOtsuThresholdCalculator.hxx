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
#ifndef itkOtsuThresholdCalculator_hxx
#define itkOtsuThresholdCalculator_hxx

#include "itkOtsuThresholdCalculator.h"
#include "itkMath.h"

namespace itk
{
template< typename THistogram, typename TOutput >
void
OtsuThresholdCalculator< THistogram, TOutput >
::GenerateData(void)
{
  this->UpdateProgress(0.0);
  // Compute the Otsu threshold using the OtsuMultipleThresholdsCalculator to ensure code reusability.
  m_OtsuMultipleThresholdsCalculator->SetInputHistogram( this->GetInput() );
  m_OtsuMultipleThresholdsCalculator->SetNumberOfThresholds( 1 );
  m_OtsuMultipleThresholdsCalculator->Compute();
  this->GetOutput()->Set( static_cast<OutputType>( m_OtsuMultipleThresholdsCalculator->GetOutput()[0] ) );
  this->UpdateProgress(1.0);
}


template< typename THistogram, typename TOutput >
void
OtsuThresholdCalculator< THistogram, TOutput >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  itkPrintSelfObjectMacro(OtsuMultipleThresholdsCalculator);
}

} // end namespace itk

#endif
