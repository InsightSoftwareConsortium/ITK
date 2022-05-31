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
#ifndef itkWaveletCoeffsSpatialDomainImageFilter_hxx
#define itkWaveletCoeffsSpatialDomainImageFilter_hxx
#include "itkForwardFFTImageFilter.h"
#include "itkInverseFFTImageFilter.h"
#include "itkFFTPadImageFilter.h"
#include "itkWaveletFrequencyForward.h"
#include "itkWaveletFrequencyInverse.h"
#include "itkWaveletFrequencyForwardUndecimated.h"
#include "itkWaveletFrequencyInverseUndecimated.h"
#include "itkWaveletFrequencyFilterBankGenerator.h"
#include "itkHeldIsotropicWavelet.h"
#include "itkVowIsotropicWavelet.h"
#include "itkSimoncelliIsotropicWavelet.h"
#include "itkShannonIsotropicWavelet.h"
#include "itkMonogenicSignalFrequencyImageFilter.h"
#include "itkVectorInverseFFTImageFilter.h"
#include "itkPhaseAnalysisSoftThresholdImageFilter.h"
#include "itkZeroDCImageFilter.h"
#include "itkImage.h"
#include "itkCastImageFilter.h"
#include "itkNumberToString.h"
#include <string>

namespace itk
{
template <typename TImageType, typename TWaveletFunction>
WaveletCoeffsSpatialDomainImageFilter<TImageType, TWaveletFunction>::WaveletCoeffsSpatialDomainImageFilter()
{
  m_Levels = 4;
  m_HighPassSubBands = 3;

  m_FFTPadFilter = FFTPadType::New();
  m_ZeroDCFilter = ZeroDCType::New();
  m_ForwardFFTFilter = FFTForwardType::New();
  m_ForwardWaveletFilter = ForwardWaveletType::New();

  m_InverseFFTFilter = InverseFFTType::New();
  m_ChangeInformationFilter = ChangeInformationType::New();
  m_CastFloatFilter = CastFloatType::New();
}

template <typename TImageType, typename TWaveletFunction>
void
WaveletCoeffsSpatialDomainImageFilter<TImageType, TWaveletFunction>::GenerateData()
{

  typename ImageType::Pointer input = ImageType::New();
  input->Graft(const_cast<ImageType *>(this->GetInput()));
  m_FFTPadFilter->SetInput(input);

  m_ZeroDCFilter->SetInput(m_FFTPadFilter->GetOutput());
  m_ForwardFFTFilter->SetInput(m_ZeroDCFilter->GetOutput());
  m_ForwardWaveletFilter->SetInput(m_ForwardFFTFilter->GetOutput());

  m_ForwardWaveletFilter->SetHighPassSubBands(this->m_HighPassSubBands);
  m_ForwardWaveletFilter->SetLevels(this->m_Levels);

  unsigned int k = (this->m_Levels * this->m_HighPassSubBands);
  this->SetNumberOfRequiredOutputs(k + 1);
  this->Modified();
  for (unsigned int i = 0; i < k + 1; ++i)
  {
    this->SetNthOutput(i, this->MakeOutput(i));
  }

  for (unsigned int i = 0; i < k + 1; ++i)
  {
    m_InverseFFTFilter->SetInput(m_ForwardWaveletFilter->GetOutput(i)); // This API is strange.
    m_ChangeInformationFilter->SetInput(m_InverseFFTFilter->GetOutput());

    m_ChangeInformationFilter->SetReferenceImage(m_FFTPadFilter->GetOutput());
    m_ChangeInformationFilter->UseReferenceImageOn();
    m_ChangeInformationFilter->ChangeAll();

    m_CastFloatFilter->SetInput(m_ChangeInformationFilter->GetOutput());

    typename ImageType::Pointer modifiedWavelets;
    // TODO Warning: InPlace here deletes buffered region of input.

    m_CastFloatFilter->InPlaceOff();
    m_CastFloatFilter->Update();
    modifiedWavelets = m_CastFloatFilter->GetOutput();

    modifiedWavelets->DisconnectPipeline();

    this->GraftNthOutput(i, modifiedWavelets);
  }
}

template <typename TImageType, typename TWaveletFunction>
void
WaveletCoeffsSpatialDomainImageFilter<TImageType, TWaveletFunction>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << " Levels: " << this->m_Levels << std::endl;
  os << indent << " HighPassSubBands: " << this->m_HighPassSubBands << std::endl;
}
} // end namespace itk
#endif
