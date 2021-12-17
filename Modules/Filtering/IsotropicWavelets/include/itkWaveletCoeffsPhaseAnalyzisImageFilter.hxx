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
#ifndef itkWaveletCoeffsPhaseAnalyzisImageFilter_hxx
#define itkWaveletCoeffsPhaseAnalyzisImageFilter_hxx
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
WaveletCoeffsPhaseAnalyzisImageFilter<TImageType, TWaveletFunction>::WaveletCoeffsPhaseAnalyzisImageFilter()
{
  m_Levels = 4;
  m_HighPassSubBands = 3;
  m_OutputIndex = 1;
  m_ApplySoftThreshold = false;
  m_ThresholdNumOfSigmas = 2.0;

  m_FFTPadFilter = FFTPadType::New();
  m_ZeroDCFilter = ZeroDCType::New();
  m_ForwardFFTFilter = FFTForwardType::New();
  m_ForwardWaveletFilter = ForwardWaveletType::New();

  m_MonogenicSignalFrequencyFilter = MonogenicSignalFrequencyType::New();
  m_VectorInverseFFTFilter = VectorInverseFFTType::New();
  m_PhaseAnalysisFilter = PhaseAnalysisType::New();
  m_FFTForwardPhaseFilter = FFTForwardType::New();

  m_InverseWaveletFilter = InverseWaveletType::New();
  m_InverseFFTFilter = InverseFFTType::New();

  m_ChangeInformationFilter = ChangeInformationType::New();
  m_CastFloatFilter = CastFloatType::New();
}

template <typename TImageType, typename TWaveletFunction>
void
WaveletCoeffsPhaseAnalyzisImageFilter<TImageType, TWaveletFunction>::GenerateData()
{
  // ====================================================================
  // ==================== Graft Input Declaration =======================
  // ====================================================================
  typename ImageType::Pointer input = ImageType::New();
  input->Graft(const_cast<ImageType *>(this->GetInput()));
  m_FFTPadFilter->SetInput(input);

  // ==================== Filter Inter Connection =======================
  m_ZeroDCFilter->SetInput(m_FFTPadFilter->GetOutput());
  m_ForwardFFTFilter->SetInput(m_ZeroDCFilter->GetOutput());
  m_ForwardWaveletFilter->SetInput(m_ForwardFFTFilter->GetOutput());

  // ==================== Filter Set Parameters =========================
  m_ForwardWaveletFilter->SetHighPassSubBands(this->m_HighPassSubBands);
  m_ForwardWaveletFilter->SetLevels(this->m_Levels);

  // ======================== Loop the Filters ==========================
  typename ForwardWaveletType::OutputsType modifiedWavelets;
  for (unsigned int i = 0; i < m_ForwardWaveletFilter->GetNumberOfOutputs(); ++i)
  {
    m_MonogenicSignalFrequencyFilter->SetInput(m_ForwardWaveletFilter->GetOutput(i)); // This API is strange.
    m_VectorInverseFFTFilter->SetInput(m_MonogenicSignalFrequencyFilter->GetOutput());
    m_PhaseAnalysisFilter->SetInput(m_VectorInverseFFTFilter->GetOutput());

    m_PhaseAnalysisFilter->SetApplySoftThreshold(this->m_ApplySoftThreshold);
    if (this->m_ApplySoftThreshold)
    {
      m_PhaseAnalysisFilter->SetNumOfSigmas(this->m_ThresholdNumOfSigmas);
    }

    m_FFTForwardPhaseFilter->SetInput(m_PhaseAnalysisFilter->GetOutputCosPhase());

    m_FFTForwardPhaseFilter->Update();
    modifiedWavelets.push_back(m_FFTForwardPhaseFilter->GetOutput());
    modifiedWavelets.back()->DisconnectPipeline();
  }

  // ==================== Filter Set Parameters =========================
  m_InverseWaveletFilter->SetHighPassSubBands(this->m_HighPassSubBands);
  m_InverseWaveletFilter->SetLevels(this->m_Levels);
  m_InverseWaveletFilter->ApplyReconstructionFactorsOff();
  m_InverseWaveletFilter->SetInputs(modifiedWavelets);

  // ==================== Filter Inter Connection =======================
  m_InverseFFTFilter->SetInput(m_InverseWaveletFilter->GetOutput());
  m_ChangeInformationFilter->SetInput(m_InverseFFTFilter->GetOutput());
  m_ChangeInformationFilter->SetReferenceImage(m_FFTPadFilter->GetOutput());
  m_ChangeInformationFilter->UseReferenceImageOn();
  m_ChangeInformationFilter->ChangeAll();
  m_CastFloatFilter->SetInput(m_ChangeInformationFilter->GetOutput());

  // ====================================================================
  // ==================== Graft Output Declaration ======================
  // ====================================================================
  m_CastFloatFilter->GraftOutput(this->GetOutput());
  m_CastFloatFilter->Update();
  this->GraftOutput(m_CastFloatFilter->GetOutput());
}

template <typename TImageType, typename TWaveletFunction>
void
WaveletCoeffsPhaseAnalyzisImageFilter<TImageType, TWaveletFunction>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << " Levels: " << this->m_Levels << std::endl;
  os << indent << " HighPassSubBands: " << this->m_HighPassSubBands << std::endl;
  os << indent << " OutputIndex: " << this->m_OutputIndex << std::endl;
  os << indent << " ApplySoftThreshold: " << m_ApplySoftThreshold << std::endl;
  os << indent << " ThresholdNumOfSigmas: " << m_ThresholdNumOfSigmas << std::endl;
}
} // end namespace itk
#endif
