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

#ifndef itkKrcahEigenToScalarPreprocessingImageToImageFilter_hxx
#define itkKrcahEigenToScalarPreprocessingImageToImageFilter_hxx

#include "itkKrcahEigenToScalarPreprocessingImageToImageFilter.h"
#include "itkMath.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage >
KrcahEigenToScalarPreprocessingImageToImageFilter< TInputImage, TOutputImage >
::KrcahEigenToScalarPreprocessingImageToImageFilter() :
  m_Sigma(1.0f),
  m_ScalingConstant(10.0f),
  m_ReleaseInternalFilterData(true)
{
  /* Only need the input image */
  this->SetNumberOfRequiredInputs(1);

  /* Instantiate all filters */
  m_GaussianFilter = GaussianFilterType::New();
  m_SubtractFilter = SubstractFilterType::New();
  m_MultiplyFilter = MultiplyFilterType::New();
  m_AddFilter = AddFilterType::New();
}

template< typename TInputImage, typename TOutputImage >
void
KrcahEigenToScalarPreprocessingImageToImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  /* Get Input */
  typename TInputImage::Pointer input = TInputImage::New();
  input->Graft( const_cast< TInputImage * >( this->GetInput() ));
  
  /* I*G */
  m_GaussianFilter->SetInput(input);
  m_GaussianFilter->SetVariance(Math::squared_magnitude(this->GetSigma()));

  /* I - I*G */
  m_SubtractFilter->SetInput1(input);
  m_SubtractFilter->SetInput2(m_GaussianFilter->GetOutput());

  /* k(I-(I*G)) */
  m_MultiplyFilter->SetInput(m_SubtractFilter->GetOutput());
  m_MultiplyFilter->SetConstant(this->GetScalingConstant());

  /* I+k*(I-(I*G)) */
  m_AddFilter->SetInput1(input);
  m_AddFilter->SetInput2(m_MultiplyFilter->GetOutput());

  /* Release data if asked */
  if (this->GetReleaseInternalFilterData())
  {
    m_GaussianFilter->ReleaseDataFlagOn();
    m_SubtractFilter->ReleaseDataFlagOn();
    m_MultiplyFilter->ReleaseDataFlagOn();
    m_AddFilter->ReleaseDataFlagOn();
  }

  /* Setup progress reporter */
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);
  progress->RegisterInternalFilter(m_GaussianFilter, 0.25);
  progress->RegisterInternalFilter(m_SubtractFilter, 0.25);
  progress->RegisterInternalFilter(m_MultiplyFilter, 0.25);
  progress->RegisterInternalFilter(m_AddFilter, 0.25);

  /* Graft Output */
  m_AddFilter->Update();
  this->GraftOutput(m_AddFilter->GetOutput());
}

template< typename TInputImage, typename TOutputImage >
void
KrcahEigenToScalarPreprocessingImageToImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "GaussianFilter: " << m_GaussianFilter.GetPointer() << std::endl;
  os << indent << "SubtractFilter: " << m_SubtractFilter.GetPointer() << std::endl;
  os << indent << "MultiplyFilter: " << m_MultiplyFilter.GetPointer() << std::endl;
  os << indent << "AddFilter: " << m_AddFilter.GetPointer() << std::endl;
  os << indent << "Sigma: " << GetSigma() << std::endl;
  os << indent << "ScalingConstant: " << GetScalingConstant() << std::endl;
  os << indent << "ReleaseInternalFilterData: " << GetReleaseInternalFilterData() << std::endl;
}

} // end namespace

#endif // itkKrcahEigenToScalarPreprocessingImageToImageFilter_hxx
