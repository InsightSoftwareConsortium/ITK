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
#ifndef itkDescoteauxEigenToScalarImageFilter_hxx
#define itkDescoteauxEigenToScalarImageFilter_hxx

#include "itkDescoteauxEigenToScalarImageFilter.h"
#include "itkProgressAccumulator.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage, typename TMaskImage >
DescoteauxEigenToScalarImageFilter< TInputImage, TOutputImage, TMaskImage >
::DescoteauxEigenToScalarImageFilter()
{
  /* Instantiate filters. */
  m_ParameterEstimationFilter               = ParameterEstimationFilterType::New();
  m_UnaryFunctorFilter                      = UnaryFunctorFilterType::New();

  /* We require an input image */
  this->SetNumberOfRequiredInputs( 1 );
}

template< typename TInputImage, typename TOutputImage, typename TMaskImage >
void
DescoteauxEigenToScalarImageFilter< TInputImage, TOutputImage, TMaskImage >
::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();

  if ( this->GetInput() )
  {
    InputImagePointer image = const_cast< typename Superclass::InputImageType * >( this->GetInput() );
    image->SetRequestedRegionToLargestPossibleRegion();
  }
  if ( this->GetMaskImage() )
  {
    MaskImagePointer mask = const_cast< TMaskImage * >( this->GetMaskImage() );
    mask->SetRequestedRegionToLargestPossibleRegion();
  }
}

template< typename TInputImage, typename TOutputImage, typename TMaskImage >
void
DescoteauxEigenToScalarImageFilter< TInputImage, TOutputImage, TMaskImage >
::EnlargeOutputRequestedRegion(DataObject *data)
{
  Superclass::EnlargeOutputRequestedRegion(data);
  data->SetRequestedRegionToLargestPossibleRegion();
}

template< typename TInputImage, typename TOutputImage, typename TMaskImage >
void
DescoteauxEigenToScalarImageFilter< TInputImage, TOutputImage, TMaskImage >
::GenerateData()
{
  /* Get inputs */
  InputImageConstPointer input = this->GetInput();
  
  /* Connect filters */
  m_ParameterEstimationFilter->SetInput(input);
  m_UnaryFunctorFilter->SetInput(m_ParameterEstimationFilter->GetOutput());
  m_UnaryFunctorFilter->SetAlphaInput(m_ParameterEstimationFilter->GetAlphaOutput());
  m_UnaryFunctorFilter->SetBetaInput(m_ParameterEstimationFilter->GetBetaOutput());
  m_UnaryFunctorFilter->SetCInput(m_ParameterEstimationFilter->GetCOutput());

  /* Setup progress reporter */
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);
  progress->RegisterInternalFilter(m_ParameterEstimationFilter, 0.5);
  progress->RegisterInternalFilter(m_UnaryFunctorFilter, 0.5);

  /* Graft output and we're done! */
  m_UnaryFunctorFilter->Update();
  this->GraftOutput(m_UnaryFunctorFilter->GetOutput());
}

template< typename TInputImage, typename TOutputImage, typename TMaskImage >
void
DescoteauxEigenToScalarImageFilter< TInputImage, TOutputImage, TMaskImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "ParameterEstimationFilter: " << m_ParameterEstimationFilter.GetPointer() << std::endl;
  os << indent << "UnaryFunctorFiler: " << m_UnaryFunctorFilter.GetPointer() << std::endl;
}

} // end namespace

#endif // itkDescoteauxEigenToScalarImageFilter_hxx 
