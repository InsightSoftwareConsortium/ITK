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
#ifndef itkNormalizeImageFilter_hxx
#define itkNormalizeImageFilter_hxx

#include "itkNormalizeImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkProgressAccumulator.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage >
NormalizeImageFilter< TInputImage, TOutputImage >
::NormalizeImageFilter()
{
  m_StatisticsFilter = ITK_NULLPTR;
  m_StatisticsFilter = StatisticsImageFilter< TInputImage >::New();
  m_ShiftScaleFilter = ShiftScaleImageFilter< TInputImage, TOutputImage >::New();
}

template< typename TInputImage, typename TOutputImage >
void
NormalizeImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();
  if ( this->GetInput() )
    {
    InputImagePointer image =
      const_cast< typename Superclass::InputImageType * >( this->GetInput() );
    image->SetRequestedRegionToLargestPossibleRegion();
    }
}

template< typename TInputImage, typename TOutputImage >
void
NormalizeImageFilter< TInputImage, TOutputImage >
::Modified() const
{
  Superclass::Modified();
  m_StatisticsFilter->Modified();
  m_ShiftScaleFilter->Modified();
}

template< typename TInputImage, typename TOutputImage >
void
NormalizeImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();

  progress->SetMiniPipelineFilter(this);

  progress->RegisterInternalFilter(m_StatisticsFilter, .5f);
  progress->RegisterInternalFilter(m_ShiftScaleFilter, .5f);

  // Gather statistics

  m_StatisticsFilter->SetInput( this->GetInput() );
  m_StatisticsFilter->GetOutput()->SetRequestedRegion( this->GetOutput()->GetRequestedRegion() );
  m_StatisticsFilter->Update();

  // Set the parameters for Shift
  m_ShiftScaleFilter->SetShift( -m_StatisticsFilter->GetMean() );
  m_ShiftScaleFilter->SetScale( NumericTraits< typename StatisticsImageFilter< TInputImage >::RealType >::OneValue()
                                / m_StatisticsFilter->GetSigma() );
  m_ShiftScaleFilter->SetInput( this->GetInput() );

  m_ShiftScaleFilter->GetOutput()->SetRequestedRegion( this->GetOutput()->GetRequestedRegion() );
  m_ShiftScaleFilter->Update();

  // Graft the mini pipeline output to this filters output
  this->GraftOutput( m_ShiftScaleFilter->GetOutput() );
}
} // end namespace itk

#endif
