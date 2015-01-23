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
#ifndef itkVotingBinaryIterativeHoleFillingImageFilter_hxx
#define itkVotingBinaryIterativeHoleFillingImageFilter_hxx
#include "itkVotingBinaryIterativeHoleFillingImageFilter.h"

#include "itkConstNeighborhoodIterator.h"
#include "itkNeighborhoodInnerProduct.h"
#include "itkImageRegionIterator.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkOffset.h"
#include "itkProgressReporter.h"

#include <vector>
#include <algorithm>

namespace itk
{
template< typename TInputImage >
VotingBinaryIterativeHoleFillingImageFilter< TInputImage >
::VotingBinaryIterativeHoleFillingImageFilter()
{
  m_Radius.Fill(1);
  m_ForegroundValue = NumericTraits< InputPixelType >::max();
  m_BackgroundValue = NumericTraits< InputPixelType >::ZeroValue();
  m_MaximumNumberOfIterations = 10;
  m_CurrentNumberOfIterations = 0;
  m_MajorityThreshold = 1;
  m_NumberOfPixelsChanged = 0;
}

template< typename TInputImage >
void
VotingBinaryIterativeHoleFillingImageFilter< TInputImage >
::GenerateData()
{
  typename InputImageType::ConstPointer input  = this->GetInput();

  m_NumberOfPixelsChanged = 0;

  typename VotingFilterType::Pointer filter = VotingFilterType::New();

  filter->SetRadius( this->GetRadius() );
  filter->SetBackgroundValue( this->GetBackgroundValue() );
  filter->SetForegroundValue( this->GetForegroundValue() );
  filter->SetMajorityThreshold( this->GetMajorityThreshold() );

  m_CurrentNumberOfIterations = 0;

  typename OutputImageType::Pointer output;

  ProgressReporter progress(this, 0, m_MaximumNumberOfIterations);

  while ( m_CurrentNumberOfIterations < m_MaximumNumberOfIterations )
    {
    filter->SetInput(input);
    filter->Update();

    m_CurrentNumberOfIterations++;
    progress.CompletedPixel();   // not really a pixel but an iteration
    this->InvokeEvent( IterationEvent() );

    const unsigned int numberOfPixelsChangedInThisIteration =
      filter->GetNumberOfPixelsChanged();
    m_NumberOfPixelsChanged += numberOfPixelsChangedInThisIteration;

    output = filter->GetOutput();
    output->DisconnectPipeline();
    input = output;
    if ( numberOfPixelsChangedInThisIteration == 0 )
      {
      break;
      }
    }
  this->GraftOutput(output);
}

/**
 * Standard "PrintSelf" method
 */
template< typename TInputImage >
void
VotingBinaryIterativeHoleFillingImageFilter< TInputImage >
::PrintSelf(
  std::ostream & os,
  Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Radius: " << m_Radius << std::endl;
  os << indent << "Foreground value : "
     << static_cast< typename NumericTraits< InputPixelType >::PrintType >( m_ForegroundValue ) << std::endl;
  os << indent << "Background value : "
     << static_cast< typename NumericTraits< InputPixelType >::PrintType >( m_BackgroundValue ) << std::endl;
  os << indent << "Maximum Number of Iterations : " << m_MaximumNumberOfIterations << std::endl;
  os << indent << "Current Number of Iterations : " << m_CurrentNumberOfIterations << std::endl;
  os << indent << "Majority Threshold           : " << m_MajorityThreshold << std::endl;
  os << indent << "Number of Pixels Changed     : " << m_NumberOfPixelsChanged << std::endl;
}
} // end namespace itk

#endif
