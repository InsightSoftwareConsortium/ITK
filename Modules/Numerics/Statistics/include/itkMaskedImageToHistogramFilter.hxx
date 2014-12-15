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
#ifndef itkMaskedImageToHistogramFilter_hxx
#define itkMaskedImageToHistogramFilter_hxx

#include "itkMaskedImageToHistogramFilter.h"
#include "itkProgressReporter.h"
#include "itkImageRegionConstIterator.h"

namespace itk
{
namespace Statistics
{
template< typename TImage, typename TMaskImage >
MaskedImageToHistogramFilter< TImage, TMaskImage >
::MaskedImageToHistogramFilter()
{
//  this->SetNumberOfRequiredInputs(2);
  this->SetMaskValue( NumericTraits<MaskPixelType>::max() );
}

template< typename TImage, typename TMaskImage >
void
MaskedImageToHistogramFilter< TImage, TMaskImage >
::ThreadedComputeMinimumAndMaximum( const RegionType & inputRegionForThread, ThreadIdType threadId, ProgressReporter & progress )
{
  unsigned int nbOfComponents = this->GetInput()->GetNumberOfComponentsPerPixel();
  HistogramMeasurementVectorType min( nbOfComponents );
  HistogramMeasurementVectorType max( nbOfComponents );

  MaskPixelType maskValue = this->GetMaskValue();

  ImageRegionConstIterator< TImage > inputIt( this->GetInput(), inputRegionForThread );
  ImageRegionConstIterator< TMaskImage > maskIt( this->GetMaskImage(), inputRegionForThread );
  inputIt.GoToBegin();
  maskIt.GoToBegin();
  HistogramMeasurementVectorType m( nbOfComponents );

  min.Fill( NumericTraits<ValueType>::max() );
  max.Fill( NumericTraits<ValueType>::NonpositiveMin() );
  while ( !inputIt.IsAtEnd() )
    {
    if( maskIt.Get() == maskValue )
      {
      const PixelType & p = inputIt.Get();
      NumericTraits<PixelType>::AssignToArray( p, m );
      for( unsigned int i=0; i<nbOfComponents; i++ )
        {
        min[i] = std::min( m[i], min[i] );
        max[i] = std::max( m[i], max[i] );
        }
      }
    ++inputIt;
    ++maskIt;
    progress.CompletedPixel();  // potential exception thrown here
    }
  this->m_Minimums[threadId] = min;
  this->m_Maximums[threadId] = max;
}

template< typename TImage, typename TMaskImage >
void
MaskedImageToHistogramFilter< TImage, TMaskImage >
::ThreadedComputeHistogram(const RegionType & inputRegionForThread, ThreadIdType threadId, ProgressReporter & progress )
{
  unsigned int nbOfComponents = this->GetInput()->GetNumberOfComponentsPerPixel();
  ImageRegionConstIterator< TImage > inputIt( this->GetInput(), inputRegionForThread );
  ImageRegionConstIterator< TMaskImage > maskIt( this->GetMaskImage(), inputRegionForThread );
  inputIt.GoToBegin();
  maskIt.GoToBegin();
  HistogramMeasurementVectorType m( nbOfComponents );
  MaskPixelType maskValue = this->GetMaskValue();

  typename HistogramType::IndexType index;
  while ( !inputIt.IsAtEnd() )
    {
    if( maskIt.Get() == maskValue )
      {
      const PixelType & p = inputIt.Get();
      NumericTraits<PixelType>::AssignToArray( p, m );
      this->m_Histograms[threadId]->GetIndex( m, index );
      this->m_Histograms[threadId]->IncreaseFrequencyOfIndex( index, 1 );
      }
    ++inputIt;
    ++maskIt;
    progress.CompletedPixel();  // potential exception thrown here
    }
}

} // end of namespace Statistics
} // end of namespace itk

#endif
