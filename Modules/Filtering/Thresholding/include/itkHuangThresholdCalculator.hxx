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

#ifndef itkHuangThresholdCalculator_hxx
#define itkHuangThresholdCalculator_hxx

#include "itkHuangThresholdCalculator.h"
#include "itkMath.h"
#include "itkProgressReporter.h"
#include "itkMath.h"

namespace itk
{

template<typename THistogram, typename TOutput>
void
HuangThresholdCalculator<THistogram, TOutput>
::GenerateData(void)
{
  const HistogramType * histogram = this->GetInput();

  TotalAbsoluteFrequencyType total = histogram->GetTotalFrequency();
  if( total == NumericTraits< TotalAbsoluteFrequencyType >::ZeroValue() )
    {
    itkExceptionMacro(<< "Histogram is empty");
    }
  m_Size = histogram->GetSize( 0 );
  ProgressReporter progress( this, 0, m_Size );
  if( m_Size == 1 )
    {
    this->GetOutput()->Set( static_cast<OutputType>(histogram->GetMeasurement( 0, 0 )) );
    return;
    }

  // Find first and last non-empty bin - could replace with stl
  m_FirstBin = 0;
  while( m_FirstBin < m_Size && histogram->GetFrequency(m_FirstBin, 0) == 0 )
    {
    ++m_FirstBin;
    }
  if( m_FirstBin == m_Size )
    {
    itkWarningMacro(<< "No data in histogram");
    return;
    }
  m_LastBin = m_Size - 1;
  while( m_LastBin > m_FirstBin && histogram->GetFrequency(m_LastBin, 0) == 0)
    {
    --m_LastBin;
    }

  // Calculate the cumulative density and the weighted cumulative density
  std::vector<double> S(m_LastBin + 1, 0.0);
  std::vector<double> W(m_LastBin + 1, 0.0);

  S[0] = histogram->GetFrequency(0, 0);

  for( InstanceIdentifier i = std::max( NumericTraits< InstanceIdentifier >::OneValue(), m_FirstBin );
       i <= m_LastBin; i++ )
    {
    S[i] = S[i - 1] + histogram->GetFrequency(i, 0);
    W[i] = W[i - 1] + histogram->GetMeasurement(i, 0) * histogram->GetFrequency(i, 0);
    }

  // precalculate the summands of the entropy given the absolute difference x - mu (integral)
  double C = static_cast< double >( m_LastBin - m_FirstBin );
  std::vector<double> Smu(m_LastBin + 1 - m_FirstBin, 0);

  for( size_t i = 1; i < Smu.size(); i++)
    {
    double mu = 1. / ( 1. + static_cast< double >( i ) / C );
    Smu[i] = -mu * std::log( mu ) - (1. - mu) * std::log( 1. - mu );
    }

  // Calculate the threshold.
  // Need to take care - W[0] is zero, which means that mu=0 first
  // time round. This may be below the range of values in the
  // histogram, especially if masking is in place.
  // Also need to be careful at the end. The Java implementation from
  // ImageJ loops from first to last, not < last. This makes the
  // calculation of mu a bit silly :
  // mu = Math::Round<int>((W[last] - W[threshold]) / (S[last] - S[threshold]));
  // which is going to produce 0/0. Hence the loop bounds have been
  // changed. I think there is a bug in the ImageJ implementation, but
  // perhaps it is hidden by whatever java does when rounding a NaN to integer.

  InstanceIdentifier bestThreshold = 0;
  double bestEntropy = itk::NumericTraits<double>::max();

  for( InstanceIdentifier threshold = m_FirstBin;
       threshold < m_LastBin; threshold++ )
    {
    double entropy = 0.;
    MeasurementType mu = Math::Round< MeasurementType >(W[threshold] / S[threshold]);

    typename HistogramType::MeasurementVectorType v(1);
    v[0] = mu;

    typename HistogramType::IndexType       muFullIdx;
    typename HistogramType::IndexValueType  muIdx;

    if (histogram->GetIndex(v, muFullIdx))
      {
      muIdx = muFullIdx[0];
      for( InstanceIdentifier i = m_FirstBin; i <= threshold; i++ )
        {
        const typename HistogramType::IndexValueType signedDiff = static_cast< typename HistogramType::IndexValueType >( i ) - muIdx;
        const InstanceIdentifier diff = static_cast< InstanceIdentifier >( signedDiff < 0 ? -signedDiff : signedDiff );
        itkAssertInDebugAndIgnoreInReleaseMacro( diff < Smu.size() );

        entropy += Smu[ diff ] * histogram->GetFrequency(i, 0);
        }
      mu = Math::Round< MeasurementType >((W[m_LastBin] - W[threshold]) / (S[m_LastBin] - S[threshold]));
      v[0] = mu;

      bool status = histogram->GetIndex(v, muFullIdx);
      if (!status)
        {
        itkExceptionMacro("Failed looking up histogram");
        }
      muIdx = muFullIdx[0];
      for( InstanceIdentifier i = threshold + 1; i <= m_LastBin; i++ )
        {
        const typename HistogramType::IndexValueType signedDiff = static_cast< typename HistogramType::IndexValueType >( i ) - muIdx;
        const InstanceIdentifier diff = static_cast< InstanceIdentifier >( signedDiff < 0 ? -signedDiff : signedDiff );
        entropy += Smu[ diff ] * histogram->GetFrequency(i, 0);
        }
      if (bestEntropy > entropy)
        {
        bestEntropy = entropy;
        bestThreshold = threshold;
        }
      }
    }

  this->GetOutput()->Set( static_cast<OutputType>( histogram->GetMeasurement( bestThreshold, 0 ) ) );
}

template<typename THistogram, typename TOutput>
void
HuangThresholdCalculator<THistogram, TOutput>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FirstBin: " << static_cast< typename itk::NumericTraits<
    InstanceIdentifier >::PrintType >( m_FirstBin ) << std::endl;
  os << indent << "LastBin: " << static_cast< typename itk::NumericTraits<
    InstanceIdentifier >::PrintType >( m_LastBin ) << std::endl;
  os << indent << "Size: " << static_cast< typename itk::NumericTraits<
    SizeValueType >::PrintType >( m_Size ) << std::endl;
}
} // end namespace itk

#endif
