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

#ifndef itkRenyiEntropyThresholdCalculator_hxx
#define itkRenyiEntropyThresholdCalculator_hxx

#include "itkRenyiEntropyThresholdCalculator.h"
#include "itkProgressReporter.h"
#include "itkMath.h"

namespace itk
{

template<typename THistogram, typename TOutput>
void
RenyiEntropyThresholdCalculator<THistogram, TOutput>
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

  const double tolerance = itk::Math::eps;

  InstanceIdentifier ih;

  std::vector<double> norm_histo(m_Size); /* normalized histogram */
  std::vector<double> P1(m_Size);  /* cumulative normalized histogram */
  std::vector<double> P2(m_Size);

  for( ih = 0; ih < m_Size; ih++ )
    {
    norm_histo[ih] = static_cast< double >( histogram->GetFrequency(ih, 0) ) / static_cast< double >( total );
    }

  P1[0] = norm_histo[0];
  P2[0] = 1.0 - P1[0];
  for( ih = 1; ih < m_Size; ih++ )
    {
    P1[ih] = P1[ih-1] + norm_histo[ih];
    P2[ih] = 1.0 - P1[ih];
    }

  // Determine the first non-zero bin
  m_FirstBin = 0;
  for( ih = 0; ih < m_Size; ih++ )
    {
    if( !( std::abs( P1[ih] ) < tolerance ) )
      {
      m_FirstBin = ih;
      break;
      }
    }

  // Determine the last non-zero bin
  m_LastBin = static_cast< InstanceIdentifier >( m_Size - 1 );
  for( ih = m_Size - 1; ih >= m_FirstBin; ih-- )
    {
    if( !( std::abs( P2[ih] ) < tolerance ) )
      {
      m_LastBin = ih;
      break;
      }
    }

  InstanceIdentifier t_star2 = this->MaxEntropyThresholding( histogram, norm_histo, P1, P2 );
  InstanceIdentifier t_star1 = this->MaxEntropyThresholding2( histogram, norm_histo, P1, P2 );
  InstanceIdentifier t_star3 = this->MaxEntropyThresholding3( histogram, norm_histo, P1, P2 );

  InstanceIdentifier tmp_var;

  // Sort t_star values
  if( t_star2 < t_star1 )
    {
    tmp_var = t_star1;
    t_star1 = t_star2;
    t_star2 = tmp_var;
    }
  if( t_star3 < t_star2 )
    {
    tmp_var = t_star2;
    t_star2 = t_star3;
    t_star3 = tmp_var;
    }
  if( t_star2 < t_star1 )
    {
    tmp_var = t_star1;
    t_star1 = t_star2;
    t_star2 = tmp_var;
    }

  double beta1 = 0.;
  double beta2 = 0.;
  double beta3 = 0.;

  // Adjust beta values.
  // Note that t_star1, t_star2, t_star3 are unsigned.
  if( std::abs( static_cast< double >( t_star1 ) - static_cast< double >( t_star2 ) ) <= 5. )
    {
    if( std::abs( static_cast< double >( t_star2 ) - static_cast< double >( t_star3 ) ) <= 5. )
      {
      beta1 = 1.;
      beta2 = 2.;
      beta3 = 1.;
      }
    else
      {
      beta1 = 0.;
      beta2 = 1.;
      beta3 = 3.;
      }
    }
  else
    {
    if( std::abs( static_cast< double >( t_star2 ) - static_cast< double >( t_star3 ) ) <= 5. )
      {
      beta1 = 3.;
      beta2 = 1.;
      beta3 = 0.;
      }
    else
      {
      beta1 = 1.;
      beta2 = 2.;
      beta3 = 1.;
      }
    }

  itkAssertInDebugAndIgnoreInReleaseMacro( t_star1 < m_Size );
  itkAssertInDebugAndIgnoreInReleaseMacro( t_star2 < m_Size );
  itkAssertInDebugAndIgnoreInReleaseMacro( t_star3 < m_Size );

  double omega = P1[t_star3] - P1[t_star1];

  // Determine the optimal threshold value
  double realOptThreshold = static_cast< double >( t_star1 ) * ( P1[t_star1]+ 0.25 * omega * beta1 ) +
      static_cast< double >( t_star2 ) * 0.25 * omega * beta2 +
      static_cast< double >( t_star3 ) * ( P2[t_star3] + 0.25 * omega * beta3 );

  InstanceIdentifier opt_threshold = static_cast< InstanceIdentifier >( realOptThreshold );

  this->GetOutput()->Set( static_cast<OutputType>( histogram->GetMeasurement( opt_threshold, 0 ) ) );
}

template<typename THistogram, typename TOutput>
typename RenyiEntropyThresholdCalculator<THistogram, TOutput>::InstanceIdentifier
RenyiEntropyThresholdCalculator<THistogram, TOutput>
::MaxEntropyThresholding( const HistogramType* histogram,
                          const std::vector< double >& normHisto,
                          const std::vector< double >& P1,
                          const std::vector< double >& P2 )
{

  // Calculate the total entropy each gray-level and fin the threshold that
  // maximizes it

  InstanceIdentifier threshold = 0; // was MIN_INT in original code, but if an empty image is processed it gives an error later on.
  double max_ent = NumericTraits< double >::min(); // max entropy

  for( InstanceIdentifier it = m_FirstBin; it <= m_LastBin; it++ )
    {
    // Entropy of the background pixels
    double ent_back = 0.0;
    for( InstanceIdentifier ih = 0; ih <= it; ih++ )
      {
      if( histogram->GetFrequency(ih, 0) != NumericTraits< AbsoluteFrequencyType >::ZeroValue() )
        {
        double x = ( normHisto[ih] / P1[it] );
        ent_back -= x * std::log ( x );
        }
      }

    // Entropy of the object pixels
    double ent_obj = 0.0;
    for( InstanceIdentifier ih = it + 1; ih < m_Size; ih++ )
      {
      if( histogram->GetFrequency(ih, 0) != NumericTraits< AbsoluteFrequencyType >::ZeroValue() )
        {
        double x = ( normHisto[ih] / P2[it] );
        ent_obj -= x * std::log( x );
        }
      }

    // Total entropy
    double tot_ent = ent_back + ent_obj;

    // IJ.log(""+max_ent+"  "+tot_ent);

    if( max_ent < tot_ent )
      {
      max_ent = tot_ent;
      threshold = it;
      }
    }
  return threshold;
}

template<typename THistogram, typename TOutput>
typename RenyiEntropyThresholdCalculator<THistogram, TOutput>::InstanceIdentifier
RenyiEntropyThresholdCalculator<THistogram, TOutput>
::MaxEntropyThresholding2( const HistogramType* itkNotUsed( histogram ),
                           const std::vector< double >& normHisto,
                           const std::vector< double >& P1,
                           const std::vector< double >& P2 )
{

  InstanceIdentifier threshold = 0; //was MIN_INT in original code, but if an empty image is processed it gives an error later on.
  double max_ent = NumericTraits< double >::min();
  double alpha = 0.5;
  double term = 1.0 / ( 1.0 - alpha );

  for( InstanceIdentifier it = m_FirstBin; it <= m_LastBin; it++ )
    {
    // Entropy of the background pixels
    double ent_back = 0.0;
    for( InstanceIdentifier ih = 0; ih <= it; ih++ )
      {
      ent_back += std::sqrt( normHisto[ih] / P1[it] );
      }

    // Entropy of the object pixel
    double ent_obj = 0.0;
    for( InstanceIdentifier ih = it + 1; ih < m_Size; ih++ )
      {
      ent_obj += std::sqrt( normHisto[ih] / P2[it] );
      }

    // Total entropy
    double product = ent_back * ent_obj;
    double tot_ent = 0.;

    if( product > 0.0 )
      {
      tot_ent = term * std::log( ent_back * ent_obj );
      }

    if( tot_ent > max_ent )
      {
      max_ent = tot_ent;
      threshold = it;
      }
    }

  return threshold;
}

template<typename THistogram, typename TOutput>
typename RenyiEntropyThresholdCalculator<THistogram, TOutput>::InstanceIdentifier
RenyiEntropyThresholdCalculator<THistogram, TOutput>
::MaxEntropyThresholding3( const HistogramType* itkNotUsed( histogram ),
                           const std::vector< double >& normHisto,
                           const std::vector< double >& P1,
                           const std::vector< double >& P2 )
{
  InstanceIdentifier threshold = 0; //was MIN_INT in original code, but if an empty image is processed it gives an error later on.
  double max_ent = 0.0;
  double alpha = 2.0;
  double term = 1.0 / ( 1.0 - alpha );
  for( InstanceIdentifier it = m_FirstBin; it <= m_LastBin; it++ )
    {
    // Entropy of the background pixels
    double ent_back = 0.0;
    for( InstanceIdentifier ih = 0; ih <= it; ih++ )
      {
      double x = normHisto[ih] / P1[it];
      ent_back += x * x;
      }

    // Entropy of the object pixels
    double ent_obj = 0.0;
    for( InstanceIdentifier ih = it + 1; ih < m_Size; ih++ )
      {
      double x = normHisto[ih] / P2[it];
      ent_obj += x * x;
      }

    // Total entropy
    double tot_ent = 0.0;
    double product = ent_back * ent_obj;
    if( product > 0.0 )
      {
      tot_ent = term * std::log( product );
      }

    if( tot_ent > max_ent )
      {
      max_ent = tot_ent;
      threshold = it;
      }
    }

  return threshold;
}

template<typename THistogram, typename TOutput>
void
RenyiEntropyThresholdCalculator<THistogram, TOutput>
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
