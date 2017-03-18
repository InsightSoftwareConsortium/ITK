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

#ifndef itkLiThresholdCalculator_hxx
#define itkLiThresholdCalculator_hxx

#include "itkLiThresholdCalculator.h"
#include "itkProgressReporter.h"
#include "itkMath.h"

namespace itk
{

template<typename THistogram, typename TOutput>
void
LiThresholdCalculator<THistogram, TOutput>
::GenerateData(void)
{
  const HistogramType * histogram = this->GetInput();

  if ( histogram->GetTotalFrequency() == 0 )
    {
    itkExceptionMacro(<< "Histogram is empty");
    }
  ProgressReporter progress(this, 0, histogram->GetSize(0) );
  if( histogram->GetSize(0) == 1 )
    {
    this->GetOutput()->Set( static_cast<OutputType>( histogram->GetMeasurement(0,0) ) );
    }

  unsigned int size = histogram->GetSize(0);

  long int histthresh;
  int ih;
  int num_pixels;
  double sum_back; // sum of the background pixels at a given threshold
  double sum_obj;  // sum of the object pixels at a given threshold
  int num_back; // number of background pixels at a given threshold
  int num_obj;  // number of object pixels at a given threshold
  double old_thresh;
  double new_thresh;
  double mean_back; // mean of the background pixels at a given threshold
  double mean_obj;  // mean of the object pixels at a given threshold
  double mean;  // mean gray-level in the image
  double tolerance; // threshold tolerance
  double temp;

  tolerance = 0.5;
  num_pixels = histogram->GetTotalFrequency();

  // Calculate the mean gray-level
  mean = 0.0;
  for ( ih = 0; (unsigned)ih < size; ih++ ) //0 + 1?
    {
    mean += histogram->GetMeasurement(ih, 0) * histogram->GetFrequency(ih, 0);
    }
  mean /= num_pixels;

  // Initial estimate
  new_thresh = mean;

  do {
  old_thresh = new_thresh;
  typename HistogramType::MeasurementVectorType ot(1);
  ot.Fill((int) (old_thresh + 0.5));
    {
    typename HistogramType::IndexType local_index;
    histogram->GetIndex(ot, local_index);
    histthresh = local_index[0];
    }

  // Calculate the means of background and object pixels

  // Background
  sum_back = 0;
  num_back = 0;
  for ( ih = 0; ih <= histthresh; ih++ )
    {
    sum_back += histogram->GetMeasurement(ih, 0) * histogram->GetFrequency(ih, 0);
    num_back += histogram->GetFrequency(ih, 0);
    }
  mean_back = ( num_back == 0 ? 0.0 : ( sum_back / ( double ) num_back ) );

  // Object
  sum_obj = 0;
  num_obj = 0;
  for ( ih = histthresh + 1; (unsigned)ih < size; ih++ )
    {
    sum_obj += histogram->GetMeasurement(ih, 0) * histogram->GetFrequency(ih, 0);
    num_obj += histogram->GetFrequency(ih, 0);
    }
  mean_obj = ( num_obj == 0 ? 0.0 : ( sum_obj / ( double ) num_obj ) );

  // Calculate the new threshold: Equation (7) in Ref. 2
  //new_thresh = simple_round ( ( mean_back - mean_obj ) / ( Math.log ( mean_back ) - Math.log ( mean_obj ) ) );
  //simple_round ( double x ) {
  // return ( int ) ( IS_NEG ( x ) ? x - .5 : x + .5 );
  //}
  //
  //#define IS_NEG( x ) ( ( x ) < -DBL_EPSILON )
  //
  temp = ( mean_back - mean_obj ) / ( std::log ( mean_back ) - std::log ( mean_obj ) );

  double epsilon = itk::NumericTraits<double>::epsilon();
  if( temp < -epsilon )
    {
    new_thresh = (int) (temp - 0.5);
    }
  else
    {
    new_thresh = (int) (temp + 0.5);
    }
  //  Stop the iterations when the difference between the new and old threshold
  // values is less than the tolerance
  }
  while ( std::abs ( new_thresh - old_thresh ) > tolerance );

  this->GetOutput()->Set( static_cast<OutputType>( histogram->GetMeasurement( histthresh, 0 ) ) );
}

} // end namespace itk

#endif
