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

#ifndef itkLiThresholdCalculator_hxx
#define itkLiThresholdCalculator_hxx

#include "itkProgressReporter.h"
#include "itkMath.h"
#include <algorithm>

namespace itk
{

template <typename THistogram, typename TOutput>
void
LiThresholdCalculator<THistogram, TOutput>::GenerateData()
{
  const HistogramType * histogram = this->GetInput();

  if (histogram->GetTotalFrequency() == 0)
  {
    itkExceptionMacro("Histogram is empty");
  }
  const ProgressReporter progress(this, 0, histogram->GetSize(0));
  if (histogram->GetSize(0) == 1)
  {
    this->GetOutput()->Set(static_cast<OutputType>(histogram->GetMeasurement(0, 0)));
  }

  const unsigned int size = histogram->GetSize(0);


  // If there are negative values then shift the values to zero.
  const double bin_min = std::min(static_cast<double>(histogram->GetBinMin(0, 0)), 0.0);

  double tolerance = 0.5; // threshold tolerance
  int    num_pixels = histogram->GetTotalFrequency();

  // Calculate the mean gray-level
  double mean = 0.0;                                           // mean gray-level in the image
  for (int ih = 0; static_cast<unsigned int>(ih) < size; ++ih) // 0 + 1?
  {
    mean += histogram->GetMeasurement(ih, 0) * histogram->GetFrequency(ih, 0);
  }
  mean /= num_pixels;

  // Initial estimate
  long   histthresh = 0;
  double new_thresh = mean;
  double old_thresh = NAN;
  do
  {
    old_thresh = new_thresh;
    typename HistogramType::MeasurementVectorType ot(1);
    ot.Fill(static_cast<int>(old_thresh + 0.5));
    {
      typename HistogramType::IndexType local_index;
      histogram->GetIndex(ot, local_index);
      histthresh = local_index[0];

      if (histogram->IsIndexOutOfBounds(local_index))
      {
        itkWarningMacro("Unexpected histogram index out of bounds!");
        break;
      }
    }

    // Calculate the means of background and object pixels

    // Background
    double sum_back = 0; // sum of the background pixels at a given threshold
    int    num_back = 0; // number of background pixels at a given thresh
    for (int ih = 0; ih <= histthresh; ++ih)
    {
      sum_back += histogram->GetMeasurement(ih, 0) * histogram->GetFrequency(ih, 0);
      num_back += histogram->GetFrequency(ih, 0);
    }

    // mean of the background pixels at a given threshold
    double mean_back = (num_back == 0 ? 0.0 : (sum_back / static_cast<double>(num_back)));

    // Object
    // sum of the object pixels at a given threshold
    double sum_obj = 0;
    // number of object pixels at a given threshold
    int num_obj = 0;
    for (int ih = histthresh + 1; static_cast<unsigned int>(ih) < size; ++ih)
    {
      sum_obj += histogram->GetMeasurement(ih, 0) * histogram->GetFrequency(ih, 0);
      num_obj += histogram->GetFrequency(ih, 0);
    }

    // mean of the object pixels at a given threshold
    double mean_obj = (num_obj == 0 ? 0.0 : (sum_obj / static_cast<double>(num_obj)));

    // Calculate the new threshold: Equation (7) in Ref. 2
    // new_thresh = simple_round ( ( mean_back - mean_obj ) / ( Math.log ( mean_back ) - Math.log ( mean_obj ) ) );
    // simple_round ( double x ) {
    // return ( int ) ( IS_NEG ( x ) ? x - .5 : x + .5 );
    //}
    //
    // #define IS_NEG( x ) ( ( x ) < -DBL_EPSILON )
    //

    // Shift the mean by the minimum to have the range start at zero,
    // and avoid the log of a negative value.
    mean_back -= bin_min;
    mean_obj -= bin_min;
    double temp = (mean_back - mean_obj) / (std::log(mean_back) - std::log(mean_obj));

    const double epsilon = itk::NumericTraits<double>::epsilon();
    if (temp < -epsilon)
    {
      new_thresh = static_cast<int>(temp - 0.5);
    }
    else
    {
      new_thresh = static_cast<int>(temp + 0.5);
    }
    //  Stop the iterations when the difference between the new and old threshold
    // values is less than the tolerance

    // Shift the result back.
    new_thresh += bin_min;


  } while (itk::Math::abs(new_thresh - old_thresh) > tolerance);

  this->GetOutput()->Set(static_cast<OutputType>(histogram->GetMeasurement(histthresh, 0)));
}

} // end namespace itk

#endif
