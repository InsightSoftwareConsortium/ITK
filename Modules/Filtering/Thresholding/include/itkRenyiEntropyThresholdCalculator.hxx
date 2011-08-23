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

#ifndef __itkRenyiEntropyThresholdCalculator_hxx
#define __itkRenyiEntropyThresholdCalculator_hxx

#include "itkRenyiEntropyThresholdCalculator.h"
#include "itkProgressReporter.h"
#include "vnl/vnl_math.h"

namespace itk
{
/*
 * Compute the RenyiEntropy's threshold
 */
template<class THistogram, class TOutput>
void
RenyiEntropyThresholdCalculator<THistogram, TOutput>
::GenerateData(void)
{
  const HistogramType * histogram = this->GetInput();
  // histogram->Print(std::cout);
  if ( histogram->GetTotalFrequency() == 0 )
    {
    itkExceptionMacro(<< "Histogram is empty");
    }
  ProgressReporter progress(this, 0, histogram->GetSize(0) );
  if( histogram->GetSize(0) == 1 )
    {
    this->GetOutput()->Set( histogram->GetMeasurement(0,0) );
    }

  unsigned int size = histogram->GetSize(0);

  const double tolerance = 2.220446049250313E-16;
  int threshold;
  int opt_threshold;

  int ih, it;
  int first_bin;
  int last_bin;
  int tmp_var;
  int t_star1, t_star2, t_star3;
  int beta1, beta2, beta3;
  double alpha;/* alpha parameter of the method */
  double term;
  double tot_ent;  /* total entropy */
  double max_ent;  /* max entropy */
  double ent_back; /* entropy of the background pixels at a given threshold */
  double ent_obj;  /* entropy of the object pixels at a given threshold */
  double omega;
  std::vector<double> norm_histo(size); /* normalized histogram */
  std::vector<double> P1(size);  /* cumulative normalized histogram */
  std::vector<double> P2(size);

  int total =0;
  for (ih = 0; (unsigned)ih < size; ih++ )
    {
    total += histogram->GetFrequency(ih, 0);
    }

  for (ih = 0; (unsigned)ih < size; ih++ )
    norm_histo[ih] = (double)histogram->GetFrequency(ih, 0)/total;

  P1[0]=norm_histo[0];
  P2[0]=1.0-P1[0];
  for (ih = 1; (unsigned)ih < size; ih++ )
    {
    P1[ih]= P1[ih-1] + norm_histo[ih];
    P2[ih]= 1.0 - P1[ih];
    }

  /* Determine the first non-zero bin */
  first_bin=0;
  for (ih = 0; (unsigned)ih < size; ih++ )
    {
    if ( !(vcl_abs(P1[ih])<tolerance))
      {
      first_bin = ih;
      break;
      }
    }

  /* Determine the last non-zero bin */
  last_bin=size - 1;
  for (ih = size - 1; ih >= first_bin; ih-- )
    {
    if ( !(vcl_abs(P2[ih])<tolerance))
    {
    last_bin = ih;
    break;
    }
    }

  /* Maximum Entropy Thresholding - BEGIN */
  /* ALPHA = 1.0 */
  /* Calculate the total entropy each gray-level
     and find the threshold that maximizes it
  */
  threshold =0; // was MIN_INT in original code, but if an empty image is processed it gives an error later on.
  max_ent = 0.0;

  for ( it = first_bin; it <= last_bin; it++ )
    {
    /* Entropy of the background pixels */
    ent_back = 0.0;
    for ( ih = 0; ih <= it; ih++ )
      {
      if ( histogram->GetFrequency(ih, 0) != 0 )
        {
        ent_back -= ( norm_histo[ih] / P1[it] ) * vcl_log ( norm_histo[ih] / P1[it] );
        }
      }

    /* Entropy of the object pixels */
    ent_obj = 0.0;
    for ( ih = it + 1; (unsigned)ih < size; ih++ )
      {
      if (histogram->GetFrequency(ih, 0) != 0)
        {
        ent_obj -= ( norm_histo[ih] / P2[it] ) * vcl_log ( norm_histo[ih] / P2[it] );
        }
      }

    /* Total entropy */
    tot_ent = ent_back + ent_obj;

    // IJ.log(""+max_ent+"  "+tot_ent);

    if ( max_ent < tot_ent )
      {
      max_ent = tot_ent;
      threshold = it;
      }
    }
  t_star2 = threshold;

  /* Maximum Entropy Thresholding - END */
  threshold =0; //was MIN_INT in original code, but if an empty image is processed it gives an error later on.
  max_ent = 0.0;
  alpha = 0.5;
  term = 1.0 / ( 1.0 - alpha );
  for ( it = first_bin; it <= last_bin; it++ )
    {
    /* Entropy of the background pixels */
    ent_back = 0.0;
    for ( ih = 0; ih <= it; ih++ )
      ent_back += vcl_sqrt ( norm_histo[ih] / P1[it] );

    /* Entropy of the object pixels */
    ent_obj = 0.0;
    for ( ih = it + 1; (unsigned)ih < size; ih++ )
      ent_obj += vcl_sqrt ( norm_histo[ih] / P2[it] );

    /* Total entropy */
    tot_ent = term * ( ( ent_back * ent_obj ) > 0.0 ? vcl_log ( ent_back * ent_obj ) : 0.0);

    if ( tot_ent > max_ent )
      {
      max_ent = tot_ent;
      threshold = it;
      }
    }

  t_star1 = threshold;

  threshold = 0; //was MIN_INT in original code, but if an empty image is processed it gives an error later on.
  max_ent = 0.0;
  alpha = 2.0;
  term = 1.0 / ( 1.0 - alpha );
  for ( it = first_bin; it <= last_bin; it++ )
    {
    /* Entropy of the background pixels */
    ent_back = 0.0;
    for ( ih = 0; ih <= it; ih++ )
      ent_back += ( norm_histo[ih] * norm_histo[ih] ) / ( P1[it] * P1[it] );

    /* Entropy of the object pixels */
    ent_obj = 0.0;
    for ( ih = it + 1; (unsigned)ih < size; ih++ )
      ent_obj += ( norm_histo[ih] * norm_histo[ih] ) / ( P2[it] * P2[it] );

    /* Total entropy */
    tot_ent = term *( ( ent_back * ent_obj ) > 0.0 ? vcl_log(ent_back * ent_obj ): 0.0 );

    if ( tot_ent > max_ent )
      {
      max_ent = tot_ent;
      threshold = it;
      }
    }

  t_star3 = threshold;

  /* Sort t_star values */
  if ( t_star2 < t_star1 )
    {
    tmp_var = t_star1;
    t_star1 = t_star2;
    t_star2 = tmp_var;
    }
  if ( t_star3 < t_star2 )
    {
    tmp_var = t_star2;
    t_star2 = t_star3;
    t_star3 = tmp_var;
    }
  if ( t_star2 < t_star1 )
    {
    tmp_var = t_star1;
    t_star1 = t_star2;
    t_star2 = tmp_var;
    }

  /* Adjust beta values */
  if ( vcl_abs ( t_star1 - t_star2 ) <= 5 )
    {
    if ( vcl_abs ( t_star2 - t_star3 ) <= 5 )
      {
      beta1 = 1;
      beta2 = 2;
      beta3 = 1;
      }
    else
      {
      beta1 = 0;
      beta2 = 1;
      beta3 = 3;
      }
    }
  else
    {
    if ( vcl_abs ( t_star2 - t_star3 ) <= 5 )
      {
      beta1 = 3;
      beta2 = 1;
      beta3 = 0;
      }
    else
      {
      beta1 = 1;
      beta2 = 2;
      beta3 = 1;
      }
    }
  //IJ.log(""+t_star1+" "+t_star2+" "+t_star3);
  /* Determine the optimal threshold value */
  omega = P1[t_star3] - P1[t_star1];
  opt_threshold = (int) (t_star1 * ( P1[t_star1] + 0.25 * omega * beta1 ) + 0.25 * t_star2 * omega * beta2  + t_star3 * ( P2[t_star3] + 0.25 * omega * beta3 ));


  this->GetOutput()->Set( static_cast<OutputType>( histogram->GetMeasurement( opt_threshold, 0 ) ) );
}

} // end namespace itk

#endif
