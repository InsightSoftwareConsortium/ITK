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
#ifndef __itkKittlerIllingworthThresholdCalculator_hxx
#define __itkKittlerIllingworthThresholdCalculator_hxx

#include "itkKittlerIllingworthThresholdCalculator.h"
#include "itkProgressReporter.h"
#include "vnl/vnl_math.h"
#include "itkMath.h"

namespace itk
{

template<class THistogram, class TOutput>
int
KittlerIllingworthThresholdCalculator<THistogram, TOutput>
::Mean()
{
  const HistogramType * data = this->GetInput();
  double tot=0, sum=0;
  tot = data->GetTotalFrequency();
  for (unsigned i=0; i<data->GetSize(0); i++)
    {
    sum += (data->GetMeasurement(i,0)*data->GetFrequency(i,0));
    }
  double mean = sum/tot;
  // search the bin corresponding to the mean value
  typename HistogramType::MeasurementVectorType v(1);
  v[0] = mean;
  return data->GetIndex(v)[0];
}


template<class THistogram, class TOutput>
double
KittlerIllingworthThresholdCalculator<THistogram, TOutput>
::A( int j)
{
  const HistogramType * y = this->GetInput();
  double x = 0;
  for (int i = 0; i<=j; i++)
    x += y->GetFrequency(i,0);
  return x;
}

template<class THistogram, class TOutput>
double
KittlerIllingworthThresholdCalculator<THistogram, TOutput>
::B( int j)
{
  const HistogramType * y = this->GetInput();
  double x = 0;
  for( int i=0; i<=j; i++ )
    x += y->GetMeasurement(i,0)*y->GetFrequency(i,0);
  return x;
}

template<class THistogram, class TOutput>
double
KittlerIllingworthThresholdCalculator<THistogram, TOutput>
::C( int j)
{
  const HistogramType * y = this->GetInput();
  double x = 0;
  for( int i = 0; i<=j; i++ )
    x += y->GetMeasurement(i,0)*y->GetMeasurement(i,0)*y->GetFrequency(i,0);
  return x;
}


/*
 * Compute the KittlerIllingworth's threshold
 */
template<class THistogram, class TOutput>
void
KittlerIllingworthThresholdCalculator<THistogram, TOutput>
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

  int threshold = Mean(); // threshold is a histogram index
  int Tprev =-2;
  double mu, nu, p, q, sigma2, tau2, w0, w1, w2, sqterm, temp;
  //int counter=1;
  while (threshold!=Tprev)
    {
    //Calculate some statistics.
    double At = A(threshold);
    double Bt = B(threshold);
    double Ct = C(threshold);
    double As1 = A(size - 1);
    double Bs1 = B(size - 1);
    double Cs1 = C(size - 1);

    mu = Bt/At;
    nu = (Bs1-Bt)/(As1-At);
    p = At/As1;
    q = (As1-At) / As1;
    sigma2 = Ct/At-(mu*mu);
    tau2 = (Cs1-Ct) / (As1-At) - (nu*nu);

    //The terms of the quadratic equation to be solved.
    w0 = 1.0/sigma2-1.0/tau2;
    w1 = mu/sigma2-nu/tau2;
    w2 = (mu*mu)/sigma2 - (nu*nu)/tau2 + vcl_log10((sigma2*(q*q))/(tau2*(p*p)));

    //If the next threshold would be imaginary, return with the current one.
    sqterm = (w1*w1)-w0*w2;
    if (sqterm < 0)
      {
      itkWarningMacro( << "KittlerIllingworthThresholdCalculator: not converging.");
      this->GetOutput()->Set( static_cast<OutputType>( histogram->GetMeasurement( threshold, 0 ) ) );
      return;
      }

    //The updated threshold is the integer part of the solution of the quadratic equation.
    Tprev = threshold;
    temp = (w1+vcl_sqrt(sqterm))/w0;

    if (vnl_math_isnan(temp))
      {
      itkWarningMacro (<< "KittlerIllingworthThresholdCalculator: NaN, not converging.");
      threshold = Tprev;
      }
    else
      {
      typename HistogramType::MeasurementVectorType v(1);
      v[0] = temp;
      threshold = Math::Floor<int>((double)histogram->GetIndex(v)[0]);
      }
  }
  this->GetOutput()->Set( static_cast<OutputType>( histogram->GetMeasurement( threshold, 0 ) ) );
}

} // end namespace itk

#endif
