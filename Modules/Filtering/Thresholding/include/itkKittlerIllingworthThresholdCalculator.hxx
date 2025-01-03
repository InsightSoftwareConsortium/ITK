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
#ifndef itkKittlerIllingworthThresholdCalculator_hxx
#define itkKittlerIllingworthThresholdCalculator_hxx

#include "itkProgressReporter.h"
#include "itkMath.h"

namespace itk
{

template <typename THistogram, typename TOutput>
IndexValueType
KittlerIllingworthThresholdCalculator<THistogram, TOutput>::Mean()
{
  const HistogramType * data = this->GetInput();

  auto   tot = static_cast<double>(data->GetTotalFrequency());
  double sum = 0;

  for (InstanceIdentifier i = 0; i < data->GetSize(0); ++i)
  {
    sum += static_cast<double>(data->GetMeasurement(i, 0) * data->GetFrequency(i, 0));
  }
  const double mean = sum / tot;

  // search the bin corresponding to the mean value
  typename HistogramType::MeasurementVectorType v(1);
  v[0] = mean;

  typename HistogramType::IndexType idx;
  const bool                        status = data->GetIndex(v, idx);
  itkAssertInDebugAndIgnoreInReleaseMacro(status);
  if (!status)
  {
    itkExceptionMacro("Failed histogram lookup");
  }
  return idx[0];
}


template <typename THistogram, typename TOutput>
double
KittlerIllingworthThresholdCalculator<THistogram, TOutput>::A(InstanceIdentifier j)
{
  const HistogramType * y = this->GetInput();
  double                x = 0;
  for (InstanceIdentifier i = 0; i <= j; ++i)
  {
    x += static_cast<double>(y->GetFrequency(i, 0));
  }
  return x;
}

template <typename THistogram, typename TOutput>
double
KittlerIllingworthThresholdCalculator<THistogram, TOutput>::B(InstanceIdentifier j)
{
  const HistogramType * y = this->GetInput();
  double                x = 0;
  for (InstanceIdentifier i = 0; i <= j; ++i)
  {
    x += static_cast<double>(y->GetMeasurement(i, 0)) * static_cast<double>(y->GetFrequency(i, 0));
  }
  return x;
}

template <typename THistogram, typename TOutput>
double
KittlerIllingworthThresholdCalculator<THistogram, TOutput>::C(InstanceIdentifier j)
{
  const HistogramType * y = this->GetInput();
  double                x = 0;
  for (InstanceIdentifier i = 0; i <= j; ++i)
  {
    auto temp = static_cast<double>(y->GetMeasurement(i, 0));
    x += temp * temp * static_cast<double>(y->GetFrequency(i, 0));
  }
  return x;
}

template <typename THistogram, typename TOutput>
void
KittlerIllingworthThresholdCalculator<THistogram, TOutput>::GenerateData()
{
  const HistogramType * histogram = this->GetInput();

  if (histogram->GetTotalFrequency() == 0)
  {
    itkExceptionMacro("Histogram is empty");
  }
  const SizeValueType    size = histogram->GetSize(0);
  const ProgressReporter progress(this, 0, size);
  if (size == 1)
  {
    this->GetOutput()->Set(static_cast<OutputType>(histogram->GetMeasurement(0, 0)));
    return;
  }

  IndexValueType threshold = Mean(); // threshold is a histogram index
  IndexValueType Tprev = -2;

  const double As1 = A(size - 1);
  const double Bs1 = B(size - 1);
  const double Cs1 = C(size - 1);

  if (itk::Math::abs(As1) < itk::Math::eps)
  {
    itkGenericExceptionMacro("As1 = 0.");
  }

  while (threshold != Tprev)
  {
    // Calculate some statistics.
    const double At = A(threshold);
    const double Bt = B(threshold);
    const double Ct = C(threshold);

    if (itk::Math::abs(At) < itk::Math::eps)
    {
      itkGenericExceptionMacro("At = 0.");
    }
    const double mu = Bt / At;

    if (itk::Math::abs(As1 - At) < itk::Math::eps)
    {
      itkWarningMacro("KittlerIllingworthThresholdCalculator: not converging: As1 = At = " << At);
      break;
    }

    const double nu = (Bs1 - Bt) / (As1 - At);

    const double p = At / As1;
    const double q = (As1 - At) / As1;
    const double sigma2 = Ct / At - (mu * mu);
    const double tau2 = (Cs1 - Ct) / (As1 - At) - (nu * nu);

    if (sigma2 < itk::Math::eps)
    {
      itkGenericExceptionMacro("sigma2 <= 0");
    }

    if (itk::Math::abs(tau2) < itk::Math::eps)
    {
      itkGenericExceptionMacro("tau2 = 0");
    }

    if (itk::Math::abs(p) < itk::Math::eps)
    {
      itkGenericExceptionMacro("p = 0");
    }

    // The terms of the quadratic equation to be solved.
    const double w0 = 1.0 / sigma2 - 1.0 / tau2;
    const double w1 = mu / sigma2 - nu / tau2;
    const double w2 = (mu * mu) / sigma2 - (nu * nu) / tau2 + std::log10((sigma2 * (q * q)) / (tau2 * p * p));

    // If the next threshold would be imaginary, return with the current one.
    const double sqterm = w1 * w1 - w0 * w2;
    if (sqterm < itk::Math::eps)
    {
      itkWarningMacro("KittlerIllingworthThresholdCalculator: not converging.");
      break;
    }

    if (itk::Math::abs(w0) < itk::Math::eps)
    {
      const double temp = -w2 / w1;

      typename HistogramType::MeasurementVectorType v(1);
      typename HistogramType::IndexType             idx;
      v[0] = temp;
      const bool status = histogram->GetIndex(v, idx);
      itkAssertInDebugAndIgnoreInReleaseMacro(status);
      if (status)
      {
        threshold = Math::Floor<IndexValueType>(static_cast<double>(idx[0]));
      }
      else
      {
        itkExceptionMacro("KittlerIllingworthThresholdCalculator failed to lookup threshold");
      }
    }
    else
    {
      // The updated threshold is the integer part of the solution of the quadratic equation.
      Tprev = threshold;
      const double temp = (w1 + std::sqrt(sqterm)) / w0;

      // Not sure if this condition is really useful
      if (itk::Math::isnan(temp))
      {
        itkWarningMacro("KittlerIllingworthThresholdCalculator: NaN, not converging.");
        threshold = Tprev;
        break;
      }

      typename HistogramType::MeasurementVectorType v(1);
      typename HistogramType::IndexType             idx;
      v[0] = temp;
      const bool status = histogram->GetIndex(v, idx);
      itkAssertInDebugAndIgnoreInReleaseMacro(status);
      if (status)
      {
        threshold = Math::Floor<IndexValueType>(static_cast<double>(idx[0]));
      }
      else
      {
        itkExceptionMacro("KittlerIllingworthThresholdCalculator failed to lookup threshold");
      }
    }
  }
  this->GetOutput()->Set(static_cast<OutputType>(histogram->GetMeasurement(threshold, 0)));
}

} // end namespace itk

#endif
