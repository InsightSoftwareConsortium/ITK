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
#ifndef itkVariationalRegistrationStopCriterion_hxx
#define itkVariationalRegistrationStopCriterion_hxx

#include "itkVariationalRegistrationStopCriterion.h"

#include "itkMath.h"

namespace itk
{

/**
 * Default constructor
 */
template <typename TRegistrationFilter, typename TMRFilter>
VariationalRegistrationStopCriterion<TRegistrationFilter, TMRFilter>::VariationalRegistrationStopCriterion()
{
  // Set MR strategy to default
  m_MultiResolutionPolicy = MULTI_RESOLUTION_POLICY_DEFAULT;

  // Initialize general parameters.
  m_ElapsedIterations = 0;
  m_IterationModulus = 1;

  // Initialize increase count parameters.
  m_PerformIncreaseCountCheck = false;
  m_MaximumIncreaseCount = 10;
  m_CurrentIncreaseCount = 0;

  // Initialize line fitting parameters.
  m_PerformLineFittingCheck = false;
  m_PerformLineFittingMaxDistanceCheck = false;
  m_LineFittingUseAbsoluteValues = false;

  m_LineFittingMode = LINE_FITTING_MODE_ORIGINAL;
  m_NumberOfFittingIterations = 20;
  m_RegressionLineSlopeThreshold = 0.005;
  m_MaxDistanceToRegressionLine = 0.005;

  m_IterationArray = nullptr;
  m_DistanceArray = nullptr;
  m_DistanceArrayForFitting = nullptr;

  m_MaxMetricValue = NumericTraits<double>::min();
  m_MinMetricValue = NumericTraits<double>::max();

  this->ResetFittingData();
}

/**
 * Default destructor
 */
template <typename TRegistrationFilter, typename TMRFilter>
VariationalRegistrationStopCriterion<TRegistrationFilter, TMRFilter>::~VariationalRegistrationStopCriterion()
{
  if (m_IterationArray != nullptr)
    delete[] m_IterationArray;
  if (m_DistanceArray != nullptr)
    delete[] m_DistanceArray;
  if (m_DistanceArrayForFitting != nullptr)
    delete[] m_DistanceArrayForFitting;
}

/**
 * Not implemented because registration filter must not be const to
 * return the result of check
 */
template <typename TRegistrationFilter, typename TMRFilter>
void
VariationalRegistrationStopCriterion<TRegistrationFilter, TMRFilter>::Execute(
  const itk::Object *      itkNotUsed(caller),
  const itk::EventObject & itkNotUsed(event))
{
  itkWarningMacro(<< "Execute called with const Filter, no check performed.");
}

/**
 * Handle iteration event to check if stop criterion is fulfilled
 */
template <typename TRegistrationFilter, typename TMRFilter>
void
VariationalRegistrationStopCriterion<TRegistrationFilter, TMRFilter>::Execute(itk::Object *            caller,
                                                                              const itk::EventObject & event)
{
  // If event is an iteration event, check if thrown by registration
  // or multi resolution filter
  if (itk::IterationEvent().CheckEvent(&event))
  {
    // Cast caller for subsequent check
    auto * regFilter = dynamic_cast<RegistrationFilterType *>(caller);

    auto * mrFilter = dynamic_cast<MRFilterType *>(caller);

    // If caller is MR filter, set mode for next level according to
    // MR policy
    if (mrFilter)
    {
      // Set the mode for the next level according to MR policy
      this->SetModeForNextLevel(mrFilter->GetElapsedLevels(), mrFilter->GetNumberOfLevels());

      // Reset data before new measurement
      this->ResetFittingData();
    }

    // If caller is registration filter, log metric of last iteration
    // and check, if stop criterion is fulfilled
    else if (regFilter)
    {
      // Set next metric value
      this->SetNextMetricValue(regFilter->GetMetric());

      // Perform check and stop registration if check was positive
      if (this->CheckStopRegistration())
      {
        regFilter->StopRegistration();
      }
    }
  }

  // If initialize event called by MR filter, set MR mode for first
  // level according to MR policy
  else if (itk::InitializeEvent().CheckEvent(&event))
  {
    auto * mrFilter = dynamic_cast<MRFilterType *>(caller);

    if (mrFilter)
    {
      // Set the mode for the next level according to MR policy
      this->SetModeForNextLevel(mrFilter->GetElapsedLevels(), mrFilter->GetNumberOfLevels());

      // Reset data before new measurement
      this->ResetFittingData();
    }
  }
}

/**
 *
 */
template <typename TRegistrationFilter, typename TMRFilter>
void
VariationalRegistrationStopCriterion<TRegistrationFilter, TMRFilter>::SetModeForNextLevel(
  const unsigned int nextLevel,
  const unsigned int numberOfLevels)
{
  switch (m_MultiResolutionPolicy)
  {
    case MULTI_RESOLUTION_POLICY_DEFAULT:
      // Use the default stop criterion behavior in each level
      break;

    case MULTI_RESOLUTION_POLICY_SIMPLE_GRADUATED:
      // On the coarser levels, perform increase count check.
      if (static_cast<int>(nextLevel) < static_cast<int>(numberOfLevels) - 1)
      {
        this->SetPerformIncreaseCountCheck(true);
        this->SetPerformLineFittingCheck(false);
      }
      // On finest level, also apply line fitting.
      else
      {
        this->SetPerformIncreaseCountCheck(true);
        this->SetPerformLineFittingCheck(true);
        this->SetPerformLineFittingMaxDistanceCheck(true);
        this->SetLineFittingModeToNormalizedValues();
      }
      break;

    case MULTI_RESOLUTION_POLICY_GRADUATED:
      // On coarser levels perform no check, always i.e. execute the max number
      // of iterations.
      if (static_cast<int>(nextLevel) < static_cast<int>(numberOfLevels) - 2)
      {
        this->SetPerformIncreaseCountCheck(false);
        this->SetPerformLineFittingCheck(false);
      }
      // On the second finest level, perform increase count check.
      else if (static_cast<int>(nextLevel) < static_cast<int>(numberOfLevels) - 1)
      {
        this->SetPerformIncreaseCountCheck(true);
        this->SetPerformLineFittingCheck(false);
      }
      // On finest level, also apply line fitting.
      else
      {
        this->SetPerformIncreaseCountCheck(true);
        this->SetPerformLineFittingCheck(true);
        this->SetPerformLineFittingMaxDistanceCheck(true);
        this->SetLineFittingModeToNormalizedValues();
      }
      break;

    default:
      itkExceptionMacro(<< "Stop criterion policy unknown");
      break;
  }
}

/**
 * Reset the fitting data
 */
template <typename TRegistrationFilter, typename TMRFilter>
void
VariationalRegistrationStopCriterion<TRegistrationFilter, TMRFilter>::ResetFittingData()
{
  m_ElapsedIterations = 0;
  m_CurrentIncreaseCount = 0;

  m_MaxMetricValue = NumericTraits<double>::max();
  m_MinMetricValue = NumericTraits<double>::max();

  if (m_IterationArray != nullptr)
    delete[] m_IterationArray;
  if (m_DistanceArray != nullptr)
    delete[] m_DistanceArray;
  if (m_DistanceArrayForFitting != nullptr)
    delete[] m_DistanceArrayForFitting;

  m_IterationArray = new double[m_NumberOfFittingIterations];
  m_DistanceArray = new double[m_NumberOfFittingIterations];
  m_DistanceArrayForFitting = new double[m_NumberOfFittingIterations];

  for (int i = 0; i < m_NumberOfFittingIterations; i++)
  {
    m_IterationArray[i] = NumericTraits<double>::ZeroValue();
    m_DistanceArray[i] = NumericTraits<double>::NonpositiveMin();
    m_DistanceArrayForFitting[i] = NumericTraits<double>::NonpositiveMin();
  }
}

/**
 * Set the number of iterations used for line fitting
 */
template <typename TRegistrationFilter, typename TMRFilter>
void
VariationalRegistrationStopCriterion<TRegistrationFilter, TMRFilter>::SetNumberOfFittingIterations(int it)
{
  m_NumberOfFittingIterations = it;

  ResetFittingData();
}

/**
 * Set the metric value for the current iteration
 */
template <typename TRegistrationFilter, typename TMRFilter>
void
VariationalRegistrationStopCriterion<TRegistrationFilter, TMRFilter>::SetNextMetricValue(const double value)
{
  double absValue = value;

  if (m_LineFittingUseAbsoluteValues)
  {
    absValue = std::fabs(value);
  }
  else if (value < 0)
  {
    itkWarningMacro(<< "Metric value is < 0");
    absValue = std::fabs(value);
  }

  if (m_ElapsedIterations == 0 || m_MaxMetricValue < 0)
  {
    m_MaxMetricValue = absValue;
    m_MinMetricValue = absValue;
  }
  else
  {
    if (absValue > m_MaxMetricValue)
    {
      m_MaxMetricValue = absValue;
    }

    if (absValue < m_MinMetricValue)
    {
      m_MinMetricValue = absValue;
      m_CurrentIncreaseCount = 0; // Reset increase counter.
    }
    else
    {
      m_CurrentIncreaseCount++; // If no improvement, increase counter.
    }
  }

  unsigned int index = m_ElapsedIterations % m_NumberOfFittingIterations;
  m_DistanceArray[index] = absValue;
  m_IterationArray[index] = static_cast<double>(m_ElapsedIterations);

  m_ElapsedIterations++;
}

/**
 * Perform stop criterion check. This function returns false, if error occurred.
 */
template <typename TRegistrationFilter, typename TMRFilter>
bool
VariationalRegistrationStopCriterion<TRegistrationFilter, TMRFilter>::CheckStopRegistration()
{
  // Check modulus if e.g. only every fifth iteration should be checked.
  if ((m_ElapsedIterations + 1) % m_IterationModulus != 0)
  {
    return false;
  }

  //--------------------------------------
  //
  // perform checking for INCREASE_COUNT
  //
  //--------------------------------------
  // Check the Increase count if the flag is set.
  if (m_PerformIncreaseCountCheck)
  {
    // If max number of increases is reached, return true.
    if (m_CurrentIncreaseCount > m_MaximumIncreaseCount)
    {
      return true;
    }
  }

  //--------------------------------------
  //
  // perform checking for LINE_FIT
  //
  //--------------------------------------
  if (m_PerformLineFittingCheck)
  {
    // Check if minimal number of iterations for performing line fitting is reached.
    if (m_ElapsedIterations < m_NumberOfFittingIterations)
    {
      return false;
    }

    // Check if valid values are set
    for (int k = 0; k < m_NumberOfFittingIterations; k++)
    {
      if (m_DistanceArray[k] == NumericTraits<double>::NonpositiveMin())
      {
        return false;
      }
    }

    // Write distance array for fitting according to current fitting mode
    // (original, scaled or normalized).
    double scaleFactor = 1.0;
    switch (m_LineFittingMode)
    {
      case LINE_FITTING_MODE_ORIGINAL:
        // No scaling or normalization of the values.
        for (int k = 0; k < m_NumberOfFittingIterations; k++)
        {
          m_DistanceArrayForFitting[k] = m_DistanceArray[k];
        }
        break;

      case LINE_FITTING_MODE_NORMALIZED:
        scaleFactor = 1.0 / (m_MaxMetricValue - m_MinMetricValue);

        // Normalization to [0,1].
        for (int k = 0; k < m_NumberOfFittingIterations; k++)
        {
          m_DistanceArrayForFitting[k] = scaleFactor * (m_DistanceArray[k] - m_MinMetricValue);
        }
        break;

      case LINE_FITTING_MODE_SCALED:
        scaleFactor = 1.0 / m_MaxMetricValue;

        // Scaling to a maximum value of 1.
        for (int k = 0; k < m_NumberOfFittingIterations; k++)
        {
          m_DistanceArrayForFitting[k] = scaleFactor * m_DistanceArray[k];
        }
        break;

      default:
        itkExceptionMacro(<< "Unknown line fitting mode!");
        break;
    }

    // Perform line fitting.
    double   m = 0.0;
    double   b = 0.0;
    double * x_values = m_IterationArray;
    double * y_values = m_DistanceArrayForFitting;

    this->FitLine(x_values, y_values, m_NumberOfFittingIterations, &m, &b);
    // std::cout << "Fitted line (m * x + b): m=" << m << " b=" << b <<std::cout;
    itkDebugMacro(<< "Fitted line (m * x + b): m=" << m << " b=" << b);

    // If gradient is positive when absolute values are used.
    // force to stop without any other checking.
    if (!m_LineFittingUseAbsoluteValues && m > 0)
    {
      return true;
    }

    // Check if regression line slope is above threshold.
    if (std::fabs(m) < m_RegressionLineSlopeThreshold)
    {
      // If max distance check should be performed, check if the maximal
      // distance of a value to the regression line is above a threshold.
      // In this case, do not stop registration.
      if (m_PerformLineFittingMaxDistanceCheck)
      {
        // Calculate max distance from fitted line.
        double dist = 0.0;
        for (int k = 0; k < m_NumberOfFittingIterations; k++)
        {
          dist = std::fabs(m_DistanceArrayForFitting[k] - (m * m_IterationArray[k] + b));

          // If distance check is above threshold, return false
          if (dist > m_MaxDistanceToRegressionLine)
          {
            return false;
          }
        }
      }

      // If max distance check false or not performed, return true
      return true;
    }
  }

  // If no check was positive, return false
  return false;
}

/**
 * Perform line fitting using linear regression
 */
template <typename TRegistrationFilter, typename TMRFilter>
void
VariationalRegistrationStopCriterion<TRegistrationFilter, TMRFilter>::FitLine(const double * const x,
                                                                              const double * const y,
                                                                              const int            n,
                                                                              double *             m,
                                                                              double *             b)
{
  double sumx = 0.0;
  double sumy = 0.0;
  double sumx2 = 0.0;
  double sumxy = 0.0;
  auto   dn = (double)n;

  if (n <= 1)
  {
    *m = 0;
    *b = 0;
  }
  else
  {
    double divisor;
    for (int i = 0; i < n; i++)
    {
      sumx += x[i];
      sumy += y[i];
      sumx2 += itk::Math::sqr(x[i]);
      sumxy += x[i] * y[i];
    }
    divisor = (sumx2 - (itk::Math::sqr(sumx) / dn));
    if (divisor != 0)
    {
      *m = (sumxy - ((sumx * sumy) / dn)) / divisor;
      *b = (sumy - ((*m) * sumx)) / dn;
    }
    else
    {
      *m = 0;
      *b = 0;
    }
  }
}

/**
 * Standard "PrintSelf" method.
 */
template <typename TRegistrationFilter, typename TMRFilter>
void
VariationalRegistrationStopCriterion<TRegistrationFilter, TMRFilter>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  // TODO print stop criterion members
}

} // end namespace itk

#endif
