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
#ifndef itkVariationalRegistrationStopCriterion_h
#define itkVariationalRegistrationStopCriterion_h

#include "itkCommand.h"
#include "itkEventObject.h"

#include "itkVariationalRegistrationFilter.h"
#include "itkVariationalRegistrationMultiResolutionFilter.h"

namespace itk
{

/** \class itk::VariationalRegistrationStopCriterion
 *
 *  \brief A flexible stop criterion for the variational registration framework.
 *
 *  This stop criterion is realised as an observer for the
 *  VariationalRegistrationFilter and
 *  VariationalRegistrationMultiResolutionFilter.
 *  It allows testing for
 *    - Number of increase counts for the associated metric
 *    - Slope of a line fitted to the last metric values
 *
 *  For the line fitting, three different modes can be chosen:
 *    - Original: Metric values are used as they are provided by the registration
 *      filter
 *    - Normalized: [min,max] are scaled to [0,1]
 *    - Scaled: [0,max] are scaled to [0,1]
 *
 *  In multi resolution settings, it can be useful to use different stopping
 *  criteria for each MR level. Therefore, different multi resolution policies
 *  can be chosen:
 *    - Default: Don't change criterion depending on the multi resolution level
 *    - Simple graduated: Line fitting and increase count check on finest level,
 *      only increase count check on other levels
 *    - Graduated: Line fitting and increase count check on finest level,
 *      only increase count check on second finest level, no check (maximum number
 *      of iterations) on other levels
 *
 *  \sa VariationalRegistrationFilter
 *  \sa VariationalRegistrationMultiResolutionFilter
 *
 *  \ingroup VariationalRegistration
 *
 *  \note This class was developed with funding from the German Research
 *  Foundation (DFG: EH 224/3-1 and HA 235/9-1).
 *  \author Alexander Schmidt-Richberg
 *  \author Rene Werner
 *  \author Jan Ehrhardt
 */
template <typename TRegistrationFilter, typename TMRFilter>
class VariationalRegistrationStopCriterion : public Command
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(VariationalRegistrationStopCriterion);

  /** Standard class type alias. */
  using Self = VariationalRegistrationStopCriterion;
  using Superclass = Command;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Registration and MR filter types */
  using RegistrationFilterType = TRegistrationFilter;
  using MRFilterType = TMRFilter;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Set iteration modulus.
   *
   * The iteration modulus determines, if check is only performed every
   * modulus iterations. */
  itkSetMacro(IterationModulus, int);

  /**  Get iteration modulus. */
  itkGetMacro(IterationModulus, int);

  /** Perform increase count check. */
  itkSetMacro(PerformIncreaseCountCheck, bool);
  itkGetMacro(PerformIncreaseCountCheck, bool);
  itkBooleanMacro(PerformIncreaseCountCheck);

  /** Set maximum increase count. */
  itkSetMacro(MaximumIncreaseCount, int);

  /** Get maximum increase count. */
  itkGetMacro(MaximumIncreaseCount, int);

  /** Perform line fitting check. */
  itkSetMacro(PerformLineFittingCheck, bool);
  itkGetMacro(PerformLineFittingCheck, bool);
  itkBooleanMacro(PerformLineFittingCheck);

  /** Perform line fitting max distance check.
   *
   *  m_PerformLineFittingCheck must be true for this taking effect. */
  itkSetMacro(PerformLineFittingMaxDistanceCheck, bool);
  itkGetMacro(PerformLineFittingMaxDistanceCheck, bool);
  itkBooleanMacro(PerformLineFittingMaxDistanceCheck);

  /** Use the absolute metric value for the line fitting.
   *
   *  m_LineFittingUseAbsoluteValues must be true for this taking effect. */
  itkSetMacro(LineFittingUseAbsoluteValues, bool);
  itkGetMacro(LineFittingUseAbsoluteValues, bool);
  itkBooleanMacro(LineFittingUseAbsoluteValues);

  /** Set number of fitting iterations. This will call Reset().
   *
   * For the previous m_NumberOfFittingIterations iterations a linear
   * regression to the (normalized) values is calculated. If the absolute
   * value the gradient is smaller then DistanceGradientThresh, the stop
   * criterion is satisfied.
   * \param it Number of fitting iterations. */
  virtual void
  SetNumberOfFittingIterations(const int it);

  /** Get number of fitting iterations. */
  itkGetMacro(NumberOfFittingIterations, int);

  /** Set gradient threshold. */
  itkSetMacro(RegressionLineSlopeThreshold, double);

  /** Get gradient threshold. */
  itkGetMacro(RegressionLineSlopeThreshold, double);

  /** Set maximum distance to regression line. */
  itkSetMacro(MaxDistanceToRegressionLine, double);

  /** Get maximum distance to regression line. */
  itkGetMacro(MaxDistanceToRegressionLine, double);

  /** Enumerate for the different line fitting modes. */
  enum LineFittingMode
  {
    LINE_FITTING_MODE_ORIGINAL = 0,
    LINE_FITTING_MODE_NORMALIZED = 1,
    LINE_FITTING_MODE_SCALED = 2
  };

  /** Set the line fitting mode. Three modes are provided:
   * - Original: Metric values are used as they are provided by the registration
   *   filter
   * - Normalized: [min,max] are scaled to [0,1]
   * - Scaled: [0,max] are scaled to [0,1] */
  itkSetEnumMacro(LineFittingMode, LineFittingMode);

  /** Get the line fitting mode. */
  itkGetEnumMacro(LineFittingMode, LineFittingMode);

  /** Set line fitting mode to original. */
  virtual void
  SetLineFittingModeToOriginalValues()
  {
    this->SetLineFittingMode(LINE_FITTING_MODE_ORIGINAL);
  }

  /** Set line fitting mode to normalized values. */
  virtual void
  SetLineFittingModeToNormalizedValues()
  {
    this->SetLineFittingMode(LINE_FITTING_MODE_NORMALIZED);
  }

  /** Set line fitting mode to scaled values. */
  virtual void
  SetLineFittingModeToScaledValues()
  {
    this->SetLineFittingMode(LINE_FITTING_MODE_SCALED);
  }

  /** Enumerate for the different multi resolution policies. */
  enum MultiResolutionPolicy
  {
    MULTI_RESOLUTION_POLICY_DEFAULT = 0,
    MULTI_RESOLUTION_POLICY_SIMPLE_GRADUATED = 1,
    MULTI_RESOLUTION_POLICY_GRADUATED = 2
  };

  /** Set the multi resolution policy. Three different multi resolution policies
   * are provided:
   * - Default: Don't change criterion depending on the multi resolution level
   * - Simple graduated: Line fitting and increase count check on finest level,
   *   only increase count check on other levels
   * - Graduated: Line fitting and increase count check on finest level,
   *   only increase count check on second finest level, no check (maximum number
   *   of iterations) on other levels */
  itkSetEnumMacro(MultiResolutionPolicy, MultiResolutionPolicy);

  /** Get the multi resolution policy. */
  itkGetEnumMacro(MultiResolutionPolicy, MultiResolutionPolicy);

  /** In each level, use the default stop criterion of the registration filter. */
  virtual void
  SetMultiResolutionPolicyToDefault()
  {
    this->SetMultiResolutionPolicy(MULTI_RESOLUTION_POLICY_DEFAULT);
  }

  /** Apply a simple graduated policy:
   *  - On coarser levels, perform increase count check.
   *  - On finest level, also apply line fitting. */
  virtual void
  SetMultiResolutionPolicyToSimpleGraduated()
  {
    this->SetMultiResolutionPolicy(MULTI_RESOLUTION_POLICY_SIMPLE_GRADUATED);
  }

  /** Apply a graduated policy:
   *  - On coarser levels, perform no check at all, i.e. always execute the
   *    max number of iterations.
   *  - On the second finest level, perform increase count check.
   *  - On finest level, also apply line fitting. */
  virtual void
  SetMultiResolutionPolicyToGraduated()
  {
    this->SetMultiResolutionPolicy(MULTI_RESOLUTION_POLICY_GRADUATED);
  }

  void
  Execute(itk::Object * caller, const itk::EventObject & event) override;

  void
  Execute(const itk::Object * caller, const itk::EventObject & event) override;

protected:
  VariationalRegistrationStopCriterion();
  ~VariationalRegistrationStopCriterion() override;

  /** Print information about the filter. */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Set the flags according to the multi-resolution policy for
   * the next level. */
  virtual void
  SetModeForNextLevel(const unsigned int nextLevel, const unsigned int numberOfLevels);

  /** Set the metric value for the current iteration.
   * \param value The metric value of the current iteration. */
  virtual void
  SetNextMetricValue(const double value);

  /** Reset the fitting data. */
  virtual void
  ResetFittingData();

  /** Perform the checking of the stop criterion.
   * \return Result of the stopping check. */
  virtual bool
  CheckStopRegistration();

  /** Calculate linear regression line. */
  virtual void
  FitLine(const double * const x, const double * const y, const int n, double * m, double * b);

private:
  // Stop criterion multi-resolution policy.
  MultiResolutionPolicy m_MultiResolutionPolicy;

  // General parameter.
  int m_IterationModulus;  // Only check every mod iterations.
  int m_ElapsedIterations; // The number of elapsed iterations.

  // Member for increase count calculation.
  bool m_PerformIncreaseCountCheck; // Perform increase count check?

  int m_CurrentIncreaseCount;
  int m_MaximumIncreaseCount;

  double m_MaxMetricValue; // Stores max metric value.
  double m_MinMetricValue; // Stores min metric value.

  // Member for line fitting.
  bool m_PerformLineFittingCheck;
  bool m_PerformLineFittingMaxDistanceCheck;
  bool m_LineFittingUseAbsoluteValues; // Use absolute values for line fitting.

  LineFittingMode m_LineFittingMode; // Mode for the line fitting.

  int    m_NumberOfFittingIterations;
  double m_RegressionLineSlopeThreshold;
  double m_MaxDistanceToRegressionLine;

  // Distance and iteration array.
  double * m_DistanceArray;
  double * m_IterationArray;
  double * m_DistanceArrayForFitting;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVariationalRegistrationStopCriterion.hxx"
#endif

#endif
