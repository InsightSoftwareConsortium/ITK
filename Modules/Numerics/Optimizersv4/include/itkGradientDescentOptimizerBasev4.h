/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkGradientDescentOptimizerBasev4_h
#define itkGradientDescentOptimizerBasev4_h

#include "itkObjectToObjectOptimizerBase.h"
#include "itkWindowConvergenceMonitoringFunction.h"
#include "itkThreadedIndexedContainerPartitioner.h"
#include "itkDomainThreader.h"

namespace itk
{
/**
 *\class GradientDescentOptimizerBasev4
 *  \brief Abstract base class for gradient descent-style optimizers.
 *
 * Gradient modification is threaded in \c ModifyGradient.
 *
 * Derived classes must override \c ModifyGradientByScalesOverSubRange,
 * \c ModifyGradientByLearningRateOverSubRange and \c ResumeOptimization.
 *
 * \ingroup ITKOptimizersv4
 */
template <typename TInternalComputationValueType>
class ITK_TEMPLATE_EXPORT GradientDescentOptimizerBasev4Template
  : public ObjectToObjectOptimizerBaseTemplate<TInternalComputationValueType>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(GradientDescentOptimizerBasev4Template);

  /** Standard class type aliases. */
  using Self = GradientDescentOptimizerBasev4Template;
  using Superclass = ObjectToObjectOptimizerBaseTemplate<TInternalComputationValueType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(GradientDescentOptimizerBasev4Template, Superclass);

#if !defined(ITK_LEGACY_REMOVE)
  /**Exposes enums values for backwards compatibility*/
  static constexpr itk::StopConditionObjectToObjectOptimizerEnum MAXIMUM_NUMBER_OF_ITERATIONS =
    itk::StopConditionObjectToObjectOptimizerEnum::MAXIMUM_NUMBER_OF_ITERATIONS;
  static constexpr itk::StopConditionObjectToObjectOptimizerEnum COSTFUNCTION_ERROR =
    itk::StopConditionObjectToObjectOptimizerEnum::COSTFUNCTION_ERROR;
  static constexpr itk::StopConditionObjectToObjectOptimizerEnum UPDATE_PARAMETERS_ERROR =
    itk::StopConditionObjectToObjectOptimizerEnum::UPDATE_PARAMETERS_ERROR;
  static constexpr itk::StopConditionObjectToObjectOptimizerEnum STEP_TOO_SMALL =
    itk::StopConditionObjectToObjectOptimizerEnum::STEP_TOO_SMALL;
  static constexpr itk::StopConditionObjectToObjectOptimizerEnum CONVERGENCE_CHECKER_PASSED =
    itk::StopConditionObjectToObjectOptimizerEnum::CONVERGENCE_CHECKER_PASSED;
  static constexpr itk::StopConditionObjectToObjectOptimizerEnum GRADIENT_MAGNITUDE_TOLEARANCE =
    itk::StopConditionObjectToObjectOptimizerEnum::GRADIENT_MAGNITUDE_TOLEARANCE;
  static constexpr itk::StopConditionObjectToObjectOptimizerEnum OTHER_ERROR =
    itk::StopConditionObjectToObjectOptimizerEnum::OTHER_ERROR;
#endif

  /** Stop condition return string type */
  using StopConditionReturnStringType = typename Superclass::StopConditionReturnStringType;

  /** Stop condition internal string type */
  using StopConditionDescriptionType = typename Superclass::StopConditionDescriptionType;

  /** It should be possible to derive the internal computation type from the class object. */
  using InternalComputationValueType = TInternalComputationValueType;

  /** Metric type over which this class is templated */
  using MetricType = typename Superclass::MetricType;
  using MetricTypePointer = typename MetricType::Pointer;

  /** Derivative type */
  using DerivativeType = typename Superclass::DerivativeType;

  /** Measure type */
  using MeasureType = typename Superclass::MeasureType;

  using ScalesType = typename Superclass::ScalesType;

  using ParametersType = typename Superclass::ParametersType;

  /** Type for the convergence checker */
  using ConvergenceMonitoringType = itk::Function::WindowConvergenceMonitoringFunction<TInternalComputationValueType>;

  /** Get the most recent gradient values. */
  itkGetConstReferenceMacro(Gradient, DerivativeType);

  /** Get stop condition enum */
  itkGetConstReferenceMacro(StopCondition, StopConditionObjectToObjectOptimizerEnum);

  /** Set the number of iterations. */
  void
  SetNumberOfIterations(const SizeValueType numberOfIterations) override
  {
    itkDebugMacro("setting NumberOfIterations to " << numberOfIterations);
    if (this->m_NumberOfIterations != numberOfIterations)
    {
      this->m_NumberOfIterations = numberOfIterations;
      this->Modified();
    }
  }

  /** Get the number of iterations. */
  SizeValueType
  GetNumberOfIterations() const override
  {
    return this->m_NumberOfIterations;
  }

  /** Get the current iteration number. */
  SizeValueType
  GetCurrentIteration() const override
  {
    return this->m_CurrentIteration;
  }

  /** Start and run the optimization */
  void
  StartOptimization(bool doOnlyInitialization = false) override;

  /** Resume optimization.
   * This runs the optimization loop, and allows continuation
   * of stopped optimization */
  virtual void
  ResumeOptimization() = 0;

  /** Stop optimization. The object is left in a state so the
   * optimization can be resumed by calling ResumeOptimization. */
  virtual void
  StopOptimization();

  /** Get the reason for termination */
  const StopConditionReturnStringType
  GetStopConditionDescription() const override;

  /** Modify the gradient in place, to advance the optimization.
   * This call performs a threaded modification for transforms with
   * local support (assumed to be dense). Otherwise the modification
   * is performed w/out threading.
   * See EstimateLearningRate() to perform optionally learning rate
   * estimation.
   * At completion, m_Gradient can be used to update the transform
   * parameters. Derived classes may hold additional results in
   * other member variables.
   *
   * \sa EstimateLearningRate()
   */
  virtual void
  ModifyGradientByScales();
  virtual void
  ModifyGradientByLearningRate();

  using IndexRangeType = ThreadedIndexedContainerPartitioner::IndexRangeType;

  /** Derived classes define this worker method to modify the gradient by scales.
   * Modifications must be performed over the index range defined in
   * \c subrange.
   * Called from ModifyGradientByScales(), either directly or via threaded
   * operation. */
  virtual void
  ModifyGradientByScalesOverSubRange(const IndexRangeType & subrange) = 0;

  /** Derived classes define this worker method to modify the gradient by learning rates.
   * Modifications must be performed over the index range defined in
   * \c subrange.
   * Called from ModifyGradientByLearningRate(), either directly or via threaded
   * operation.
   * This function is used in GradientDescentOptimizerBasev4ModifyGradientByScalesThreaderTemplate
   * and GradientDescentOptimizerBasev4ModifyGradientByLearningRateThreaderTemplate classes.
   */
  virtual void
  ModifyGradientByLearningRateOverSubRange(const IndexRangeType & subrange) = 0;

protected:
  /** Default constructor */
  GradientDescentOptimizerBasev4Template();
  ~GradientDescentOptimizerBasev4Template() override = default;

  /** Flag to control use of the ScalesEstimator (if set) for
   * automatic learning step estimation at *each* iteration.
   */
  bool m_DoEstimateLearningRateAtEachIteration;

  /** Flag to control use of the ScalesEstimator (if set) for
   * automatic learning step estimation only *once*, during first iteration.
   */
  bool m_DoEstimateLearningRateOnce;

  /** The maximum step size in physical units, to restrict learning rates.
   * Only used with automatic learning rate estimation.
   * It may be initialized either by calling SetMaximumStepSizeInPhysicalUnits
   * manually or by using m_ScalesEstimator automatically, and the former has
   * higher priority than the latter. See main documentation.
   */
  TInternalComputationValueType m_MaximumStepSizeInPhysicalUnits;

  /** Flag to control using the convergence monitoring for stop condition.
   *  This flag should be always set to true except for regular step gradient
   *  descent optimizer that uses minimum step length to check the convergence.
   */
  bool m_UseConvergenceMonitoring;

  /** Window size for the convergence checker.
   *  The convergence checker calculates convergence value by fitting to
   *  a window of the energy (metric value) profile.
   */
  SizeValueType m_ConvergenceWindowSize;

  /** The convergence checker. */
  typename ConvergenceMonitoringType::Pointer m_ConvergenceMonitoring;

  typename DomainThreader<ThreadedIndexedContainerPartitioner, Self>::Pointer m_ModifyGradientByScalesThreader;
  typename DomainThreader<ThreadedIndexedContainerPartitioner, Self>::Pointer m_ModifyGradientByLearningRateThreader;

  /* Common variables for optimization control and reporting */
  bool                                     m_Stop{ false };
  StopConditionObjectToObjectOptimizerEnum m_StopCondition;
  StopConditionDescriptionType             m_StopConditionDescription;

  /** Current gradient */
  DerivativeType m_Gradient;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
};

/** This helps to meet backward compatibility */
using GradientDescentOptimizerBasev4 = GradientDescentOptimizerBasev4Template<double>;

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkGradientDescentOptimizerBasev4.hxx"
#endif

#endif
