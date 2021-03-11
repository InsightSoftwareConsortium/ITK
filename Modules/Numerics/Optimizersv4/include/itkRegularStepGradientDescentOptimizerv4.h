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
#ifndef itkRegularStepGradientDescentOptimizerv4_h
#define itkRegularStepGradientDescentOptimizerv4_h

#include "itkGradientDescentOptimizerv4.h"
#include <itkCompensatedSummation.h>

namespace itk
{
/**
 *\class RegularStepGradientDescentOptimizerv4
 *  \brief Regular Step Gradient descent optimizer.
 *
 *   This optimizer is a variant of gradient descent that attempts to prevent it
 *   from taking steps that are too large. At each iteration, this optimizer
 *   will take a step along the direction of the metric derivative. Each time the
 *   direction of the derivative abruptly changes, the optimizer assumes that a
 *   local extrema has been passed and reacts by reducing the step length by a
 *   relaxation factor that is set to 0.5 by default.
 *   The default value for the initial step length is 1, and this value can only
 *   be changed manually via SetLearningRate() since this optimizer does not use
 *   the ScaleEstimator to automatically estimate the learning rate.
 *   Also note that unlike the previous version of ReuglarStepGradientDescentOptimizer,
 *   ITKv4 does not have a "maximize/minimize" option to modify the effect of
 *   the metric derivative. The assigned metric is assumed to return a parameter
 *   derivative result that "improves" the optimization.
 *
 * \ingroup ITKOptimizersv4
 */
template <typename TInternalComputationValueType = double>
class ITK_TEMPLATE_EXPORT RegularStepGradientDescentOptimizerv4
  : public GradientDescentOptimizerv4Template<TInternalComputationValueType>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(RegularStepGradientDescentOptimizerv4);

  /** Standard class type aliases. */
  using Self = RegularStepGradientDescentOptimizerv4;
  using Superclass = GradientDescentOptimizerv4Template<TInternalComputationValueType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(RegularStepGradientDescentOptimizerv4, GradientDescentOptimizerv4Template);

  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro(Self);


  /** It should be possible to derive the internal computation type from the class object. */
  using InternalComputationValueType = TInternalComputationValueType;

  /** Derivative type. */
  using DerivativeType = typename Superclass::DerivativeType;

  /** Metric type over which this class is templated. */
  using MeasureType = typename Superclass::MeasureType;
  using IndexRangeType = typename Superclass::IndexRangeType;
  using ScalesType = typename Superclass::ScalesType;
  using ParametersType = typename Superclass::ParametersType;

  /** Compensated summation type. */
  using CompensatedSummationType = CompensatedSummation<InternalComputationValueType>;

  /** Minimum step length (learning rate) value for convergence checking.
   *  When the local minima is passed by taking a large step, the step
   *  length is adjusted (decreased) by the relaxation factor, so that smaller
   *  steps are taken towards the minimum point (convergence).
   *  When the step length value reaches a small value, it would be treated
   *  as converged.
   *
   *  The default value is set to 1e-4 to pass all tests.
   */
  itkSetMacro(MinimumStepLength, TInternalComputationValueType);
  itkGetConstReferenceMacro(MinimumStepLength, TInternalComputationValueType);

  /** Set/Get relaxation factor value. */
  itkSetMacro(RelaxationFactor, TInternalComputationValueType);
  itkGetConstReferenceMacro(RelaxationFactor, TInternalComputationValueType);

  /** Set/Get gradient magnitude tolerance value for convergence checking. */
  itkSetMacro(GradientMagnitudeTolerance, TInternalComputationValueType);
  itkGetConstReferenceMacro(GradientMagnitudeTolerance, TInternalComputationValueType);

  /** Set/Get current scale for learning rate. */
  itkSetMacro(CurrentLearningRateRelaxation, MeasureType);
  itkGetConstReferenceMacro(CurrentLearningRateRelaxation, MeasureType);

  /** Start and run the optimization. */
  void
  StartOptimization(bool doOnlyInitialization = false) override;

  /** Estimate the learning rate based on the current gradient. */
  void
  EstimateLearningRate() override;

  /** Get current gradient step value. */
  double
  GetCurrentStepLength() const;

protected:
  /** Advance one Step following the gradient direction.
   * Includes transform update. */
  void
  AdvanceOneStep() override;

  /** Modify the input gradient over a given index range. */
  void
  ModifyGradientByScalesOverSubRange(const IndexRangeType & subrange) override;
  void
  ModifyGradientByLearningRateOverSubRange(const IndexRangeType & subrange) override;


  /** Default constructor. */
  RegularStepGradientDescentOptimizerv4();

  /** Destructor. */
  ~RegularStepGradientDescentOptimizerv4() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;


private:
  TInternalComputationValueType m_RelaxationFactor;

  TInternalComputationValueType m_MinimumStepLength;

  TInternalComputationValueType m_GradientMagnitudeTolerance;

  MeasureType m_CurrentLearningRateRelaxation;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkRegularStepGradientDescentOptimizerv4.hxx"
#endif

#endif
