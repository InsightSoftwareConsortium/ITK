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
#ifndef itkMultiGradientOptimizerv4_h
#define itkMultiGradientOptimizerv4_h

#include "itkObjectToObjectOptimizerBase.h"
#include "itkGradientDescentOptimizerv4.h"

namespace itk
{
/**
 *\class MultiGradientOptimizerv4Template
 *  \brief Multiple gradient-based optimizers are combined in order to perform a multi-objective optimization.
 *
 *  This optimizer will do a combined gradient descent optimization using whatever metric/optimizer gradient
 *  sub-optimizers are passed to it by the user.  The learning rate or scaleestimator for each sub-optimizer
 *  controls the relative weight of each metric in the optimization.  Denote the weights as \f$ w_1 \f$ and \f$ w_2 \f$
 * then the MultiGradientOptimizer will optimize \f$ \sum_i w_i Metric_i \f$ by using update rule:
 *
 *  \f[
 *    params_{new} = params_{old} + \frac{1}{N_{Metrics}} * ( \sum_i w_i Grad(Metric_i) )
 *  \f]
 *
 *  \note The scales, learning rates and weights options must be set individually for each sub-optimizer,
 *  and have no effect when set on this class.
 *
 *  The test for this class illustrates the expected behavior.
 *
 * \ingroup ITKOptimizersv4
 */
template <typename TInternalComputationValueType>
class ITK_TEMPLATE_EXPORT MultiGradientOptimizerv4Template
  : public GradientDescentOptimizerv4Template<TInternalComputationValueType>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MultiGradientOptimizerv4Template);

  /** Standard class type aliases. */
  using Self = MultiGradientOptimizerv4Template;
  using Superclass = GradientDescentOptimizerv4Template<TInternalComputationValueType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(MultiGradientOptimizerv4Template, Superclass);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  using LocalOptimizerType = itk::GradientDescentOptimizerv4Template<TInternalComputationValueType>;
  using LocalOptimizerPointer =
    typename itk::GradientDescentOptimizerv4Template<TInternalComputationValueType>::Pointer;
  using ParametersType = typename Superclass::ParametersType;
  using OptimizerType = ObjectToObjectOptimizerBaseTemplate<TInternalComputationValueType>;
  using OptimizerPointer = typename OptimizerType::Pointer;
  using OptimizersListType = std::vector<LocalOptimizerPointer>;
  using OptimizersListSizeType = typename OptimizersListType::size_type;

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
  using DerivativeType = typename MetricType::DerivativeType;

  /** Measure type */
  using MeasureType = typename Superclass::MeasureType;
  using MetricValuesListType = std::vector<MeasureType>;

  /** Get stop condition enum */
  const StopConditionObjectToObjectOptimizerEnum &
  GetStopCondition() const override
  {
    return this->m_StopCondition;
  }

  /** Begin the optimization */
  void
  StartOptimization(bool doOnlyInitialization = false) override;

  /** Stop optimization. The object is left in a state so the
   * optimization can be resumed by calling ResumeOptimization. */
  void
  StopOptimization() override;

  /** Resume the optimization. Can be called after StopOptimization to
   * resume. The bulk of the optimization work loop is here. */
  void
  ResumeOptimization() override;

  /** Get the reason for termination */
  const StopConditionReturnStringType
  GetStopConditionDescription() const override;

  /** Get the list of optimizers currently held.  */
  OptimizersListType &
  GetOptimizersList();

  /** Set the list of optimizers to combine */
  void
  SetOptimizersList(OptimizersListType & p);

  /** Get the list of metric values that we produced after the multi-objective search.  */
  const MetricValuesListType &
  GetMetricValuesList() const;

protected:
  /** Default constructor */
  MultiGradientOptimizerv4Template();
  ~MultiGradientOptimizerv4Template() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /* Common variables for optimization control and reporting */
  bool                                     m_Stop{ false };
  StopConditionObjectToObjectOptimizerEnum m_StopCondition;
  StopConditionDescriptionType             m_StopConditionDescription;
  OptimizersListType                       m_OptimizersList;
  MetricValuesListType                     m_MetricValuesList;
  MeasureType                              m_MinimumMetricValue;
  MeasureType                              m_MaximumMetricValue;
};

/** This helps to meet backward compatibility */
using MultiGradientOptimizerv4 = MultiGradientOptimizerv4Template<double>;

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMultiGradientOptimizerv4.hxx"
#endif

#endif
