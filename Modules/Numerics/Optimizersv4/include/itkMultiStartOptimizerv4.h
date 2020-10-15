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
#ifndef itkMultiStartOptimizerv4_h
#define itkMultiStartOptimizerv4_h

#include "itkObjectToObjectOptimizerBase.h"
#include "itkGradientDescentOptimizerv4.h"

namespace itk
{

/**
 *\class MultiStartOptimizerv4Template
 *  \brief Multi-start searches over input parameters and returns the best metric value
 *
 *   The multi-start algorithm performs gradient descent from N (large) number of starting points and
 *   returns the best solution. Ideal start points would sample the solution space almost uniformly, thus,
 *   in theory, this is a global optimizer.  In this implementation, the quality of the optimization
 *   depends on the parameter space samples that the user inputs to the optimizer.  Multi-start can be
 *   modified in numerous ways to improve robustness of standard approaches.  These improvements usually
 *   focus modifying the parameter sample space.  This is why we place the burden on the user to provide
 *   the parameter samples over which to optimize.
 *
 * \ingroup ITKOptimizersv4
 */
template <typename TInternalComputationValueType>
class ITK_TEMPLATE_EXPORT MultiStartOptimizerv4Template
  : public ObjectToObjectOptimizerBaseTemplate<TInternalComputationValueType>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MultiStartOptimizerv4Template);

  /** Standard class type aliases. */
  using Self = MultiStartOptimizerv4Template;
  using Superclass = ObjectToObjectOptimizerBaseTemplate<TInternalComputationValueType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(MultiStartOptimizerv4Template, Superclass);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  using ParametersType = typename Superclass::ParametersType;
  using ParametersListType = std::vector<ParametersType>;
  using ParameterListSizeType = typename ParametersListType::size_type;

  using OptimizerType = ObjectToObjectOptimizerBaseTemplate<TInternalComputationValueType>;
  using OptimizerPointer = typename OptimizerType::Pointer;
  using LocalOptimizerType = typename itk::GradientDescentOptimizerv4Template<TInternalComputationValueType>;
  using LocalOptimizerPointer = typename LocalOptimizerType::Pointer;

  /** Enables backwards compatibility for enum values */
#if !defined(ITK_LEGACY_REMOVE)
  // We need to expose the enum values at the class level
  // for backwards compatibility
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
  static constexpr itk::StopConditionObjectToObjectOptimizerEnum OTHER_ERROR =
    itk::StopConditionObjectToObjectOptimizerEnum::OTHER_ERROR;
#endif

  /** Stop condition return string type */
  using StopConditionReturnStringType = typename Superclass::StopConditionReturnStringType;

  /** Stop condition internal string type */
  using StopConditionDescriptionType = typename Superclass::StopConditionDescriptionType;
  /** Stop condition return string type */

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
  itkGetConstReferenceMacro(StopCondition, StopConditionObjectToObjectOptimizerEnum);

  /** Create an instance of the local optimizer */
  void
  InstantiateLocalOptimizer();

  /** Begin the optimization */
  void
  StartOptimization(bool doOnlyInitialization = false) override;

  /** Stop optimization. The object is left in a state so the
   * optimization can be resumed by calling ResumeOptimization. */
  virtual void
  StopOptimization();

  /** Resume the optimization. Can be called after StopOptimization to
   * resume. The bulk of the optimization work loop is here. */
  virtual void
  ResumeOptimization();

  /** Get the reason for termination */
  const StopConditionReturnStringType
  GetStopConditionDescription() const override;

  /** Get the list of parameters over which to search.  */
  ParametersListType &
  GetParametersList();

  /** Set the list of parameters over which to search */
  void
  SetParametersList(ParametersListType & p);

  /** Get the list of metric values that we produced after the multi-start search.  */
  const MetricValuesListType &
  GetMetricValuesList() const;

  /** Return the parameters from the best visited position */
  ParametersType
  GetBestParameters();

  /** Set/Get the optimizer. */
  itkSetObjectMacro(LocalOptimizer, OptimizerType);
  itkGetModifiableObjectMacro(LocalOptimizer, OptimizerType);

  inline ParameterListSizeType
  GetBestParametersIndex()
  {
    return this->m_BestParametersIndex;
  }

protected:
  /** Default constructor */
  MultiStartOptimizerv4Template();
  ~MultiStartOptimizerv4Template() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /* Common variables for optimization control and reporting */
  bool                                     m_Stop{ false };
  StopConditionObjectToObjectOptimizerEnum m_StopCondition;
  StopConditionDescriptionType             m_StopConditionDescription;
  ParametersListType                       m_ParametersList;
  MetricValuesListType                     m_MetricValuesList;
  MeasureType                              m_MinimumMetricValue;
  MeasureType                              m_MaximumMetricValue;
  ParameterListSizeType                    m_BestParametersIndex;
  OptimizerPointer                         m_LocalOptimizer;
};

/** This helps to meet backward compatibility */
using MultiStartOptimizerv4 = MultiStartOptimizerv4Template<double>;

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMultiStartOptimizerv4.hxx"
#endif

#endif
