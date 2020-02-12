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
#ifndef itkSingleValuedNonLinearVnlOptimizerv4_h
#define itkSingleValuedNonLinearVnlOptimizerv4_h

#include "ITKOptimizersv4Export.h"

#include "itkObjectToObjectOptimizerBase.h"
#include "itkSingleValuedVnlCostFunctionAdaptorv4.h"
#include "itkCommand.h"

namespace itk
{
/**
 *\class SingleValuedNonLinearVnlOptimizerv4
 * \brief This is a base for the ITKv4 Optimization methods using
 * the vnl library.
 *
 * It is an Adaptor class for optimizers provided by the vnl library.
 *
 * \ingroup ITKOptimizersv4
 */
class ITKOptimizersv4_EXPORT SingleValuedNonLinearVnlOptimizerv4 : public ObjectToObjectOptimizerBaseTemplate<double>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(SingleValuedNonLinearVnlOptimizerv4);

  /** Standard class type aliases. */
  using Self = SingleValuedNonLinearVnlOptimizerv4;
  using Superclass = ObjectToObjectOptimizerBase;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(SingleValuedNonLinearVnlOptimizerv4, ObjectToObjectOptimizerBase)

    /** Command observer that will interact with the ITKVNL cost-function
     * adaptor in order to generate iteration events. This will allow to overcome
     * the limitation of VNL optimizers not offering callbacks for every
     * iteration */
    using CommandType = ReceptorMemberCommand<Self>;

  using MetricType = Superclass::MetricType;
  using DerivativeType = Superclass::DerivativeType;
  using ParametersType = Superclass::ParametersType;
  using ScalesType = Superclass::ScalesType;

  /** Stop condition return string type */
  using StopConditionReturnStringType = Superclass::StopConditionReturnStringType;

  /** Stop condition internal string type */
  using StopConditionDescriptionType = Superclass::StopConditionDescriptionType;

  void
  StartOptimization(bool doOnlyInitialization = false) override;

  /** Set the metric (cost function). This method has to be overloaded
   *  by derived classes because the CostFunctionAdaptor requires
   *  to know the number of parameters at construction time. This
   *  number of parameters is obtained at run-time from the itkObjectToObjectMetric.
   *  As a consequence each derived optimizer should construct its own
   *  CostFunctionAdaptor when overloading this method  */
  void
  SetMetric(MetricType * metric) override = 0;

  /** Return Cached Values. These method have the advantage of not triggering a
   * recomputation of the metric value, but it has the disadvantage of returning
   * a value that may not be the one corresponding to the current parameters. For
   * GUI update purposes, this method is a good option.
   * \note The metric value is cached in the base class, retrieved via GetValue(). */
  itkGetConstReferenceMacro(CachedDerivative, DerivativeType);
  itkGetConstReferenceMacro(CachedCurrentPosition, ParametersType);

  /** Get the reason for termination */
  const StopConditionReturnStringType
  GetStopConditionDescription() const override = 0;

protected:
  SingleValuedNonLinearVnlOptimizerv4();
  ~SingleValuedNonLinearVnlOptimizerv4() override;

  using CostFunctionAdaptorType = SingleValuedVnlCostFunctionAdaptorv4;

  void
  SetCostFunctionAdaptor(CostFunctionAdaptorType * adaptor);

  const CostFunctionAdaptorType *
  GetCostFunctionAdaptor() const;

  CostFunctionAdaptorType *
  GetCostFunctionAdaptor();

  /** The purpose of this method is to get around the lack of
   *  const-correctness in VNL cost-functions and optimizers */
  CostFunctionAdaptorType *
  GetNonConstCostFunctionAdaptor() const;

  /** Print out internal state */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  /** The purpose of this method is to get around the lack of iteration reporting
   * in VNL optimizers. By interfacing directly with the ITK cost function
   * adaptor we are generating here Iteration Events. Note the iteration events
   * here are produce PER EVALUATION of the metric, not per real iteration of the
   * vnl optimizer. Optimizers that evaluate the metric multiple times at each
   * iteration will generate a lot more of Iteration events here. */
  void
  IterationReport(const EventObject & event);

  CostFunctionAdaptorType * m_CostFunctionAdaptor;

  CommandType::Pointer m_Command;

  mutable ParametersType m_CachedCurrentPosition;
  mutable DerivativeType m_CachedDerivative;
};
} // end namespace itk

#endif
