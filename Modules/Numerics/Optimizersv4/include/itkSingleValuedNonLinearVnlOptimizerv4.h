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
#ifndef itkSingleValuedNonLinearVnlOptimizerv4_h
#define itkSingleValuedNonLinearVnlOptimizerv4_h

#include "ITKOptimizersv4Export.h"

#include "itkObjectToObjectOptimizerBase.h"
#include "itkSingleValuedVnlCostFunctionAdaptorv4.h"
#include "itkCommand.h"

namespace itk
{
/** \class SingleValuedNonLinearVnlOptimizerv4
 * \brief This is a base for the ITKv4 Optimization methods using
 * the vnl library.
 *
 * It is an Adaptor class for optimizers provided by the vnl library.
 *
 * \ingroup ITKOptimizersv4
 */
class ITKOptimizersv4_EXPORT SingleValuedNonLinearVnlOptimizerv4 :
    public ObjectToObjectOptimizerBaseTemplate<double>
{
public:
  /** Standard class typedefs. */
  typedef SingleValuedNonLinearVnlOptimizerv4 Self;
  typedef ObjectToObjectOptimizerBase         Superclass;
  typedef SmartPointer< Self >                Pointer;
  typedef SmartPointer< const Self >          ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(SingleValuedNonLinearVnlOptimizerv4, ObjectToObjectOptimizerBase)

  /** Command observer that will interact with the ITKVNL cost-function
   * adaptor in order to generate iteration events. This will allow to overcome
   * the limitation of VNL optimizers not offering callbacks for every
   * iteration */
  typedef ReceptorMemberCommand< Self >   CommandType;

  typedef Superclass::MetricType     MetricType;
  typedef Superclass::DerivativeType DerivativeType;
  typedef Superclass::ParametersType ParametersType;
  typedef Superclass::ScalesType     ScalesType;

  /** Stop condition return string type */
  typedef Superclass::StopConditionReturnStringType StopConditionReturnStringType;

  /** Stop condition internal string type */
  typedef Superclass::StopConditionDescriptionType  StopConditionDescriptionType;

  virtual void StartOptimization(bool doOnlyInitialization = false) ITK_OVERRIDE;

  /** Set the metric (cost function). This method has to be overloaded
   *  by derived classes because the CostFunctionAdaptor requires
   *  to know the number of parameters at construction time. This
   *  number of parameters is obtained at run-time from the itkObjectToObjectMetric.
   *  As a consequence each derived optimizer should construct its own
   *  CostFunctionAdaptor when overloading this method  */
  virtual void SetMetric(MetricType *metric) ITK_OVERRIDE = 0;

  /** Return Cached Values. These method have the advantage of not triggering a
   * recomputation of the metric value, but it has the disadvantage of returning
   * a value that may not be the one corresponding to the current parameters. For
   * GUI update purposes, this method is a good option.
   * \note The metric value is cached in the base class, retrieved via GetValue(). */
  itkGetConstReferenceMacro(CachedDerivative, DerivativeType);
  itkGetConstReferenceMacro(CachedCurrentPosition, ParametersType);

  /** Get the reason for termination */
  virtual const StopConditionReturnStringType GetStopConditionDescription() const ITK_OVERRIDE = 0;

protected:
  SingleValuedNonLinearVnlOptimizerv4();
  virtual ~SingleValuedNonLinearVnlOptimizerv4() ITK_OVERRIDE;

  typedef SingleValuedVnlCostFunctionAdaptorv4 CostFunctionAdaptorType;

  void SetCostFunctionAdaptor(CostFunctionAdaptorType *adaptor);

  const CostFunctionAdaptorType * GetCostFunctionAdaptor() const;

  CostFunctionAdaptorType * GetCostFunctionAdaptor();

  /** The purpose of this method is to get around the lack of
   *  const-correctness in VNL cost-functions and optimizers */
  CostFunctionAdaptorType * GetNonConstCostFunctionAdaptor() const;

  /** Print out internal state */
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  /** The purpose of this method is to get around the lack of iteration reporting
   * in VNL optimizers. By interfacing directly with the ITK cost function
   * adaptor we are generating here Iteration Events. Note the iteration events
   * here are produce PER EVALUATION of the metric, not per real iteration of the
   * vnl optimizer. Optimizers that evaluate the metric multiple times at each
   * iteration will generate a lot more of Iteration events here. */
  void IterationReport(const EventObject & event);

  ITK_DISALLOW_COPY_AND_ASSIGN(SingleValuedNonLinearVnlOptimizerv4);

  CostFunctionAdaptorType *m_CostFunctionAdaptor;

  CommandType::Pointer m_Command;

  mutable ParametersType m_CachedCurrentPosition;
  mutable DerivativeType m_CachedDerivative;
};
} // end namespace itk

#endif
