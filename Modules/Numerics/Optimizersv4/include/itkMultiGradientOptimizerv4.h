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
#ifndef itkMultiGradientOptimizerv4_h
#define itkMultiGradientOptimizerv4_h

#include "itkObjectToObjectOptimizerBase.h"
#include "itkGradientDescentOptimizerv4.h"

namespace itk
{
  /** \class MultiGradientOptimizerv4Template
   *  \brief Multiple gradient-based optimizers are combined in order to perform a multi-objective optimization.
   *
   *  This optimizer will do a combined gradient descent optimization using whatever metric/optimizer gradient
   *  sub-optimizers are passed to it by the user.  The learning rate or scaleestimator for each sub-optimizer
   *  controls the relative weight of each metric in the optimization.  Denote the weights as \f$ w_1 \f$ and \f$ w_2 \f$ then
   *  the MultiGradientOptimizer will optimize \f$ \sum_i w_i Metric_i \f$ by using update rule:
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
template<typename TInternalComputationValueType>
class ITK_TEMPLATE_EXPORT MultiGradientOptimizerv4Template
: public GradientDescentOptimizerv4Template<TInternalComputationValueType>
{
public:
  /** Standard class typedefs. */
  typedef MultiGradientOptimizerv4Template                             Self;
  typedef GradientDescentOptimizerv4Template<TInternalComputationValueType>  Superclass;
  typedef SmartPointer< Self >                                         Pointer;
  typedef SmartPointer< const Self >                                   ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(MultiGradientOptimizerv4Template, Superclass);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  typedef itk::GradientDescentOptimizerv4Template<TInternalComputationValueType>                   LocalOptimizerType;
  typedef typename itk::GradientDescentOptimizerv4Template<TInternalComputationValueType>::Pointer LocalOptimizerPointer;
  typedef typename Superclass::ParametersType                                                ParametersType;
  typedef ObjectToObjectOptimizerBaseTemplate<TInternalComputationValueType>                       OptimizerType;
  typedef typename OptimizerType::Pointer                                                    OptimizerPointer;
  typedef std::vector< LocalOptimizerPointer >                                               OptimizersListType;
  typedef typename OptimizersListType::size_type                                             OptimizersListSizeType;

  typedef typename Superclass::StopConditionType                                             StopConditionType;

  /** Stop condition return string type */
  typedef typename Superclass::StopConditionReturnStringType StopConditionReturnStringType;

  /** Stop condition internal string type */
  typedef typename Superclass::StopConditionDescriptionType  StopConditionDescriptionType;

  /** It should be possible to derive the internal computation type from the class object. */
  typedef TInternalComputationValueType             InternalComputationValueType;

  /** Metric type over which this class is templated */
  typedef typename Superclass::MetricType           MetricType;
  typedef typename MetricType::Pointer              MetricTypePointer;

  /** Derivative type */
  typedef typename MetricType::DerivativeType       DerivativeType;

  /** Measure type */
  typedef typename Superclass::MeasureType          MeasureType;
  typedef std::vector< MeasureType >                MetricValuesListType;

  /** Get stop condition enum */
  virtual const StopConditionType & GetStopCondition() const ITK_OVERRIDE
    {
    return this->m_StopCondition;
    }

  /** Begin the optimization */
  virtual void StartOptimization( bool doOnlyInitialization = false ) ITK_OVERRIDE;

  /** Stop optimization. The object is left in a state so the
   * optimization can be resumed by calling ResumeOptimization. */
  virtual void StopOptimization(void) ITK_OVERRIDE;

  /** Resume the optimization. Can be called after StopOptimization to
   * resume. The bulk of the optimization work loop is here. */
  virtual void ResumeOptimization() ITK_OVERRIDE;

  /** Get the reason for termination */
  virtual const StopConditionReturnStringType GetStopConditionDescription() const ITK_OVERRIDE;

  /** Get the list of optimizers currently held.  */
  OptimizersListType & GetOptimizersList();

  /** Set the list of optimizers to combine */
  void SetOptimizersList(OptimizersListType & p);

  /** Get the list of metric values that we produced after the multi-objective search.  */
  const MetricValuesListType & GetMetricValuesList() const;

  protected:

  /** Default constructor */
  MultiGradientOptimizerv4Template();
  virtual ~MultiGradientOptimizerv4Template() ITK_OVERRIDE;

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /* Common variables for optimization control and reporting */
  bool                          m_Stop;
  StopConditionType             m_StopCondition;
  StopConditionDescriptionType  m_StopConditionDescription;
  OptimizersListType            m_OptimizersList;
  MetricValuesListType          m_MetricValuesList;
  MeasureType                   m_MinimumMetricValue;
  MeasureType                   m_MaximumMetricValue;

  private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MultiGradientOptimizerv4Template);

};

/** This helps to meet backward compatibility */
typedef MultiGradientOptimizerv4Template<double> MultiGradientOptimizerv4;

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMultiGradientOptimizerv4.hxx"
#endif

#endif
