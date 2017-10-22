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
#ifndef itkGradientDescentOptimizerBasev4_h
#define itkGradientDescentOptimizerBasev4_h

#include "itkObjectToObjectOptimizerBase.h"
#include "itkWindowConvergenceMonitoringFunction.h"
#include "itkThreadedIndexedContainerPartitioner.h"
#include "itkDomainThreader.h"

namespace itk
{
/** \class GradientDescentOptimizerBasev4
 *  \brief Abstract base class for gradient descent-style optimizers.
 *
 * Gradient modification is threaded in \c ModifyGradient.
 *
 * Derived classes must override \c ModifyGradientByScalesOverSubRange,
 * \c ModifyGradientByLearningRateOverSubRange and \c ResumeOptimization.
 *
 * \ingroup ITKOptimizersv4
 */
template<typename TInternalComputationValueType>
class ITK_TEMPLATE_EXPORT GradientDescentOptimizerBasev4Template
  : public ObjectToObjectOptimizerBaseTemplate<TInternalComputationValueType>
{
public:
  /** Standard class typedefs. */
  typedef GradientDescentOptimizerBasev4Template                       Self;
  typedef ObjectToObjectOptimizerBaseTemplate<TInternalComputationValueType> Superclass;
  typedef SmartPointer< Self >                                         Pointer;
  typedef SmartPointer< const Self >                                   ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(GradientDescentOptimizerBasev4Template, Superclass);

  /** Codes of stopping conditions. */
  typedef enum {
    MAXIMUM_NUMBER_OF_ITERATIONS,
    COSTFUNCTION_ERROR,
    UPDATE_PARAMETERS_ERROR,
    STEP_TOO_SMALL,
    CONVERGENCE_CHECKER_PASSED,
    GRADIENT_MAGNITUDE_TOLEARANCE,
    OTHER_ERROR
    } StopConditionType;

  /** Stop condition return string type */
  typedef typename Superclass::StopConditionReturnStringType StopConditionReturnStringType;

  /** Stop condition internal string type */
  typedef typename Superclass::StopConditionDescriptionType  StopConditionDescriptionType;

  /** It should be possible to derive the internal computation type from the class object. */
  typedef TInternalComputationValueType          InternalComputationValueType;

  /** Metric type over which this class is templated */
  typedef typename Superclass::MetricType                    MetricType;
  typedef typename MetricType::Pointer                       MetricTypePointer;

  /** Derivative type */
  typedef typename Superclass::DerivativeType                DerivativeType;

  /** Measure type */
  typedef typename Superclass::MeasureType                   MeasureType;

  typedef typename Superclass::ScalesType                    ScalesType;

  typedef typename Superclass::ParametersType                ParametersType;

  /** Type for the convergence checker */
  typedef itk::Function::WindowConvergenceMonitoringFunction<TInternalComputationValueType>
  ConvergenceMonitoringType;

  /** Get the most recent gradient values. */
  itkGetConstReferenceMacro( Gradient, DerivativeType );

  /** Get stop condition enum */
  itkGetConstReferenceMacro(StopCondition, StopConditionType);

  /** Set the number of iterations. */
  virtual void SetNumberOfIterations( const SizeValueType numberOfIterations ) ITK_OVERRIDE
    {
    itkDebugMacro("setting NumberOfIterations to " << numberOfIterations );
    if ( this->m_NumberOfIterations != numberOfIterations)
      {
      this->m_NumberOfIterations = numberOfIterations;
      this->Modified();
      }
    }

  /** Get the number of iterations. */
  virtual SizeValueType GetNumberOfIterations() const ITK_OVERRIDE
    {
    return this->m_NumberOfIterations;
    }

  /** Get the current iteration number. */
  virtual SizeValueType GetCurrentIteration() const ITK_OVERRIDE
    {
    return this->m_CurrentIteration;
    }

  /** Start and run the optimization */
  virtual void StartOptimization( bool doOnlyInitialization = false ) ITK_OVERRIDE;

  /** Resume optimization.
   * This runs the optimization loop, and allows continuation
   * of stopped optimization */
  virtual void ResumeOptimization() = 0;

  /** Stop optimization. The object is left in a state so the
   * optimization can be resumed by calling ResumeOptimization. */
  virtual void StopOptimization();

  /** Get the reason for termination */
  virtual const StopConditionReturnStringType GetStopConditionDescription() const ITK_OVERRIDE;

  /** Modify the gradient in place, to advance the optimization.
   * This call performs a threaded modification for transforms with
   * local support (assumed to be dense). Otherwise the modification
   * is performed w/out threading.
   * See EstimateLearningRate() to perform optionaly learning rate
   * estimation.
   * At completion, m_Gradient can be used to update the transform
   * parameters. Derived classes may hold additional results in
   * other member variables.
   *
   * \sa EstimateLearningRate()
   */
  virtual void ModifyGradientByScales();
  virtual void ModifyGradientByLearningRate();

  typedef ThreadedIndexedContainerPartitioner::IndexRangeType IndexRangeType;

  /** Derived classes define this worker method to modify the gradient by scales.
   * Modifications must be performed over the index range defined in
   * \c subrange.
   * Called from ModifyGradientByScales(), either directly or via threaded
   * operation. */
  virtual void ModifyGradientByScalesOverSubRange( const IndexRangeType& subrange ) = 0;

  /** Derived classes define this worker method to modify the gradient by learning rates.
   * Modifications must be performed over the index range defined in
   * \c subrange.
   * Called from ModifyGradientByLearningRate(), either directly or via threaded
   * operation.
   * This function is used in GradientDescentOptimizerBasev4ModifyGradientByScalesThreaderTemplate
   * and GradientDescentOptimizerBasev4ModifyGradientByLearningRateThreaderTemplate classes.
   */
  virtual void ModifyGradientByLearningRateOverSubRange( const IndexRangeType& subrange ) = 0;

protected:

  /** Default constructor */
  GradientDescentOptimizerBasev4Template();
  virtual ~GradientDescentOptimizerBasev4Template() ITK_OVERRIDE;

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
  TInternalComputationValueType  m_MaximumStepSizeInPhysicalUnits;

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
  bool                          m_Stop;
  StopConditionType             m_StopCondition;
  StopConditionDescriptionType  m_StopConditionDescription;

  /** Current gradient */
  DerivativeType     m_Gradient;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:

  ITK_DISALLOW_COPY_AND_ASSIGN(GradientDescentOptimizerBasev4Template);

};

/** This helps to meet backward compatibility */
typedef GradientDescentOptimizerBasev4Template<double> GradientDescentOptimizerBasev4;

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGradientDescentOptimizerBasev4.hxx"
#endif

#endif
