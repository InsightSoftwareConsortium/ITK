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
#ifndef itkGradientDescentOptimizerv4_h
#define itkGradientDescentOptimizerv4_h

#include "itkGradientDescentOptimizerBasev4.h"

namespace itk
{
  /** \class GradientDescentOptimizerv4Template
   *  \brief Gradient descent optimizer.
   *
   * GradientDescentOptimizer implements a simple gradient descent optimizer.
   * At each iteration the current position is updated according to
   *
   * \f[
   *        p_{n+1} = p_n
   *                + \mbox{learningRate}
   \, \frac{\partial f(p_n) }{\partial p_n}
   * \f]
   *
   * Optionally, the best metric value and matching parameters
   * can be stored and retried via GetValue() and GetCurrentPosition().
   * See SetReturnBestParametersAndValue().
   *
   * Gradient scales can be manually set or automatically estimated,
   * as documented in the base class.
   * The learing rate defaults to 1.0, and can be set in two ways:
   * 1) manually, via \c SetLearningRate().
   * Or,
   * 2) automatically, either at each iteration or only at the first iteration,
   * by assigning a ScalesEstimator via SetScalesEstimator(). When a
   * ScalesEstimator is assigned, the optimizer is enabled by default to estimate
   * learning rate only once, during the first iteration. This behavior can be changed via
   * SetDoEstimateLearningRateAtEveryIteration() and
   * SetDoEstimateLearningRateOnce(). For learning rate to be estimated at each iteration,
   * the user must call SetDoEstimateLearningRateAtEveryIteration(true) and
   * SetDoEstimateLearningRateOnce(false). When enabled, the optimizer computes learning
   * rate(s) such that at each step, each voxel's change in physical space will be less
   * than m_MaximumStepSizeInPhysicalUnits.
   *
   *      m_LearningRate =
   *        m_MaximumStepSizeInPhysicalUnits /
   *        m_ScalesEstimator->EstimateStepScale(scaledGradient)
   *
   * where m_MaximumStepSizeInPhysicalUnits defaults to the voxel spacing returned by
   * m_ScalesEstimator->EstimateMaximumStepSize() (which is typically 1 voxel),
   * and can be set by the user via SetMaximumStepSizeInPhysicalUnits().
   * When SetDoEstimateLearningRateOnce is enabled, the voxel change may become
   * being greater than m_MaximumStepSizeInPhysicalUnits in later iterations.
   *
   * \note Unlike the previous version of GradientDescentOptimizer, this version
   * does not have a "maximize/minimize" option to modify the effect of the metric
   * derivative. The assigned metric is assumed to return a parameter derivative
   * result that "improves" the optimization when *added* to the current
   * parameters via the metric::UpdateTransformParameters method, after the
   * optimizer applies scales and a learning rate.
   *
   * \ingroup ITKOptimizersv4
   */
template<typename TInternalComputationValueType>
class GradientDescentOptimizerv4Template
: public GradientDescentOptimizerBasev4Template<TInternalComputationValueType>
{
public:
  /** Standard class typedefs. */
  typedef GradientDescentOptimizerv4Template                                    Self;
  typedef GradientDescentOptimizerBasev4Template<TInternalComputationValueType> Superclass;
  typedef SmartPointer< Self >                                                  Pointer;
  typedef SmartPointer< const Self >                                            ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(GradientDescentOptimizerv4Template, Superclass);

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(Self);


  /** It should be possible to derive the internal computation type from the class object. */
  typedef TInternalComputationValueType                     InternalComputationValueType;

  /** Derivative type */
  typedef typename Superclass::DerivativeType               DerivativeType;

  /** Metric type over which this class is templated */
  typedef typename Superclass::MeasureType                  MeasureType;
  typedef typename Superclass::IndexRangeType               IndexRangeType;
  typedef typename Superclass::ScalesType                   ScalesType;
  typedef typename Superclass::ParametersType               ParametersType;
  typedef typename Superclass::StopConditionType            StopConditionType;

  /** Set the learning rate. */
  itkSetMacro(LearningRate, TInternalComputationValueType);

  /** Get the learning rate. */
  itkGetConstReferenceMacro(LearningRate, TInternalComputationValueType);

  /** Set the maximum step size, in physical space units.
   *
   *  Only relevant when m_ScalesEstimator is set by user,
   *  and automatic learning rate estimation is enabled.
   *  See main documentation.
   */
  itkSetMacro(MaximumStepSizeInPhysicalUnits, TInternalComputationValueType);

  /** Get the maximum step size, in physical space units. */
  itkGetConstReferenceMacro(MaximumStepSizeInPhysicalUnits, TInternalComputationValueType);

  /** Option to use ScalesEstimator for learning rate estimation at
   * *each* iteration. The estimation overrides the learning rate
   * set by SetLearningRate(). Default is false.
   *
   * \sa SetDoEstimateLearningRateOnce()
   * \sa SetScalesEstimator()
   */
  itkSetMacro(DoEstimateLearningRateAtEachIteration, bool);
  itkGetConstReferenceMacro(DoEstimateLearningRateAtEachIteration, bool);
  itkBooleanMacro(DoEstimateLearningRateAtEachIteration);

  /** Option to use ScalesEstimator for learning rate estimation
   * only *once*, during first iteration. The estimation overrides the
   * learning rate set by SetLearningRate(). Default is true.
   *
   * \sa SetDoEstimateLearningRateAtEachIteration()
   * \sa SetScalesEstimator()
   */
  itkSetMacro(DoEstimateLearningRateOnce, bool);
  itkGetConstReferenceMacro(DoEstimateLearningRateOnce, bool);
  itkBooleanMacro(DoEstimateLearningRateOnce);

  /** Minimum convergence value for convergence checking.
   *  The convergence checker calculates convergence value by fitting to
   *  a window of the energy profile. When the convergence value reaches
   *  a small value, it would be treated as converged.
   *
   *  The default m_MinimumConvergenceValue is set to 1e-8 to pass all
   *  tests. It is suggested to use 1e-6 for less stringent convergence
   *  checking.
   */
  itkSetMacro(MinimumConvergenceValue, TInternalComputationValueType);

  /** Window size for the convergence checker.
   *  The convergence checker calculates convergence value by fitting to
   *  a window of the energy (metric value) profile.
   *
   *  The default m_ConvergenceWindowSize is set to 50 to pass all
   *  tests. It is suggested to use 10 for less stringent convergence
   *  checking.
   */
  itkSetMacro(ConvergenceWindowSize, SizeValueType);

  /** Get current convergence value */
  itkGetConstReferenceMacro( ConvergenceValue, TInternalComputationValueType);

  /** Flag. Set to have the optimizer track and return the best
   *  best metric value and corresponding best parameters that were
   *  calculated during the optimization. This captures the best
   *  solution when the optimizer oversteps or osciallates near the end
   *  of an optimization.
   *  Results are stored in m_CurrentMetricValue and in the assigned metric's
   *  parameters, retrievable via optimizer->GetCurrentPosition().
   *  This option requires additional memory to store the best
   *  parameters, which can be large when working with high-dimensional
   *  transforms such as DisplacementFieldTransform.
   */
  itkSetMacro(ReturnBestParametersAndValue, bool);
  itkGetConstReferenceMacro(ReturnBestParametersAndValue, bool);
  itkBooleanMacro(ReturnBestParametersAndValue);

  /** Start and run the optimization */
  virtual void StartOptimization( bool doOnlyInitialization = false ) ITK_OVERRIDE;

  virtual void StopOptimization(void) ITK_OVERRIDE;

  virtual void ResumeOptimization() ITK_OVERRIDE;

  /** Estimate the learning rate based on the current gradient. */
  virtual void EstimateLearningRate();

protected:

  /** Advance one Step following the gradient direction.
   * Includes transform update. */
  virtual void AdvanceOneStep(void);

  /** Modify the gradient over a given index range. */
  virtual void ModifyGradientByScalesOverSubRange( const IndexRangeType& subrange ) ITK_OVERRIDE;
  virtual void ModifyGradientByLearningRateOverSubRange( const IndexRangeType& subrange ) ITK_OVERRIDE;

  /** Manual learning rate to apply. It is overridden by
   * automatic learning rate estimation if enabled. See main documentation.
   */
  TInternalComputationValueType  m_LearningRate;

  /** Default constructor */
  GradientDescentOptimizerv4Template();

  /** Destructor */
  virtual ~GradientDescentOptimizerv4Template();

  virtual void PrintSelf( std::ostream & os, Indent indent ) const ITK_OVERRIDE;

  /** Minimum convergence value for convergence checking.
   *  The convergence checker calculates convergence value by fitting to
   *  a window of the energy profile. When the convergence value reaches
   *  a small value, such as 1e-8, it would be treated as converged.
   */
  TInternalComputationValueType m_MinimumConvergenceValue;

  /** Current convergence value. */
  /* WindowConvergenceMonitoringFunction always returns output convergence value in 'TInternalComputationValueType' precision */
  TInternalComputationValueType m_ConvergenceValue;

  /** Store the best value and related paramters */
  MeasureType                  m_CurrentBestValue;
  ParametersType               m_BestParameters;

  /** Flag to control returning of best value and parameters. */
  bool m_ReturnBestParametersAndValue;

  /** Store the previous gradient value at each iteration,
   * so we can detect the changes in geradient direction.
   * This is needed by the regular step gradient descent and
   * Quasi newton optimizers.
   */
  DerivativeType m_PreviousGradient;

private:

  GradientDescentOptimizerv4Template( const Self & ); //purposely not implemented
  void operator=( const Self& ); //purposely not implemented
};

/** This helps to meet backward compatibility */
typedef GradientDescentOptimizerv4Template<double> GradientDescentOptimizerv4;

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGradientDescentOptimizerv4.hxx"
#endif

#endif
