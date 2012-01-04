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
#ifndef __itkGradientDescentOptimizerv4_h
#define __itkGradientDescentOptimizerv4_h

#include "itkGradientDescentOptimizerBasev4.h"
#include "itkOptimizerParameterScalesEstimator.h"
#include "itkWindowConvergenceMonitoringFunction.h"

namespace itk
{
/** \class GradientDescentOptimizerv4
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
 * The user can scale each component of the df / dp
 * but setting a scaling vector using method SetScales().
 *
 * The learning rate defaults to 1.0, and can be set via \c SetLearningRate.
 *
 * The user may set a member m_ScalesEstimator by calling SetScalesEstimator()
 * before optimization to estimate scales and learning rates automatically.
 *
 * When m_ScalesEstimator is set, m_MaximumStepSizeInPhysicalUnits may also
 * be set by the user to change the maximum step size at each iteration. Learning
 * rates are automatically restricted such that each step will produce physical
 * impacts on voxels less than m_MaximumStepSizeInPhysicalUnits.
 * m_MaximumStepSizeInPhysicalUnits defaults to the voxel spacing returned
 * by m_ScalesEstimator->EstimateMaximumStepSize().
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
class ITK_EXPORT GradientDescentOptimizerv4
  : public GradientDescentOptimizerBasev4
{
public:
  /** Standard class typedefs. */
  typedef GradientDescentOptimizerv4     Self;
  typedef GradientDescentOptimizerBasev4 Superclass;
  typedef SmartPointer< Self >           Pointer;
  typedef SmartPointer< const Self >     ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(GradientDescentOptimizerv4, GradientDescentOptimizerBasev4);

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(Self);

  /** Derivative type */
  typedef Superclass::DerivativeType      DerivativeType;

  /** Metric type over which this class is templated */
  typedef Superclass::MeasureType                  MeasureType;
  typedef Superclass::InternalComputationValueType InternalComputationValueType;

  /** Type for the convergence checker */
  typedef itk::Function::WindowConvergenceMonitoringFunction<double>
    ConvergenceMonitoringType;

  /** Set the learning rate. */
  itkSetMacro(LearningRate, InternalComputationValueType);

  /** Get the learning rate. */
  itkGetConstReferenceMacro(LearningRate, InternalComputationValueType);

  /** Set the maximum step size.
   *
   *  When m_ScalesEstimator is set by user, the optimizer will compute
   *  learning rates as
   *      m_MaximumStepSizeInPhysicalUnits /
   *      m_ScalesEstimator->EstimateStepScale(scaledGradient).
   *
   *  If SetMaximumStepSizeInPhysicalUnits is not called by user,
   *  m_MaximumStepSizeInPhysicalUnits defaults to
   *      m_ScalesEstimator->EstimateMaximumStepSize().
   *
   *  where EstimateMaximumStepSize returns one voxel spacing.
   */
  itkSetMacro(MaximumStepSizeInPhysicalUnits, InternalComputationValueType);

  /** Set the scales estimator.
   *
   *  SetScalesEstimator has higher priority than SetScales and SetLearningRate.
   *  The m_ScalesEstimator estimates both parameter scales and step scale.
   *
   *  The optimizer will compute learning rates as
   *      m_MaximumStepSizeInPhysicalUnits /
   *      m_ScalesEstimator->EstimateStepScale(scaledGradient).
   *
   *  If SetMaximumStepSizeInPhysicalUnits is not called by user,
   *  m_MaximumStepSizeInPhysicalUnits defaults to
   *      m_ScalesEstimator->EstimateMaximumStepSize().
   */
  itkSetObjectMacro(ScalesEstimator, OptimizerParameterScalesEstimator);

  /** Minimum convergence value for convergence checking.
   *  The convergence checker calculates convergence value by fitting to
   *  a window of the energy profile. When the convergence value reaches
   *  a small value, it would be treated as converged.
   *
   *  The default m_MinimumConvergenceValue is set to 1e-8 to pass all
   *  tests. It is suggested to use 1e-6 for less stringent convergence
   *  checking.
   */
  itkSetMacro(MinimumConvergenceValue, InternalComputationValueType);

  /** Window size for the convergence checker.
   *  The convergence checker calculates convergence value by fitting to
   *  a window of the energy (metric value) profile.
   *
   *  The default m_ConvergenceWindowSize is set to 50 to pass all
   *  tests. It is suggested to use 10 for less stringent convergence
   *  checking.
   */
  itkSetMacro(ConvergenceWindowSize, SizeValueType);

  /** Start and run the optimization */
  virtual void StartOptimization();

  /** Resume the optimization. Can be called after StopOptimization to
   * resume. The bulk of the optimization work loop is here. */
  virtual void ResumeOptimization();

protected:

  /** Advance one Step following the gradient direction.
   * Includes transform update. */
  virtual void AdvanceOneStep(void);

  /** Modify the gradient over a given index range. */
  virtual void ModifyGradientByScalesOverSubRange( const IndexRangeType& subrange );
  virtual void ModifyGradientByLearningRateOverSubRange( const IndexRangeType& subrange );

  InternalComputationValueType  m_LearningRate;

  /** The maximum step size to restrict learning rates. */
  InternalComputationValueType  m_MaximumStepSizeInPhysicalUnits;

  /** Estimate the learning rate */
  virtual void EstimateLearningRate();

  /** Default constructor */
  GradientDescentOptimizerv4();

  /** Destructor */
  virtual ~GradientDescentOptimizerv4();

  virtual void PrintSelf( std::ostream & os, Indent indent ) const;

  OptimizerParameterScalesEstimator::Pointer m_ScalesEstimator;

  /** Minimum convergence value for convergence checking.
   *  The convergence checker calculates convergence value by fitting to
   *  a window of the energy profile. When the convergence value reaches
   *  a small value, such as 1e-8, it would be treated as converged.
   */
  InternalComputationValueType m_MinimumConvergenceValue;

  /** Window size for the convergence checker.
   *  The convergence checker calculates convergence value by fitting to
   *  a window of the energy (metric value) profile.
   */
  SizeValueType m_ConvergenceWindowSize;

  /** The convergence checker. */
  ConvergenceMonitoringType::Pointer m_ConvergenceMonitoring;

private:
  GradientDescentOptimizerv4( const Self & ); //purposely not implemented
  void operator=( const Self& ); //purposely not implemented
};

} // end namespace itk

#endif
