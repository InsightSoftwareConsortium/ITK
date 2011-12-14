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
 * by m_ScalesEstimator.
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

  /** Set the learning rate. */
  itkSetMacro(LearningRate, InternalComputationValueType);

  /** Get the learning rate. */
  itkGetConstReferenceMacro(LearningRate, InternalComputationValueType);

  /** Set the maximum step size. */
  itkSetMacro(MaximumStepSizeInPhysicalUnits, InternalComputationValueType);

  /** Set the scales estimator. */
  itkSetObjectMacro(ScalesEstimator, OptimizerParameterScalesEstimator);

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

private:
  GradientDescentOptimizerv4( const Self & ); //purposely not implemented
  void operator=( const Self& ); //purposely not implemented
};

} // end namespace itk

#endif
