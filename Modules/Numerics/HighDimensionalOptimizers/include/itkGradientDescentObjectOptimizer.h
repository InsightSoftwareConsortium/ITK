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
#ifndef __itkGradientDescentObjectOptimizer_h
#define __itkGradientDescentObjectOptimizer_h

#include "itkGradientDescentObjectOptimizerBase.h"

namespace itk
{
/** \class GradientDescentObjectOptimizer
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
 * \note Unlike the previous version of GradientDescentOptimizer, this version
 * does not have a "maximize/minimize" option to modify the effect of the metric
 * derivative.
 *
 * \note The assigned metric is assumed to return a parameter derivative
 * result that "improves" the optimization when *added* to the current
 * parameters via the metric::UpateTransformParameters method, after the
 * optimizer applies scales and a learning rate.
 *
 * \ingroup ITKHighDimensionalOptimizers
 */
class ITK_EXPORT GradientDescentObjectOptimizer
  : public GradientDescentObjectOptimizerBase
{
public:
  /** Standard class typedefs. */
  typedef GradientDescentObjectOptimizer       Self;
  typedef GradientDescentObjectOptimizerBase   Superclass;
  typedef SmartPointer< Self >                 Pointer;
  typedef SmartPointer< const Self >           ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(GradientDescentObjectOptimizer, GradientDescentObjectOptimizerBase);

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
  virtual void ModifyGradientOverSubRange( const IndexRangeType& subrange );

  InternalComputationValueType  m_LearningRate;

  /** Default constructor */
  GradientDescentObjectOptimizer();

  /** Destructor */
  virtual ~GradientDescentObjectOptimizer();

  virtual void PrintSelf( std::ostream & os, Indent indent ) const;
private:

  //purposely not implemented
  GradientDescentObjectOptimizer( const Self & );
  //purposely not implemented
  void operator=( const Self& );

};

} // end namespace itk

#endif
