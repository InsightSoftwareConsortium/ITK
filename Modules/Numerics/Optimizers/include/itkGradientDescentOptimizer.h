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
#ifndef itkGradientDescentOptimizer_h
#define itkGradientDescentOptimizer_h

#include "itkIntTypes.h"
#include "itkSingleValuedNonLinearOptimizer.h"
#include "ITKOptimizersExport.h"
#include <string>
namespace itk
{
/** \class GradientDescentOptimizer
 * \brief Implement a gradient descent optimizer
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
 * The learning rate is a fixed scalar defined via SetLearningRate().
 * The optimizer steps through a user defined number of iterations;
 * no convergence checking is done.
 *
 * Additionally, user can scale each component,
 * \f$ \partial f / \partial p \f$,
 * by setting a scaling vector using method SetScale().
 *
 * \sa RegularStepGradientDescentOptimizer
 *
 * \ingroup Numerics Optimizers
 * \ingroup ITKOptimizers
 */
class ITKOptimizers_EXPORT GradientDescentOptimizer:
  public SingleValuedNonLinearOptimizer
{
public:
  /** Standard class typedefs. */
  typedef GradientDescentOptimizer       Self;
  typedef SingleValuedNonLinearOptimizer Superclass;
  typedef SmartPointer< Self >           Pointer;
  typedef SmartPointer< const Self >     ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GradientDescentOptimizer, SingleValuedNonLinearOptimizer);

  /** Codes of stopping conditions */
  typedef enum {
    MaximumNumberOfIterations,
    MetricError
    } StopConditionType;

  /** Methods to configure the cost function. */
  itkGetConstReferenceMacro(Maximize, bool);
  itkSetMacro(Maximize, bool);
  itkBooleanMacro(Maximize);
  bool GetMinimize() const
  { return !m_Maximize; }
  void SetMinimize(bool v)
  { this->SetMaximize(!v); }
  void MinimizeOn()
  { this->MaximizeOff(); }
  void MinimizeOff()
  { this->MaximizeOn(); }

  /** Advance one step following the gradient direction. */
  virtual void AdvanceOneStep();

  /** Start optimization. */
  virtual void    StartOptimization(void) ITK_OVERRIDE;

  /** Resume previously stopped optimization with current parameters
   * \sa StopOptimization. */
  void    ResumeOptimization();

  /** Stop optimization.
   * \sa ResumeOptimization */
  void    StopOptimization();

  /** Set the learning rate. */
  itkSetMacro(LearningRate, double);

  /** Get the learning rate. */
  itkGetConstReferenceMacro(LearningRate, double);

  /** Set the number of iterations. */
  itkSetMacro(NumberOfIterations, SizeValueType);

  /** Get the number of iterations. */
  itkGetConstReferenceMacro(NumberOfIterations, SizeValueType);

  /** Get the current iteration number. */
  itkGetConstMacro(CurrentIteration, SizeValueType);

  /** Get the current value. */
  itkGetConstReferenceMacro(Value, double);

  /** Get Stop condition. */
  itkGetConstReferenceMacro(StopCondition, StopConditionType);
  virtual const std::string GetStopConditionDescription() const ITK_OVERRIDE;

  /** Get Gradient condition. */
  itkGetConstReferenceMacro(Gradient, DerivativeType);

protected:
  GradientDescentOptimizer();
  virtual ~GradientDescentOptimizer() ITK_OVERRIDE {}
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  // made protected so subclass can access
  DerivativeType m_Gradient;

  bool m_Maximize;

  double m_LearningRate;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(GradientDescentOptimizer);

  bool               m_Stop;
  double             m_Value;
  StopConditionType  m_StopCondition;
  SizeValueType      m_NumberOfIterations;
  SizeValueType      m_CurrentIteration;
  std::ostringstream m_StopConditionDescription;
};
} // end namespace itk

#endif
