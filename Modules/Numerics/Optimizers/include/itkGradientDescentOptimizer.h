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
#ifndef itkGradientDescentOptimizer_h
#define itkGradientDescentOptimizer_h

#include "itkIntTypes.h"
#include "itkSingleValuedNonLinearOptimizer.h"
#include "ITKOptimizersExport.h"
#include <string>
namespace itk
{
/**\class GradientDescentOptimizerEnums
 * \brief Contains all enum classes in the GradientDescentOptimizer class.
 * \ingroup ITKOptimizers
 */
class GradientDescentOptimizerEnums
{
public:
  /** \class StopConditionGradientDescentOptimizer
   * \ingroup ITKOptimizers
   * Codes of stopping conditions */
  enum class StopConditionGradientDescentOptimizer : uint8_t
  {
    MaximumNumberOfIterations,
    MetricError
  };
};
// Define how to print enumeration
extern ITKOptimizers_EXPORT std::ostream &
                            operator<<(std::ostream & out, const GradientDescentOptimizerEnums::StopConditionGradientDescentOptimizer value);

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
class ITKOptimizers_EXPORT GradientDescentOptimizer : public SingleValuedNonLinearOptimizer
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(GradientDescentOptimizer);

  /** Standard class type aliases. */
  using Self = GradientDescentOptimizer;
  using Superclass = SingleValuedNonLinearOptimizer;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GradientDescentOptimizer, SingleValuedNonLinearOptimizer);

  using StopConditionGradientDescentOptimizerEnum =
    GradientDescentOptimizerEnums::StopConditionGradientDescentOptimizer;
#if !defined(ITK_LEGACY_REMOVE)
  // We need to expose the enum values at the class level
  // for backwards compatibility
  static constexpr StopConditionGradientDescentOptimizerEnum MaximumNumberOfIterations =
    StopConditionGradientDescentOptimizerEnum::MaximumNumberOfIterations;
  static constexpr StopConditionGradientDescentOptimizerEnum MetricError =
    StopConditionGradientDescentOptimizerEnum::MetricError;
#endif

  /** Methods to configure the cost function. */
  itkGetConstReferenceMacro(Maximize, bool);
  itkSetMacro(Maximize, bool);
  itkBooleanMacro(Maximize);
  bool
  GetMinimize() const
  {
    return !m_Maximize;
  }
  void
  SetMinimize(bool v)
  {
    this->SetMaximize(!v);
  }
  void
  MinimizeOn()
  {
    this->MaximizeOff();
  }
  void
  MinimizeOff()
  {
    this->MaximizeOn();
  }

  /** Advance one step following the gradient direction. */
  virtual void
  AdvanceOneStep();

  /** Start optimization. */
  void
  StartOptimization() override;

  /** Resume previously stopped optimization with current parameters
   * \sa StopOptimization. */
  void
  ResumeOptimization();

  /** Stop optimization.
   * \sa ResumeOptimization */
  void
  StopOptimization();

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
  itkGetConstReferenceMacro(StopCondition, StopConditionGradientDescentOptimizerEnum);
  const std::string
  GetStopConditionDescription() const override;

  /** Get Gradient condition. */
  itkGetConstReferenceMacro(Gradient, DerivativeType);

protected:
  GradientDescentOptimizer();
  ~GradientDescentOptimizer() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  // made protected so subclass can access
  DerivativeType m_Gradient;

  bool m_Maximize{ false };

  double m_LearningRate{ 1.0 };

private:
  bool                                      m_Stop{ false };
  double                                    m_Value{ 0.0 };
  StopConditionGradientDescentOptimizerEnum m_StopCondition{
    StopConditionGradientDescentOptimizerEnum::MaximumNumberOfIterations
  };
  SizeValueType      m_NumberOfIterations{ 100 };
  SizeValueType      m_CurrentIteration{ 0 };
  std::ostringstream m_StopConditionDescription;
};

// Define how to print enumeration
extern ITKOptimizers_EXPORT std::ostream &
                            operator<<(std::ostream & out, const GradientDescentOptimizer::StopConditionGradientDescentOptimizerEnum value);

} // end namespace itk

#endif
