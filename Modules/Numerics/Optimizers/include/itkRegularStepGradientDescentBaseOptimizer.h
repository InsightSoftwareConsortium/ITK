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
#ifndef itkRegularStepGradientDescentBaseOptimizer_h
#define itkRegularStepGradientDescentBaseOptimizer_h

#include "itkIntTypes.h"
#include "itkSingleValuedNonLinearOptimizer.h"
#include "ITKOptimizersExport.h"

namespace itk
{
/** \class RegularStepGradientDescentBaseOptimizer
 * \brief Implement a gradient descent optimizer
 *
 * \ingroup Numerics Optimizers
 * \ingroup ITKOptimizers
 */
class ITKOptimizers_EXPORT RegularStepGradientDescentBaseOptimizer:
  public SingleValuedNonLinearOptimizer
{
public:
  /** Standard "Self" typedef. */
  typedef RegularStepGradientDescentBaseOptimizer Self;
  typedef SingleValuedNonLinearOptimizer          Superclass;
  typedef SmartPointer< Self >                    Pointer;
  typedef SmartPointer< const Self >              ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(RegularStepGradientDescentBaseOptimizer,
               SingleValuedNonLinearOptimizer);

  /** Codes of stopping conditions. */
  typedef enum {
    GradientMagnitudeTolerance = 1,
    StepTooSmall = 2,
    ImageNotAvailable = 3,
    CostFunctionError = 4,
    MaximumNumberOfIterations = 5,
    Unknown = 6
    } StopConditionType;

  /** Specify whether to minimize or maximize the cost function. */
  itkSetMacro(Maximize, bool);
  itkGetConstReferenceMacro(Maximize, bool);
  itkBooleanMacro(Maximize);
  bool GetMinimize() const
  { return !m_Maximize; }
  void SetMinimize(bool v)
  { this->SetMaximize(!v); }
  void    MinimizeOn(void)
  { SetMaximize(false); }
  void    MinimizeOff(void)
  { SetMaximize(true); }

  /** Start optimization. */
  virtual void    StartOptimization(void) ITK_OVERRIDE;

  /** Resume previously stopped optimization with current parameters.
   * \sa StopOptimization */
  void    ResumeOptimization();

  /** Stop optimization.
   * \sa ResumeOptimization */
  void    StopOptimization();

  /** Set/Get parameters to control the optimization process. */
  itkSetMacro(MaximumStepLength, double);
  itkSetMacro(MinimumStepLength, double);
  itkSetMacro(RelaxationFactor, double);
  itkSetMacro(NumberOfIterations, SizeValueType);
  itkSetMacro(GradientMagnitudeTolerance, double);
  itkGetConstReferenceMacro(CurrentStepLength, double);
  itkGetConstReferenceMacro(MaximumStepLength, double);
  itkGetConstReferenceMacro(MinimumStepLength, double);
  itkGetConstReferenceMacro(RelaxationFactor, double);
  itkGetConstReferenceMacro(NumberOfIterations, SizeValueType);
  itkGetConstReferenceMacro(GradientMagnitudeTolerance, double);
  itkGetConstMacro(CurrentIteration, unsigned int);
  itkGetConstReferenceMacro(StopCondition, StopConditionType);
  itkGetConstReferenceMacro(Value, MeasureType);
  itkGetConstReferenceMacro(Gradient, DerivativeType);

  /** Get the reason for termination */
  virtual const std::string GetStopConditionDescription() const ITK_OVERRIDE;

protected:
  RegularStepGradientDescentBaseOptimizer();
  virtual ~RegularStepGradientDescentBaseOptimizer() ITK_OVERRIDE {}
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Advance one step following the gradient direction
   * This method verifies if a change in direction is required
   * and if a reduction in steplength is required. */
  virtual void AdvanceOneStep();

  /** Advance one step along the corrected gradient taking into
   * account the steplength represented by factor.
   * This method is invoked by AdvanceOneStep. It is expected
   * to be overrided by optimization methods in non-vector spaces
   * \sa AdvanceOneStep */
  virtual void StepAlongGradient(
    double,
    const DerivativeType &)
  {
    ExceptionObject ex;

    ex.SetLocation(__FILE__);
    ex.SetDescription("This method MUST be overloaded in derived classes");
    throw ex;
  }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(RegularStepGradientDescentBaseOptimizer);

protected:
  DerivativeType m_Gradient;
  DerivativeType m_PreviousGradient;

  bool               m_Stop;
  bool               m_Maximize;
  MeasureType        m_Value;
  double             m_GradientMagnitudeTolerance;
  double             m_MaximumStepLength;
  double             m_MinimumStepLength;
  double             m_CurrentStepLength;
  double             m_RelaxationFactor;
  StopConditionType  m_StopCondition;
  SizeValueType      m_NumberOfIterations;
  SizeValueType      m_CurrentIteration;
  std::ostringstream m_StopConditionDescription;
};
} // end namespace itk

#endif
