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
#ifndef itkExhaustiveOptimizer_h
#define itkExhaustiveOptimizer_h

#include "itkIntTypes.h"
#include "itkSingleValuedNonLinearOptimizer.h"
#include "ITKOptimizersExport.h"

namespace itk
{
/** \class ExhaustiveOptimizer
 * \brief Optimizer that fully samples a grid on the parametric space.
 *
 * This optimizer is equivalent to an exahaustive search in a discrete grid
 * defined over the parametric space. The grid is centered on the initial
 * position. The subdivisions of the grid along each one of the dimensions
 * of the parametric space is defined by an array of number of steps.
 *
 * A typical use is to plot the metric space to get an idea of how noisy it
 * is. An example is given below, where it is desired to plot the metric
 * space with respect to translations along x, y and z in a 3D registration
 * application:
 *     Here it is assumed that the transform is Euler3DTransform.
 *
 * \code
 *  OptimizerType::StepsType steps( m_Transform->GetNumberOfParameters() );
 *  steps[0] = 10;
 *  steps[1] = 10;
 *  steps[2] = 10;
 *  m_Optimizer->SetNumberOfSteps( steps );
 *  m_Optimizer->SetStepLength( 2 );
 * \endcode
 *
 * The optimizer throws IterationEvents after every iteration. We use this to plot
 * the metric space in an image as follows:
 *
 * \code
 *  if( itk::IterationEvent().CheckEvent(& event ) )
 *  {
 *    IndexType index;
 *    index[0] = m_Optimizer->GetCurrentIndex()[0];
 *    index[1] = m_Optimizer->GetCurrentIndex()[1];
 *    index[2] = m_Optimizer->GetCurrentIndex()[2];
 *    image->SetPixel( index, m_Optimizer->GetCurrentValue() );
 *  }
 * \endcode
 *
 * The image size is expected to be 11 x 11 x 11.
 *
 * If you wish to use different step lengths along each parametric axis,
 * you can use the SetScales() method. This accepts an array, each element
 * represents the number of subdivisions per step length. For instance scales
 * of [0.5 1 4] along with a step length of 2 will cause the optimizer
 * to search the metric space on a grid with x,y,z spacing of [1 2 8].
 *
 * Physical dimensions of the grid are influenced by both the scales and
 * the number of steps along each dimension, a side of the region is
 * stepLength*(2*numberOfSteps[d]+1)*scaling[d].
 *
 * \ingroup Numerics Optimizers
 * \ingroup ITKOptimizers
 */
class ITKOptimizers_EXPORT ExhaustiveOptimizer:
  public SingleValuedNonLinearOptimizer
{
public:
  /** Standard "Self" typedef. */
  typedef ExhaustiveOptimizer            Self;
  typedef SingleValuedNonLinearOptimizer Superclass;
  typedef SmartPointer< Self >           Pointer;
  typedef SmartPointer< const Self >     ConstPointer;

  typedef Array< SizeValueType > StepsType;
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ExhaustiveOptimizer, SingleValuedNonLinearOptimizer);

  virtual void    StartOptimization(void) ITK_OVERRIDE;

  void StartWalking();

  void ResumeWalking();

  void StopWalking();

  itkSetMacro(StepLength, double);
  itkSetMacro(NumberOfSteps, StepsType);
  itkGetConstReferenceMacro(StepLength, double);
  itkGetConstReferenceMacro(NumberOfSteps, StepsType);
  itkGetConstReferenceMacro(CurrentValue, MeasureType);
  itkGetConstReferenceMacro(MaximumMetricValue, MeasureType);
  itkGetConstReferenceMacro(MinimumMetricValue, MeasureType);
  itkGetConstReferenceMacro(MinimumMetricValuePosition, ParametersType);
  itkGetConstReferenceMacro(MaximumMetricValuePosition, ParametersType);
  itkGetConstReferenceMacro(CurrentIndex, ParametersType);
  itkGetConstReferenceMacro(MaximumNumberOfIterations, SizeValueType);

  /** Get the reason for termination */
  virtual const std::string GetStopConditionDescription() const ITK_OVERRIDE;

protected:
  ExhaustiveOptimizer();
  virtual ~ExhaustiveOptimizer() ITK_OVERRIDE {}
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Advance to the next grid position. */
  void AdvanceOneStep();

  void IncrementIndex(ParametersType & param);

protected:
  MeasureType m_CurrentValue;

  StepsType m_NumberOfSteps;

  SizeValueType m_CurrentIteration;

  bool m_Stop;

  unsigned int m_CurrentParameter;

  double m_StepLength;

  ParametersType m_CurrentIndex;

  SizeValueType m_MaximumNumberOfIterations;

  MeasureType m_MaximumMetricValue;

  MeasureType m_MinimumMetricValue;

  ParametersType m_MinimumMetricValuePosition;

  ParametersType m_MaximumMetricValuePosition;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ExhaustiveOptimizer);

  std::ostringstream m_StopConditionDescription;
};
} // end namespace itk

#endif
