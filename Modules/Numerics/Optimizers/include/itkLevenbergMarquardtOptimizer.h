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
#ifndef itkLevenbergMarquardtOptimizer_h
#define itkLevenbergMarquardtOptimizer_h

#include "itkMultipleValuedNonLinearVnlOptimizer.h"
#include "vnl/algo/vnl_levenberg_marquardt.h"
#include "ITKOptimizersExport.h"

namespace itk
{
/** \class LevenbergMarquardtOptimizer
 * \brief Wrap of the vnl_levenberg_marquardt algorithm
 *
 * \ingroup Numerics Optimizers
 * \ingroup ITKOptimizers
 *
 * \sphinx
 * \sphinxexample{Numerics/Optimizers/LevenbergMarquardtOptimization, Levenberg-Marquardt Optimiztion}
 * \endsphinx
 */
class ITKOptimizers_EXPORT LevenbergMarquardtOptimizer : public MultipleValuedNonLinearVnlOptimizer
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(LevenbergMarquardtOptimizer);

  /** Standard "Self" type alias. */
  using Self = LevenbergMarquardtOptimizer;
  using Superclass = MultipleValuedNonLinearVnlOptimizer;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(LevenbergMarquardtOptimizer, MultipleValuedNonLinearVnlOptimizer);

  /** InternalParameters type alias. */
  using InternalParametersType = vnl_vector<double>;

  /** Internal optimizer type. */
  using InternalOptimizerType = vnl_levenberg_marquardt;

  /** Method for getting access to the internal optimizer. */
  vnl_levenberg_marquardt *
  GetOptimizer() const;

  /** Start optimization with an initial value. */
  void
  StartOptimization() override;

  /** Plug in a Cost Function into the optimizer  */
  void
  SetCostFunction(MultipleValuedCostFunction * costFunction) override;

  void
  SetNumberOfIterations(unsigned int iterations);

  void
  SetValueTolerance(double tol);

  void
  SetGradientTolerance(double tol);

  void
  SetEpsilonFunction(double epsilon);

  /** Get the current value */
  MeasureType
  GetValue() const;

  const std::string
  GetStopConditionDescription() const override;

protected:
  LevenbergMarquardtOptimizer();
  ~LevenbergMarquardtOptimizer() override;

  using CostFunctionAdaptorType = Superclass::CostFunctionAdaptorType;

private:
  bool                    m_OptimizerInitialized;
  InternalOptimizerType * m_VnlOptimizer;
  unsigned int            m_NumberOfIterations;
  double                  m_ValueTolerance;
  double                  m_GradientTolerance;
  double                  m_EpsilonFunction;
};
} // end namespace itk

#endif
