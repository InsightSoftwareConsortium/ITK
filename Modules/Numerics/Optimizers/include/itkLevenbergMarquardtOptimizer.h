/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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
#include "ITKOptimizersExport.h"
#include <string>

class vnl_levenberg_marquardt;

namespace itk
{
/** \class LevenbergMarquardtOptimizer
 * \brief Wrap of the vnl_levenberg_marquardt algorithm
 *
 * \ingroup Numerics Optimizers
 * \ingroup ITKOptimizers
 *
 * \sphinx
 * \sphinxexample{Numerics/Optimizers/LevenbergMarquardtOptimization, Levenberg-Marquardt Optimization}
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

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(LevenbergMarquardtOptimizer);

  /** InternalParameters type alias. */
  using InternalParametersType = vnl_vector<double>;

#if !defined(ITK_LEGACY_REMOVE)
  /** Internal optimizer type.
   * \deprecated The minimization is performed by an Eigen-backed engine; this
   * alias is retained only for source compatibility. */
  using InternalOptimizerType = vnl_levenberg_marquardt;

  /** \deprecated Access to the former internal vnl_levenberg_marquardt is no
   * longer meaningful: the optimizer is backed by the Eigen MINPACK-port
   * Levenberg-Marquardt. Returns nullptr. */
  itkLegacyMacro(vnl_levenberg_marquardt * GetOptimizer() const);
#endif

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

  std::string
  GetStopConditionDescription() const override;

protected:
  LevenbergMarquardtOptimizer();
  ~LevenbergMarquardtOptimizer() override;

  using CostFunctionAdaptorType = Superclass::CostFunctionAdaptorType;

private:
  bool         m_OptimizerInitialized{};
  unsigned int m_NumberOfIterations{};
  double       m_ValueTolerance{};
  double       m_GradientTolerance{};
  double       m_EpsilonFunction{};
  std::string  m_StopConditionDescription{};
};
} // end namespace itk

#endif
