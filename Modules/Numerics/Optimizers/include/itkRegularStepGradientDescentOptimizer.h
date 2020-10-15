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
#ifndef itkRegularStepGradientDescentOptimizer_h
#define itkRegularStepGradientDescentOptimizer_h

#include "itkRegularStepGradientDescentBaseOptimizer.h"
#include "ITKOptimizersExport.h"

namespace itk
{
/** \class RegularStepGradientDescentOptimizer
 * \brief Implement a gradient descent optimizer
 *
 * \ingroup Numerics  Optimizers
 *
 * \ingroup ITKOptimizers
 */
class ITKOptimizers_EXPORT RegularStepGradientDescentOptimizer : public RegularStepGradientDescentBaseOptimizer
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(RegularStepGradientDescentOptimizer);

  /** Standard class type aliases. */
  using Self = RegularStepGradientDescentOptimizer;
  using Superclass = RegularStepGradientDescentBaseOptimizer;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(RegularStepGradientDescentOptimizer, RegularStepGradientDescentBaseOptimizer);

  /** Cost function type alias. */
  using CostFunctionType = Superclass::CostFunctionType;
  using CostFunctionPointer = CostFunctionType::Pointer;

protected:
  RegularStepGradientDescentOptimizer() = default;
  ~RegularStepGradientDescentOptimizer() override = default;

  /** Advance one step along the corrected gradient taking into
   * account the steplength represented by factor.
   * This method is invoked by AdvanceOneStep. It is expected
   * to be overridden by optimization methods in non-vector spaces
   * \sa AdvanceOneStep */
  void
  StepAlongGradient(double factor, const DerivativeType & transformedGradient) override;
};
} // end namespace itk

#endif
