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
#ifndef itkHeavisideStepFunction_h
#define itkHeavisideStepFunction_h

#include "itkHeavisideStepFunctionBase.h"
#include "itkNumericTraits.h"

namespace itk
{
/** \class HeavisideStepFunction
 *
 * \brief Implementation of the classical Heaviside step function.
 *
 * The Heaviside Step function is a piece-wise function:
 *
 *     http://en.wikipedia.org/wiki/Heaviside_step_function
 *
 *
 * \author Mosaliganti K., Smith B., Gelas A., Gouaillard A., Megason S.
 *
 *  This code was taken from the Insight Journal paper:
 *
 *      "Cell Tracking using Coupled Active Surfaces for Nuclei and Membranes"
 *      https://www.insight-journal.org/browse/publication/642
 *
 *  That is based on the papers:
 *
 *      "Level Set Segmentation: Active Contours without edge"
 *      https://www.insight-journal.org/browse/publication/322
 *
 *      and
 *
 *      "Level set segmentation using coupled active surfaces"
 *      https://www.insight-journal.org/browse/publication/323
 *
 *
 * \ingroup ITKCommon
 */
template <typename TInput = float, typename TOutput = double>
class ITK_TEMPLATE_EXPORT HeavisideStepFunction : public HeavisideStepFunctionBase<TInput, TOutput>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(HeavisideStepFunction);

  using Self = HeavisideStepFunction;
  using Superclass = HeavisideStepFunctionBase<TInput, TOutput>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  itkNewMacro(Self);

  itkTypeMacro(HeavisideStepFunction, HeavisideStepFunctionBase);

  using InputType = typename Superclass::InputType;
  using OutputType = typename Superclass::OutputType;

  /** Evaluate at the specified input position */
  OutputType
  Evaluate(const InputType & input) const override;

  /** Evaluate the derivative at the specified input position */
  OutputType
  EvaluateDerivative(const InputType & input) const override;

protected:
  HeavisideStepFunction();
  ~HeavisideStepFunction() override = default;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkHeavisideStepFunction.hxx"
#endif

#endif
