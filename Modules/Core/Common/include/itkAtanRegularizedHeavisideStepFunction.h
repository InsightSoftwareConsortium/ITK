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
#ifndef itkAtanRegularizedHeavisideStepFunction_h
#define itkAtanRegularizedHeavisideStepFunction_h

#include "itkRegularizedHeavisideStepFunction.h"

namespace itk
{
/** \class AtanRegularizedHeavisideStepFunction
 *
 * \brief Atan-based implementation of the Regularized (smoothed) Heaviside functions.
 *
 * \author Mosaliganti K., Smith B., Gelas A., Gouaillard A., Megason S.
 *
 *  This code was taken from the Insight Journal paper:
 *
 *      "Cell Tracking using Coupled Active Surfaces for Nuclei and Membranes"
 *      http://www.insight-journal.org/browse/publication/642
 *      https://hdl.handle.net/10380/3055
 *
 *  That is based on the papers:
 *
 *      "Level Set Segmentation: Active Contours without edge"
 *      http://www.insight-journal.org/browse/publication/322
 *      https://hdl.handle.net/1926/1532
 *
 *      and
 *
 *      "Level set segmentation using coupled active surfaces"
 *      http://www.insight-journal.org/browse/publication/323
 *      https://hdl.handle.net/1926/1533
 *
 *
 * \ingroup ITKCommon
 */
template <typename TInput = float, typename TOutput = double>
class ITK_TEMPLATE_EXPORT AtanRegularizedHeavisideStepFunction
  : public RegularizedHeavisideStepFunction<TInput, TOutput>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(AtanRegularizedHeavisideStepFunction);

  using Self = AtanRegularizedHeavisideStepFunction;
  using Superclass = RegularizedHeavisideStepFunction<TInput, TOutput>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  itkNewMacro(Self);

  itkTypeMacro(AtanRegularizedHeavisideStepFunction, RegularizedHeavisideStepFunction);

  using InputType = typename Superclass::InputType;
  using OutputType = typename Superclass::OutputType;
  using RealType = typename Superclass::RealType;

  /** Evaluate at the specified input position */
  OutputType
  Evaluate(const InputType & input) const override;

  /** Evaluate the derivative at the specified input position */
  OutputType
  EvaluateDerivative(const InputType & input) const override;

protected:
  AtanRegularizedHeavisideStepFunction() = default;
  ~AtanRegularizedHeavisideStepFunction() override = default;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkAtanRegularizedHeavisideStepFunction.hxx"
#endif

#endif
