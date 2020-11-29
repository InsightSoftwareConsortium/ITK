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
#ifndef itkHeavisideStepFunctionBase_h
#define itkHeavisideStepFunctionBase_h

#include "itkFunctionBase.h"
#include "itkConceptChecking.h"

namespace itk
{
/** \class HeavisideStepFunctionBase
 *
 * \brief Base class of the Heaviside function.
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
class HeavisideStepFunctionBase : public FunctionBase<TInput, TOutput>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(HeavisideStepFunctionBase);

  using Self = HeavisideStepFunctionBase;
  using Superclass = FunctionBase<TInput, TOutput>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information */
  itkTypeMacro(HeavisideStepFunctionBase, FunctionBase);


  using InputType = typename Superclass::InputType;
  using OutputType = typename Superclass::OutputType;

  /** Evaluate at the specified input position */
  OutputType
  Evaluate(const InputType & input) const override = 0;

  /** Evaluate the derivative at the specified input position */
  virtual OutputType
  EvaluateDerivative(const InputType & input) const = 0;

#ifdef ITK_USE_CONCEPT_CHECKING
  itkConceptMacro(DoubleConvertibleToInputCheck, (Concept::Convertible<double, TInput>));

  itkConceptMacro(DoubleConvertibleToOutputCheck, (Concept::Convertible<double, TOutput>));
#endif // ITK_USE_CONCEPT_CHECKING

protected:
  HeavisideStepFunctionBase()
    : Superclass()
  {}
  ~HeavisideStepFunctionBase() override = default;
};
} // namespace itk

#endif
