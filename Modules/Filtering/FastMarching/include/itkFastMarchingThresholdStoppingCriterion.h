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

#ifndef itkFastMarchingThresholdStoppingCriterion_h
#define itkFastMarchingThresholdStoppingCriterion_h

#include "itkFastMarchingStoppingCriterionBase.h"
#include "itkObjectFactory.h"

namespace itk
{
/**
 * \class FastMarchingThresholdStoppingCriterion
 * \brief Stopping Criterion is verified when Current Value is equal to or
 * greater than the provided threshold.
 *
 * \ingroup ITKFastMarching
 */
template <typename TInput, typename TOutput>
class FastMarchingThresholdStoppingCriterion : public FastMarchingStoppingCriterionBase<TInput, TOutput>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(FastMarchingThresholdStoppingCriterion);

  using Self = FastMarchingThresholdStoppingCriterion;
  using Superclass = FastMarchingStoppingCriterionBase<TInput, TOutput>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FastMarchingThresholdStoppingCriterion, FastMarchingStoppingCriterionBase);

  using OutputPixelType = typename Superclass::OutputPixelType;
  using NodeType = typename Superclass::NodeType;

  /** Get/set the threshold used by the stopping criteria. */
  itkSetMacro(Threshold, OutputPixelType);
  itkGetMacro(Threshold, OutputPixelType);

  bool
  IsSatisfied() const override
  {
    return (this->m_CurrentValue >= this->m_Threshold);
  }

  std::string
  GetDescription() const override
  {
    return "Current Value >= Threshold";
  }

protected:
  FastMarchingThresholdStoppingCriterion()
    : Superclass()
    , m_Threshold(NumericTraits<OutputPixelType>::ZeroValue())
  {}

  ~FastMarchingThresholdStoppingCriterion() override = default;

  OutputPixelType m_Threshold;

  void
  SetCurrentNode(const NodeType &) override
  {}

  void
  Reset() override
  {}
};

} // namespace itk
#endif // itkFastMarchingThresholdStoppingCriterion_h
