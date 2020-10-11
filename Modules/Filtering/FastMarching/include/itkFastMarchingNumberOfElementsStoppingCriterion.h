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

#ifndef itkFastMarchingNumberOfElementsStoppingCriterion_h
#define itkFastMarchingNumberOfElementsStoppingCriterion_h

#include "itkFastMarchingStoppingCriterionBase.h"
#include "itkObjectFactory.h"

namespace itk
{
/**
 * \class FastMarchingNumberOfElementsStoppingCriterion
 * \brief Stopping Criterion is verified when Current Number of Elements is equal
 * to or greater than the provided Target Number Of Elements.
 *
 * \note For itk::Image, one element is one pixel. So the number of elements is directly
 * linked to the physical size of the object, i.e.
 * \f$ PhysicalSize = TargetNumberOfElements \cdot \prod_{i=1}{dim} Spacing_{i} \f$
 *
 * \note For itk::QuadEdgeMesh, one element is one vertex.
 *
 * \ingroup ITKFastMarching
 */
template <typename TInput, typename TOutput>
class FastMarchingNumberOfElementsStoppingCriterion : public FastMarchingStoppingCriterionBase<TInput, TOutput>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(FastMarchingNumberOfElementsStoppingCriterion);

  using Self = FastMarchingNumberOfElementsStoppingCriterion;
  using Superclass = FastMarchingStoppingCriterionBase<TInput, TOutput>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FastMarchingNumberOfElementsStoppingCriterion, FastMarchingStoppingCriterionBase);

  using OutputPixelType = typename Superclass::OutputPixelType;
  using NodeType = typename Superclass::NodeType;

  /** Get/set the threshold used by the stopping criteria. */
  itkSetMacro(TargetNumberOfElements, IdentifierType);
  itkGetMacro(TargetNumberOfElements, IdentifierType);

  bool
  IsSatisfied() const override
  {
    return (this->m_CurrentNumberOfElements >= this->m_TargetNumberOfElements);
  }

  std::string
  GetDescription() const override
  {
    return "Current Number of Elements >= Target Number of Elements";
  }

protected:
  FastMarchingNumberOfElementsStoppingCriterion()
    : Superclass()
    , m_CurrentNumberOfElements(NumericTraits<IdentifierType>::ZeroValue())
    , m_TargetNumberOfElements(NumericTraits<IdentifierType>::ZeroValue())
  {}

  ~FastMarchingNumberOfElementsStoppingCriterion() override = default;

  IdentifierType m_CurrentNumberOfElements;
  IdentifierType m_TargetNumberOfElements;

  void
  SetCurrentNode(const NodeType &) override
  {
    ++this->m_CurrentNumberOfElements;
  }

  void
  Reset() override
  {
    this->m_CurrentNumberOfElements = NumericTraits<IdentifierType>::ZeroValue();
  }
};

} // namespace itk
#endif // itkFastMarchingNumberOfElementsStoppingCriterion_h
