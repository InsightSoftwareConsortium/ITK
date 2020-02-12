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

#ifndef itkFastMarchingStoppingCriterionBase_h
#define itkFastMarchingStoppingCriterionBase_h

#include "itkStoppingCriterionBase.h"
#include "itkNumericTraits.h"
#include "itkFastMarchingTraits.h"

namespace itk
{

/**
 *\class FastMarchingStoppingCriterionBase
  \brief Abstract Stopping Criterion dedicated for Fast Marching Methods

  \ingroup ITKFastMarching
  */
template <typename TInput, typename TOutput>
class FastMarchingStoppingCriterionBase : public StoppingCriterionBase
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(FastMarchingStoppingCriterionBase);

  using Self = FastMarchingStoppingCriterionBase;
  using Superclass = StoppingCriterionBase;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using Traits = FastMarchingTraits<TInput, TOutput>;

  using NodeType = typename Traits::NodeType;
  using OutputPixelType = typename Traits::OutputPixelType;
  using NodePairType = typename Traits::NodePairType;
  using OutputDomainType = typename Traits::OutputDomainType;
  using OutputDomainPointer = typename Traits::OutputDomainPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(FastMarchingStoppingCriterionBase, StoppingCriterionBase);

  /** Reinitialize internal values. */
  void
  Reinitialize()
  {
    m_CurrentValue = NumericTraits<OutputPixelType>::ZeroValue();
    m_PreviousValue = NumericTraits<OutputPixelType>::ZeroValue();

    this->Reset();
  }

  void
  SetCurrentNodePair(const NodePairType & iNodePair)
  {
    this->SetCurrentNode(iNodePair.GetNode());
    this->SetCurrentValue(iNodePair.GetValue());
  }

  itkSetObjectMacro(Domain, OutputDomainType);
  itkGetModifiableObjectMacro(Domain, OutputDomainType);

protected:
  /** Constructor */
  FastMarchingStoppingCriterionBase()
    : Superclass()
    , m_Domain(nullptr)
  {
    m_CurrentValue = NumericTraits<OutputPixelType>::ZeroValue();
    m_PreviousValue = NumericTraits<OutputPixelType>::ZeroValue();
  }

  /** Destructor */
  ~FastMarchingStoppingCriterionBase() override = default;

  OutputDomainPointer m_Domain;

  OutputPixelType m_PreviousValue;
  OutputPixelType m_CurrentValue;

  /** Inherited classes must implement this method and make sure member variables
  got reinitialized. */
  virtual void
  Reset() = 0;

  /** Set the Current Node */
  virtual void
  SetCurrentNode(const NodeType & iNode) = 0;

  /** Set the Current Value */
  virtual void
  SetCurrentValue(const OutputPixelType & iValue)
  {
    m_PreviousValue = m_CurrentValue;
    m_CurrentValue = iValue;
  }
};
} // namespace itk
#endif
