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

#ifndef itkFastMarchingReachedTargetNodesStoppingCriterion_h
#define itkFastMarchingReachedTargetNodesStoppingCriterion_h

#include "itkFastMarchingStoppingCriterionBase.h"
#include "itkObjectFactory.h"
#include "ITKFastMarchingExport.h"

namespace itk
{
/**\class FastMarchingReachedTargetNodesStoppingCriterionEnums
 * \brief Contains all enum classes used by FastMarchingReachedTargetNodesStoppingCriterion class
 * \ingroup ITKFastMarching
 */
class FastMarchingReachedTargetNodesStoppingCriterionEnums
{
public:
  /**
   *\class TargetCondition
   * \ingroup ITKFastMarching
   * TargetConditionEnum */
  enum class TargetCondition : uint8_t
  {
    OneTarget = 1,
    SomeTargets,
    AllTargets
  };
};
// Define how to print enumeration
extern ITKFastMarching_EXPORT std::ostream &
                              operator<<(std::ostream & out, const FastMarchingReachedTargetNodesStoppingCriterionEnums::TargetCondition value);
/**
 * \class FastMarchingReachedTargetNodesStoppingCriterion
 * \brief Stopping criterion for FastMarchingFilterBase.
 *
 * Stopping criterion where the condition is satisfied when the front
 * reaches one, several or all target nodes (provided by the user).
 *
 * \ingroup ITKFastMarching
 */
template <typename TInput, typename TOutput>
class FastMarchingReachedTargetNodesStoppingCriterion : public FastMarchingStoppingCriterionBase<TInput, TOutput>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(FastMarchingReachedTargetNodesStoppingCriterion);

  using Self = FastMarchingReachedTargetNodesStoppingCriterion;
  using Superclass = FastMarchingStoppingCriterionBase<TInput, TOutput>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using Traits = typename Superclass::Traits;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FastMarchingReachedTargetNodesStoppingCriterion, FastMarchingStoppingCriterionBase);

  using OutputPixelType = typename Superclass::OutputPixelType;
  using NodeType = typename Superclass::NodeType;

  using TargetConditionEnum = FastMarchingReachedTargetNodesStoppingCriterionEnums::TargetCondition;
#if !defined(ITK_LEGACY_REMOVE)
  /**Exposes enums values for backwards compatibility*/
  static constexpr TargetConditionEnum OneTarget = TargetConditionEnum::OneTarget;
  static constexpr TargetConditionEnum SomeTargets = TargetConditionEnum::SomeTargets;
  static constexpr TargetConditionEnum AllTargets = TargetConditionEnum::AllTargets;
#endif

  /** Set/Get TargetCondition to indicate if the user wants the front to
  reach one, some or all target nodes. */
  void
  SetTargetCondition(const TargetConditionEnum & iCondition)
  {
    m_TargetCondition = iCondition;
    m_Initialized = false;
    this->Modified();
  }

  itkGetConstReferenceMacro(TargetCondition, TargetConditionEnum);

  /** Set/Get TargetOffset */
  itkSetMacro(TargetOffset, OutputPixelType);
  itkGetMacro(TargetOffset, OutputPixelType);

  /** \brief Set the number of target nodes to be reached */
  void
  SetNumberOfTargetsToBeReached(const size_t & iN)
  {
    m_NumberOfTargetsToBeReached = iN;
    m_Initialized = false;
    this->Modified();
  }

  /** \brief Set Target Nodes*/
  virtual void
  SetTargetNodes(const std::vector<NodeType> & iNodes)
  {
    m_TargetNodes = iNodes;
    m_Initialized = false;
    this->Modified();
  }

  /** \brief Set the current node */
  void
  SetCurrentNode(const NodeType & iNode) override
  {
    if (!m_Initialized)
    {
      Initialize();
    }

    if (!m_Satisfied)
    {
      // Only check for reached targets if the mode is not NoTargets and
      // there is at least one TargetPoint.
      if (!m_TargetNodes.empty())
      {
        typename std::vector<NodeType>::const_iterator pointsIter = m_TargetNodes.begin();
        typename std::vector<NodeType>::const_iterator pointsEnd = m_TargetNodes.end();

        while (pointsIter != pointsEnd)
        {
          if (*pointsIter == iNode)
          {
            this->m_ReachedTargetNodes.push_back(iNode);
            m_Satisfied = (m_ReachedTargetNodes.size() == m_NumberOfTargetsToBeReached);
            break;
          }
          ++pointsIter;
        }
        if (m_Satisfied)
        {
          m_StoppingValue = this->m_CurrentValue + m_TargetOffset;
        }
      }
      else
      {
        m_Satisfied = false;
      }
    }
  }

  /** \brief returns if the stopping condition is satisfied or not. */
  bool
  IsSatisfied() const override
  {
    return m_Satisfied && (this->m_CurrentValue >= m_StoppingValue);
  }

  /** \brief Get a short description of the stopping criterion. */
  std::string
  GetDescription() const override
  {
    return "Target Nodes Reached with possible overshoot";
  }

protected:
  /** Constructor */
  FastMarchingReachedTargetNodesStoppingCriterion()
    : Superclass()
    , m_TargetOffset(NumericTraits<OutputPixelType>::ZeroValue())
    , m_StoppingValue(NumericTraits<OutputPixelType>::ZeroValue())
  {}

  /** Destructor */
  ~FastMarchingReachedTargetNodesStoppingCriterion() override = default;

  TargetConditionEnum   m_TargetCondition{ TargetConditionEnum::AllTargets };
  std::vector<NodeType> m_TargetNodes;
  std::vector<NodeType> m_ReachedTargetNodes;
  size_t                m_NumberOfTargetsToBeReached{ 0 };
  OutputPixelType       m_TargetOffset;
  OutputPixelType       m_StoppingValue;
  bool                  m_Satisfied{ false };
  bool                  m_Initialized{ false };

  void
  Reset() override
  {
    this->Initialize();
  }

  void
  Initialize()
  {
    if (m_TargetCondition == TargetConditionEnum::OneTarget)
    {
      m_NumberOfTargetsToBeReached = 1;
    }
    if (m_TargetCondition == TargetConditionEnum::AllTargets)
    {
      m_NumberOfTargetsToBeReached = m_TargetNodes.size();
    }
    if (m_NumberOfTargetsToBeReached < 1)
    {
      itkExceptionMacro(<< "Number of target nodes to be reached is null");
    }
    if (m_NumberOfTargetsToBeReached > m_TargetNodes.size())
    {
      itkExceptionMacro(
        << "Number of target nodes to be reached is above the provided number of           target nodes");
    }
    m_ReachedTargetNodes.clear();

    m_Satisfied = false;
    m_Initialized = true;
  }
};
} // namespace itk
#endif // itkFastMarchingThresholdStoppingCriterion_h
