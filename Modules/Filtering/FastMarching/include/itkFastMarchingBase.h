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

#ifndef itkFastMarchingBase_h
#define itkFastMarchingBase_h

#include "itkIntTypes.h"
#include "itkFastMarchingStoppingCriterionBase.h"
#include "itkFastMarchingTraits.h"
#include "ITKFastMarchingExport.h"

#include <queue>
#include <functional>

namespace itk
{
/**
 *\class FastMarchingTraitsEnums
 * \ingroup ITKFastMarching
 * */
class FastMarchingTraitsEnums
{
public:
  /**
   *\class TopologyCheck
   * \ingroup ITKFastMarching
   * */
  enum class TopologyCheck : uint8_t
  {
    Nothing = 0,
    NoHandles,
    Strict
  };
};
// Define how to print enumeration
extern ITKFastMarching_EXPORT std::ostream &
                              operator<<(std::ostream & out, const FastMarchingTraitsEnums::TopologyCheck value);

/**
 * \class FastMarchingBase
 * \brief Abstract class to solve an Eikonal based-equation using Fast Marching
 * Method.
 *
 * Fast marching solves an Eikonal equation where the speed is always
 * non-negative and depends on the position only. Starting from an
 * initial position on the front, fast marching systematically moves the
 * front forward one node at a time.
 *
 * Updates are performed using an entropy satisfy scheme where only
 * "upwind" neighborhoods are used. This implementation of Fast Marching
 * uses a std::priority_queue to locate the next proper node to
 * update.
 *
 * Fast Marching sweeps through N points in (N log N) steps to obtain
 * the arrival time value as the front propagates through the domain.
 *
 * The initial front is specified by two containers:
 * \li one containing the known nodes (Alive Nodes: nodes that are already
 * part of the object),
 * \li one containing the trial nodes (Trial Nodes: nodes that are
 * considered for inclusion).
 *
 * In order for the filter to evolve, at least some trial nodes must be
 * specified. These can for instance be specified as the layer of
 * nodes around the alive ones.
 *
 * The algorithm is terminated early by setting an appropriate stopping
 * criterion, or if there are no more nodes to process.
 *
 * \tparam TTraits traits which includes definition such as:
 *    \li InputDomainType (itk::Image or itk::QuadEdgeMesh)
 *    \li OutputDomainType (similar to InputDomainType)
 *    \li NodeType (itk::Index if itk::Image and PointIdentifier if
 * itk::QuadEdgeMesh)
 *    \li NodePairType std::pair< NodeType, OutputPixelType >
 *    \li Superclass (itk::ImageToImageFilter or
 * itk::QuadEdgeMeshToQuadEdgeMeshFilter )
 *
 * \todo In the current implementation, std::priority_queue only allows
 * taking nodes out from the front and putting nodes in from the back.
 * Use itk::PriorityQueueContainer instead.
 *
 * \par Topology constraints:
 * Additional flexibility in this class includes the implementation of
 * topology constraints for image-based fast marching.  Further details
 * can be found in the paper
 *
 * NJ Tustison, BA Avants, MF Siqueira, JC Gee. "Topological Well-
 * Composedness and Glamorous Glue: A Digital Gluing Algorithm for
 * Topologically Constrained Front Propagation, IEEE Transactions on
 * Image Processing, 20(6):1756-1761, June 2011.
 *
 * Essentially, one can constrain the propagating front(s) such that
 * they either:
 *  1. don't merge (using the "Strict" option)
 *  2. don't create handles (using the "NoHandles" option)
 *
 * Whereas the majority of related work uses the digital topological
 * concept of "simple points" to constrain the evolving front, this
 * filter uses the concept of "well-composedness".  Advantages of
 * the latter over the former includes being able to use the standard
 * marching cubes algorithm to produce a mesh whose genus is identical
 * to that of the evolved front(s).
 *
 * \sa FastMarchingStoppingCriterionBase
 *
 * \ingroup ITKFastMarching
 */
template <typename TInput, typename TOutput>
class ITK_TEMPLATE_EXPORT FastMarchingBase : public FastMarchingTraits<TInput, TOutput>::SuperclassType
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(FastMarchingBase);

  using Traits = FastMarchingTraits<TInput, TOutput>;
  using SuperclassType = typename Traits::SuperclassType;

  using Self = FastMarchingBase;
  using Superclass = typename FastMarchingTraits<TInput, TOutput>::SuperclassType;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(FastMarchingBase, FastMarchingTraits);

  /** Input Domain related definitions */
  using InputDomainType = typename Traits::InputDomainType;
  using InputDomainPointer = typename Traits::InputDomainPointer;
  using InputPixelType = typename Traits::InputPixelType;

  /** Output Domain related definitions */
  using OutputDomainType = typename Traits::OutputDomainType;
  using OutputDomainPointer = typename Traits::OutputDomainPointer;
  using OutputPixelType = typename Traits::OutputPixelType;

  /** NodeType type of node */
  using NodeType = typename Traits::NodeType;

  /** NodePairType pair of node and corresponding value */
  using NodePairType = typename Traits::NodePairType;
  using NodePairContainerType = typename Traits::NodePairContainerType;
  using NodePairContainerPointer = typename Traits::NodePairContainerPointer;
  using NodePairContainerConstIterator = typename Traits::NodePairContainerConstIterator;

  using LabelType = typename Traits::LabelType;

  /** StoppingCriterionType stopping criterion */
  using StoppingCriterionType = FastMarchingStoppingCriterionBase<TInput, TOutput>;
  using StoppingCriterionPointer = typename StoppingCriterionType::Pointer;

  /*
  using ElementIdentifier = long;

  using PriorityQueueElementType = MinPriorityQueueElementWrapper< NodeType,
    OutputPixelType,
    ElementIdentifier >;

  using PriorityQueueType = PriorityQueueContainer< PriorityQueueElementType,
    PriorityQueueElementType, OutputPixelType, ElementIdentifier >;
  using PriorityQueuePointer = typename PriorityQueueType::Pointer;
  */

  using TopologyCheckEnum = FastMarchingTraitsEnums::TopologyCheck;
#if !defined(ITK_LEGACY_REMOVE)
  using TopologyCheckType = FastMarchingTraitsEnums::TopologyCheck;
  /**Exposes enums values for backwards compatibility*/
  static constexpr TopologyCheckEnum Nothing = TopologyCheckEnum::Nothing;
  static constexpr TopologyCheckEnum NoHandles = TopologyCheckEnum::NoHandles;
  static constexpr TopologyCheckEnum Strict = TopologyCheckEnum::Strict;
#endif

  /** Set/Get the TopologyCheckType macro indicating whether the user
  wants to check topology (and which one). */
  itkSetEnumMacro(TopologyCheck, TopologyCheckEnum);
  itkGetConstReferenceMacro(TopologyCheck, TopologyCheckEnum);

  /** Set/Get TrialPoints */
  itkSetObjectMacro(TrialPoints, NodePairContainerType);
  itkGetModifiableObjectMacro(TrialPoints, NodePairContainerType);

  /** Set/Get AlivePoints */
  itkSetObjectMacro(AlivePoints, NodePairContainerType);
  itkGetModifiableObjectMacro(AlivePoints, NodePairContainerType);

  /** Set/Get ProcessedPoints */
  itkSetObjectMacro(ProcessedPoints, NodePairContainerType);
  itkGetModifiableObjectMacro(ProcessedPoints, NodePairContainerType);

  /** Set/Get ForbiddenPoints */
  itkSetObjectMacro(ForbiddenPoints, NodePairContainerType);
  itkGetModifiableObjectMacro(ForbiddenPoints, NodePairContainerType);

  /** \brief Set/Get the Stopping Criterion */
  itkSetObjectMacro(StoppingCriterion, StoppingCriterionType);
  itkGetModifiableObjectMacro(StoppingCriterion, StoppingCriterionType);

  /** \brief Set/Get SpeedConstant */
  itkGetMacro(SpeedConstant, double);
  itkSetMacro(SpeedConstant, double);

  /** \brief Set/Get NormalizationFactor */
  itkGetMacro(NormalizationFactor, double);
  itkSetMacro(NormalizationFactor, double);

  /** \brief Get the value reached by the front when it stops propagating */
  itkGetMacro(TargetReachedValue, OutputPixelType);

  /** Set the Collect Points flag. Instrument the algorithm to collect
   * a container of all nodes which it has visited. Useful for
   * creating Narrowbands for level set algorithms that supports
   * narrow banding. */
  itkSetMacro(CollectPoints, bool);

  /** Get the Collect Points flag. */
  itkGetConstReferenceMacro(CollectPoints, bool);
  itkBooleanMacro(CollectPoints);

protected:
  /** \brief Constructor */
  FastMarchingBase();

  /** \brief Destructor */
  ~FastMarchingBase() override = default;

  StoppingCriterionPointer m_StoppingCriterion;

  double m_SpeedConstant;
  double m_InverseSpeed;
  double m_NormalizationFactor;

  OutputPixelType m_TargetReachedValue;
  OutputPixelType m_LargeValue;
  OutputPixelType m_TopologyValue;

  NodePairContainerPointer m_TrialPoints;
  NodePairContainerPointer m_AlivePoints;
  NodePairContainerPointer m_ProcessedPoints;
  NodePairContainerPointer m_ForbiddenPoints;

  bool m_CollectPoints;

  // PriorityQueuePointer m_Heap;
  using HeapContainerType = std::vector<NodePairType>;
  using NodeComparerType = std::greater<NodePairType>;

  using PriorityQueueType = std::priority_queue<NodePairType, HeapContainerType, NodeComparerType>;

  PriorityQueueType m_Heap;

  TopologyCheckEnum m_TopologyCheck;

  /** \brief Get the total number of nodes in the domain */
  virtual IdentifierType
  GetTotalNumberOfNodes() const = 0;

  /** \brief Get the output value (front value) for a given node */
  virtual const OutputPixelType
  GetOutputValue(OutputDomainType * oDomain, const NodeType & iNode) const = 0;

  /** \brief Set the output value (front value) for a given node */
  virtual void
  SetOutputValue(OutputDomainType * oDomain, const NodeType & iNode, const OutputPixelType & iValue) = 0;

  /** \brief Get the LabelEnum Value for a given node
    \param[in] iNode
    \return its label value  */
  virtual unsigned char
  GetLabelValueForGivenNode(const NodeType & iNode) const = 0;

  /** \brief Set the Label Value for a given node
    \param[in] iNode
    \param[in] iLabel */
  virtual void
  SetLabelValueForGivenNode(const NodeType & iNode, const LabelType & iLabel) = 0;

  /** \brief Update neighbors to a given node
    \param[in] oDomain
    \param[in] iNode
  */
  virtual void
  UpdateNeighbors(OutputDomainType * oDomain, const NodeType & iNode) = 0;

  /** \brief Update value for a given node
    \param[in] oDomain
    \param[in] iNode
    */
  virtual void
  UpdateValue(OutputDomainType * oDomain, const NodeType & iNode) = 0;

  /** \brief Check if the current node violate topological criterion.
    \param[in] oDomain
    \param[in] iNode
   */
  virtual bool
  CheckTopology(OutputDomainType * oDomain, const NodeType & iNode) = 0;

  /** \brief   */
  void
  Initialize(OutputDomainType * oDomain);

  /**    */
  virtual void
  InitializeOutput(OutputDomainType * oDomain) = 0;

  /**    */
  void
  GenerateData() override;

  /** \brief PrintSelf method  */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkFastMarchingBase.hxx"
#endif

#endif
