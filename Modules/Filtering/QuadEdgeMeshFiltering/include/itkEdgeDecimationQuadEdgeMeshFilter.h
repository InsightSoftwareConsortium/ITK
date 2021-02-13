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
#ifndef itkEdgeDecimationQuadEdgeMeshFilter_h
#define itkEdgeDecimationQuadEdgeMeshFilter_h

#include <list>
#include <map>
#include <algorithm>

#include "itkQuadEdgeMeshEulerOperatorJoinVertexFunction.h"
#include "itkQuadEdgeMeshPolygonCell.h"

#include "itkDecimationQuadEdgeMeshFilter.h"
#include "itkPriorityQueueContainer.h"
#include "itkTriangleHelper.h"

namespace itk
{
/**
 * \class EdgeDecimationQuadEdgeMeshFilter
 * \brief
 * \ingroup ITKQuadEdgeMeshFiltering
 */
template <typename TInput, typename TOutput, typename TCriterion>
class ITK_TEMPLATE_EXPORT EdgeDecimationQuadEdgeMeshFilter
  : public DecimationQuadEdgeMeshFilter<TInput, TOutput, TCriterion>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(EdgeDecimationQuadEdgeMeshFilter);

  using Self = EdgeDecimationQuadEdgeMeshFilter;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using Superclass = DecimationQuadEdgeMeshFilter<TInput, TOutput, TCriterion>;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(EdgeDecimationQuadEdgeMeshFilter, DecimationQuadEdgeMeshFilter);

  using InputMeshType = TInput;
  using InputMeshPointer = typename InputMeshType::Pointer;

  using OutputMeshType = TOutput;
  using OutputMeshPointer = typename OutputMeshType::Pointer;
  using OutputPointIdentifier = typename OutputMeshType::PointIdentifier;
  using OutputPointType = typename OutputMeshType::PointType;
  using OutputVectorType = typename OutputPointType::VectorType;
  using OutputQEType = typename OutputMeshType::QEType;
  using OutputEdgeCellType = typename OutputMeshType::EdgeCellType;
  using OutputCellType = typename OutputMeshType::CellType;
  using OutputCellIdentifier = typename OutputMeshType::CellIdentifier;
  using OutputCellsContainerPointer = typename OutputMeshType::CellsContainerPointer;
  using OutputCellsContainerIterator = typename OutputMeshType::CellsContainerIterator;

  using OutputPolygonType = QuadEdgeMeshPolygonCell<OutputCellType>;

  using CriterionType = TCriterion;
  using CriterionPointer = typename CriterionType::Pointer;
  using MeasureType = typename CriterionType::MeasureType;
  using PriorityType = typename CriterionType::PriorityType;
  using PriorityQueueItemType = typename CriterionType::PriorityQueueWrapperType;

  using PriorityQueueType = PriorityQueueContainer<PriorityQueueItemType *,
                                                   ElementWrapperPointerInterface<PriorityQueueItemType *>,
                                                   PriorityType>;
  using PriorityQueuePointer = typename PriorityQueueType::Pointer;

  using QueueMapType = std::map<OutputQEType *, PriorityQueueItemType *>;
  using QueueMapConstIterator = typename QueueMapType::const_iterator;
  using QueueMapIterator = typename QueueMapType::iterator;

  using OperatorType = QuadEdgeMeshEulerOperatorJoinVertexFunction<OutputMeshType, OutputQEType>;
  using OperatorPointer = typename OperatorType::Pointer;

protected:
  EdgeDecimationQuadEdgeMeshFilter();
  ~EdgeDecimationQuadEdgeMeshFilter() override;

  bool m_Relocate{ true };
  bool m_CheckOrientation{ false };

  PriorityQueuePointer m_PriorityQueue;
  QueueMapType         m_QueueMapper;
  OutputQEType *       m_Element;
  PriorityType         m_Priority;
  OperatorPointer      m_JoinVertexFunction;

  /**
   * \brief Compute the measure value for iEdge
   * \param[in] iEdge
   * \return measure value
   */
  virtual MeasureType
  MeasureEdge(OutputQEType * iEdge) = 0;

  /**
   * \brief Fill the priority queue
   */
  void
  FillPriorityQueue() override;

  /**
   * \brief Push one edge in the priority queue
   * \param[in] iEdge
   */
  void
  PushElement(OutputQEType * iEdge);

  /**
   * \brief Check if iEdge is valid and then can be processed
   * \param[in] iEdge
   * \return
   */
  bool
  IsEdgeOKToBeProcessed(OutputQEType * iEdge);

  /**
   * \brief Extract the edge to be processed
   */
  void
  Extract() override;

  /**
   * \brief Delete a given edge in the priority queue
   * \param[in] iEdge
   */
  void
  DeleteElement(OutputQEType * iEdge);

  virtual void
  DeletePoint(const OutputPointIdentifier & iIdToBeDeleted, const OutputPointIdentifier & iRemaining);

  /**
   * \brief Push iEdge in the priority queue if it is not already, else
   * its corresponding priority value is updated.
   * \param[in] iEdge
   */
  virtual void
  PushOrUpdateElement(OutputQEType * iEdge);

  /**
   * \brief
   */
  virtual void
  JoinVertexFailed();

  /**
   * \brief
   */
  bool
  ProcessWithoutAnyTopologicalGuarantee() override;

  /**
   * \brief
   * \return
   */
  virtual unsigned int
  CheckQEProcessingStatus();

  /**
   * \brief
   * \return
   */
  bool
  ProcessWithTopologicalGuarantee() override;

  /**
   * \brief
   */
  SizeValueType
  NumberOfCommonVerticesIn0Ring() const;

  /**
   * \brief
   */
  void
  RemoveSamosa();

  /**
   * \brief
   * \param[in] iEdge
   */
  void
  TagElementOut(OutputQEType * iEdge);

  /**
   * \brief
   */
  void
  RemoveEye();

  /**
   * \brief
   * \param[in] iEdge (the one which will be merged)
   * \return the new location of merged points
   */
  virtual OutputPointType
  Relocate(OutputQEType * iEdge) = 0;

  /**
   * \brief
   * \todo Finish to implement this method!
   */
  bool
  CheckOrientation(OutputQEType * iEdge, const OutputPointIdentifier & iId, const OutputPointType & iPt)
  {
    OutputMeshPointer           output = this->GetOutput();
    OutputCellsContainerPointer cells = output->GetCells();

    std::list<OutputCellIdentifier> r1, r2, elements_to_be_tested;
    OutputQEType *                  qe = iEdge;
    OutputQEType *                  qe_it = qe->GetOnext();

    do
    {
      r1.push_back(qe_it->GetLeft());
      qe_it = qe_it->GetOnext();
    } while (qe_it != qe);

    qe = iEdge->GetSym();
    qe_it = qe->GetOnext();

    do
    {
      r2.push_back(qe_it->GetLeft());
      qe_it = qe_it->GetOnext();
    } while (qe_it != qe);

    r1.sort();
    r2.sort();

    std::set_symmetric_difference(
      r1.begin(), r1.end(), r2.begin(), r2.end(), std::back_inserter(elements_to_be_tested));

    typename std::list<OutputCellIdentifier>::iterator it = elements_to_be_tested.begin();

    using TriangleType = TriangleHelper<OutputPointType>;

    bool                  orientation_ok(true);
    OutputCellIdentifier  c_id(0);
    OutputPolygonType *   poly;
    OutputPointIdentifier p_id;

    int             k(0), replace_k(0);
    OutputPointType pt[3];

    OutputVectorType n_bef, n_aft;

    while ((it != elements_to_be_tested.end()) && orientation_ok)
    {
      c_id = *it;
      poly = dynamic_cast<OutputPolygonType *>(cells->GetElement(c_id));

      qe = poly->GetEdgeRingEntry();
      qe_it = qe;
      k = 0;

      do
      {
        p_id = qe_it->GetOrigin();
        if (p_id == iId)
        {
          replace_k = k;
        }
        pt[k++] = output->GetPoint(p_id);
        qe_it = qe_it->GetLnext();
      } while (qe_it != qe);

      n_bef = TriangleType::ComputeNormal(pt[0], pt[1], pt[2]);
      switch (replace_k)
      {
        default:
        case 0:
          n_aft = TriangleType::ComputeNormal(iPt, pt[1], pt[2]);
          break;
        case 1:
          n_aft = TriangleType::ComputeNormal(pt[0], iPt, pt[2]);
          break;
        case 2:
          n_aft = TriangleType::ComputeNormal(pt[0], pt[1], iPt);
          break;
      }

      orientation_ok = (n_bef * n_aft) < 0.;
      ++it;
    }

    return orientation_ok;
  }

  /**
   * \brief
   * \return
   */
  bool
  IsCriterionSatisfied() override;
};
} // namespace itk

#include "itkEdgeDecimationQuadEdgeMeshFilter.hxx"
#endif
