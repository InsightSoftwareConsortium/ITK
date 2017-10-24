/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
template< typename TInput, typename TOutput, typename TCriterion >
class ITK_TEMPLATE_EXPORT EdgeDecimationQuadEdgeMeshFilter:
  public DecimationQuadEdgeMeshFilter< TInput, TOutput, TCriterion >
{
public:
  typedef EdgeDecimationQuadEdgeMeshFilter Self;
  typedef SmartPointer< Self >             Pointer;
  typedef SmartPointer< const Self >       ConstPointer;
  typedef DecimationQuadEdgeMeshFilter<
    TInput, TOutput, TCriterion >          Superclass;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(EdgeDecimationQuadEdgeMeshFilter, DecimationQuadEdgeMeshFilter);

  typedef TInput                          InputMeshType;
  typedef typename InputMeshType::Pointer InputMeshPointer;

  typedef TOutput                                         OutputMeshType;
  typedef typename OutputMeshType::Pointer                OutputMeshPointer;
  typedef typename OutputMeshType::PointIdentifier        OutputPointIdentifier;
  typedef typename OutputMeshType::PointType              OutputPointType;
  typedef typename OutputPointType::VectorType            OutputVectorType;
  typedef typename OutputMeshType::QEType                 OutputQEType;
  typedef typename OutputMeshType::EdgeCellType           OutputEdgeCellType;
  typedef typename OutputMeshType::CellType               OutputCellType;
  typedef typename OutputMeshType::CellIdentifier         OutputCellIdentifier;
  typedef typename OutputMeshType::CellsContainerPointer  OutputCellsContainerPointer;
  typedef typename OutputMeshType::CellsContainerIterator OutputCellsContainerIterator;

  typedef QuadEdgeMeshPolygonCell< OutputCellType > OutputPolygonType;

  typedef TCriterion                                       CriterionType;
  typedef typename CriterionType::Pointer                  CriterionPointer;
  typedef typename CriterionType::MeasureType              MeasureType;
  typedef typename CriterionType::PriorityType             PriorityType;
  typedef typename CriterionType::PriorityQueueWrapperType PriorityQueueItemType;

  typedef PriorityQueueContainer< PriorityQueueItemType *,
                                  ElementWrapperPointerInterface< PriorityQueueItemType * >,
                                  PriorityType >                                          PriorityQueueType;
  typedef typename PriorityQueueType::Pointer PriorityQueuePointer;

  typedef std::map< OutputQEType *, PriorityQueueItemType * > QueueMapType;
  typedef typename QueueMapType::const_iterator               QueueMapConstIterator;
  typedef typename QueueMapType::iterator                     QueueMapIterator;

  typedef QuadEdgeMeshEulerOperatorJoinVertexFunction< OutputMeshType, OutputQEType > OperatorType;
  typedef typename OperatorType::Pointer                                              OperatorPointer;

protected:

  EdgeDecimationQuadEdgeMeshFilter();
  virtual ~EdgeDecimationQuadEdgeMeshFilter() ITK_OVERRIDE;

  bool m_Relocate;
  bool m_CheckOrientation;

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
  virtual MeasureType MeasureEdge(OutputQEType *iEdge) = 0;

  /**
  * \brief Fill the priority queue
  */
  void FillPriorityQueue() ITK_OVERRIDE;

  /**
  * \brief Push one edge in the priority queue
  * \param[in] iEdge
  */
  void PushElement(OutputQEType *iEdge);

  /**
  * \brief Check if iEdge is valid and then can be processed
  * \param[in] iEdge
  * \return
  */
  bool IsEdgeOKToBeProcessed(OutputQEType *iEdge);

  /**
  * \brief Extract the edge to be processed
  */
  void Extract() ITK_OVERRIDE;

  /**
  * \brief Delete a given edge in the priority queue
  * \param[in] iEdge
  */
  void DeleteElement(OutputQEType *iEdge);

  virtual void DeletePoint(const OutputPointIdentifier & iIdToBeDeleted,
                           const OutputPointIdentifier & iRemaing);

  /**
  * \brief Push iEdge in the priority queue if it is not already, else
  * its corresponding priority value is updated.
  * \param[in] iEdge
  */
  virtual void PushOrUpdateElement(OutputQEType *iEdge);

  /**
  * \brief
  */
  virtual void JoinVertexFailed();

  /**
  * \brief
  */
  virtual bool ProcessWithoutAnyTopologicalGuarantee() ITK_OVERRIDE;

  /**
  * \brief
  * \return
  */
  virtual unsigned int CheckQEProcessingStatus();

  /**
  * \brief
  * \return
  */
  virtual bool ProcessWithTopologicalGuarantee() ITK_OVERRIDE;

  /**
  * \brief
  */
  SizeValueType NumberOfCommonVerticesIn0Ring() const;

  /**
  * \brief
  */
  void RemoveSamosa();

  /**
  * \brief
  * \param[in] iEdge
  */
  void TagElementOut(OutputQEType *iEdge);

  /**
  * \brief
  */
  void RemoveEye();

  /**
  * \brief
  * \param[in] iEdge (the one which will be merged)
  * \return the new location of merged points
  */
  virtual OutputPointType Relocate(OutputQEType *iEdge) = 0;

  /**
  * \brief
  * \todo Finish to implement this method!
  */
  bool CheckOrientation(OutputQEType *iEdge,
                        const OutputPointIdentifier & iId,
                        const OutputPointType & iPt)
  {
    OutputMeshPointer           output = this->GetOutput();
    OutputCellsContainerPointer cells = output->GetCells();

    std::list< OutputCellIdentifier > r1, r2, elements_to_be_tested;
    OutputQEType *                    qe = iEdge;
    OutputQEType *                    qe_it = qe->GetOnext();

    do
      {
      r1.push_back( qe_it->GetLeft() );
      qe_it = qe_it->GetOnext();
      }
    while ( qe_it != qe );

    qe = iEdge->GetSym();
    qe_it = qe->GetOnext();

    do
      {
      r2.push_back( qe_it->GetLeft() );
      qe_it = qe_it->GetOnext();
      }
    while ( qe_it != qe );

    r1.sort();
    r2.sort();

    std::set_symmetric_difference( r1.begin(), r1.end(),
                                   r2.begin(), r2.end(),
                                   std::back_inserter(elements_to_be_tested) );

    typename std::list< OutputCellIdentifier >::iterator
    it = elements_to_be_tested.begin();

    typedef TriangleHelper< OutputPointType > TriangleType;

    bool                  orientation_ok(true);
    OutputCellIdentifier  c_id(0);
    OutputPolygonType *   poly;
    OutputPointIdentifier p_id;

    int             k(0), replace_k(0);
    OutputPointType pt[3];

    OutputVectorType n_bef, n_aft;

    while ( ( it != elements_to_be_tested.end() ) && orientation_ok )
      {
      c_id = *it;
      poly = dynamic_cast< OutputPolygonType * >( cells->GetElement(c_id) );

      qe = poly->GetEdgeRingEntry();
      qe_it = qe;
      k = 0;

      do
        {
        p_id = qe_it->GetOrigin();
        if ( p_id == iId )
          {
          replace_k = k;
          }
        pt[k++] = output->GetPoint(p_id);
        qe_it = qe_it->GetLnext();
        }
      while ( qe_it != qe );

      n_bef = TriangleType::ComputeNormal(pt[0], pt[1], pt[2]);
      switch ( replace_k )
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

      orientation_ok = ( n_bef * n_aft ) < 0.;
      ++it;
      }

    return orientation_ok;
  }

  /**
   * \brief
   * \return
   */
  bool IsCriterionSatisfied() ITK_OVERRIDE;

private:
  EdgeDecimationQuadEdgeMeshFilter(const Self &);
  void operator=(const Self &);

};
}

#include "itkEdgeDecimationQuadEdgeMeshFilter.hxx"
#endif
