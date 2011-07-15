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
#ifndef __itkQuadricDecimationQuadEdgeMeshFilter_h
#define __itkQuadricDecimationQuadEdgeMeshFilter_h

#include "itkEdgeDecimationQuadEdgeMeshFilter.h"
#include "itkQuadEdgeMeshDecimationQuadricElementHelper.h"

namespace itk
{
/**
 * \class QuadricDecimationQuadEdgeMeshFilter
 * \brief
 * \ingroup ITKQuadEdgeMeshFiltering
 */
template< class TInput, class TOutput, class TCriterion >
class ITK_EXPORT QuadricDecimationQuadEdgeMeshFilter:
  public EdgeDecimationQuadEdgeMeshFilter< TInput, TOutput, TCriterion >
{
public:
  typedef QuadricDecimationQuadEdgeMeshFilter Self;
  typedef SmartPointer< Self >                Pointer;
  typedef SmartPointer< const Self >          ConstPointer;
  typedef EdgeDecimationQuadEdgeMeshFilter<
    TInput, TOutput, TCriterion >             Superclass;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(QuadricDecimationQuadEdgeMeshFilter, EdgeDecimationQuadEdgeMeshFilter);

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(Self);

  typedef TInput                          InputMeshType;
  typedef typename InputMeshType::Pointer InputMeshPointer;

  typedef TOutput                                          OutputMeshType;
  typedef typename OutputMeshType::Pointer                 OutputMeshPointer;
  typedef typename OutputMeshType::PointIdentifier         OutputPointIdentifier;
  typedef typename OutputMeshType::PointType               OutputPointType;
  typedef typename OutputPointType::CoordRepType           OutputCoordType;
  typedef typename OutputMeshType::QEType                  OutputQEType;
  typedef typename OutputMeshType::EdgeCellType            OutputEdgeCellType;
  typedef typename OutputMeshType::CellsContainerIterator  OutputCellsContainerIterator;
  typedef typename OutputMeshType::PointsContainerPointer  OutputPointsContainerPointer;
  typedef typename OutputMeshType::PointsContainerIterator OutputPointsContainerIterator;

  itkStaticConstMacro(OutputPointDimension, unsigned int, OutputMeshType::PointDimension);

  typedef TCriterion                          CriterionType;
  typedef typename CriterionType::MeasureType MeasureType;

  typedef typename Superclass::PriorityType          PriorityType;
  typedef typename Superclass::PriorityQueueItemType PriorityQueueItemType;
  typedef typename Superclass::PriorityQueueType     PriorityQueueType;
  typedef typename Superclass::PriorityQueuePointer  PriorityQueuePointer;

  typedef typename Superclass::QueueMapType     QueueMapType;
  typedef typename Superclass::QueueMapIterator QueueMapIterator;

  typedef typename Superclass::OperatorType    OperatorType;
  typedef typename Superclass::OperatorPointer OperatorPointer;

  typedef QuadEdgeMeshDecimationQuadricElementHelper< OutputPointType >
  QuadricElementType;

  typedef std::map< OutputPointIdentifier, QuadricElementType >
  QuadricElementMapType;

  typedef typename QuadricElementMapType::iterator QuadricElementMapIterator;
protected:

  QuadricDecimationQuadEdgeMeshFilter() {}
  virtual ~QuadricDecimationQuadEdgeMeshFilter() {}

  QuadricElementMapType m_Quadric;

  virtual void Initialize()
  {
    OutputMeshPointer             output = this->GetOutput();
    OutputPointsContainerPointer  points = output->GetPoints();
    OutputPointsContainerIterator it = points->Begin();
    OutputPointIdentifier         p_id;
    OutputQEType *                qe;
    OutputQEType *                qe_it;

    while ( it != points->End() )
      {
      p_id = it->Index();

      qe = output->FindEdge(p_id);
      if ( qe != 0 )
        {
        qe_it = qe;
        do
          {
          QuadricAtOrigin(qe_it, m_Quadric[p_id]);
          qe_it = qe_it->GetOnext();
          }
        while ( qe_it != qe );
        }
      it++;
      }
  }

  inline void QuadricAtOrigin(OutputQEType *iEdge, QuadricElementType & oQ)
  {
    OutputMeshPointer output = this->GetOutput();

    OutputPointIdentifier id[3];

    id[0] = iEdge->GetOrigin();
    id[1] = iEdge->GetDestination();
    id[2] = iEdge->GetOnext()->GetDestination();

    OutputPointType p[3];

    for ( int i = 0; i < 3; i++ )
      {
      p[i] = output->GetPoint(id[i]);
      }

    oQ.AddTriangle(p[0], p[1], p[2]);
  }

  /**
   * \brief Compute the measure value for iEdge
   * \param[in] iEdge
   * \return measure value, here the squared edge length
   */
  inline MeasureType MeasureEdge(OutputQEType *iEdge)
  {
    OutputPointIdentifier id_org = iEdge->GetOrigin();
    OutputPointIdentifier id_dest = iEdge->GetDestination();
    QuadricElementType    Q = m_Quadric[id_org] + m_Quadric[id_dest];

    OutputMeshPointer output = this->GetOutput();

    OutputPointType org = output->GetPoint(id_org);
    OutputPointType dest = output->GetPoint(id_dest);

    OutputPointType mid;

    mid.SetToMidPoint(org, dest);
    OutputPointType p = Q.ComputeOptimalLocation(mid);

    return static_cast< MeasureType >( Q.ComputeError(p) );
  }

  /** \brief
   * \param[in] iIdToBeDeleted
   * \param[in] iRemaining
   */
  virtual void DeletePoint(const OutputPointIdentifier & iIdToBeDeleted,
                           const OutputPointIdentifier & iRemaining)
  {
    Superclass::DeletePoint(iIdToBeDeleted, iRemaining);

    QuadricElementMapIterator it = m_Quadric.find(iIdToBeDeleted);
    m_Quadric[iRemaining] += it->second;
    m_Quadric.erase(it);
  }

  /**
  * \param[in] iEdge
  * \return the optimal point location
  */
  OutputPointType Relocate(OutputQEType *iEdge)
  {
    OutputPointIdentifier id_org = iEdge->GetOrigin();
    OutputPointIdentifier id_dest = iEdge->GetDestination();
    QuadricElementType    Q = m_Quadric[id_org] + m_Quadric[id_dest];

    OutputMeshPointer output = this->GetOutput();

    OutputPointType org = output->GetPoint(id_org);
    OutputPointType dest = output->GetPoint(id_dest);

    OutputPointType mid;

    mid.SetToMidPoint(org, dest);

    return Q.ComputeOptimalLocation(mid);
  }

private:
  QuadricDecimationQuadEdgeMeshFilter(const Self &);
  void operator=(const Self &);
};
}
#endif
