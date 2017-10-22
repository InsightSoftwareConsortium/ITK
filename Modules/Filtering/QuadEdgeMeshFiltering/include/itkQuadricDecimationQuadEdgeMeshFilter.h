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
#ifndef itkQuadricDecimationQuadEdgeMeshFilter_h
#define itkQuadricDecimationQuadEdgeMeshFilter_h

#include "itkEdgeDecimationQuadEdgeMeshFilter.h"
#include "itkQuadEdgeMeshDecimationQuadricElementHelper.h"

namespace itk
{
/**
 * \class QuadricDecimationQuadEdgeMeshFilter
 * \brief Quadric decimation
 * \ingroup ITKQuadEdgeMeshFiltering
 */
template< typename TInput, typename TOutput, typename TCriterion >
class ITK_TEMPLATE_EXPORT QuadricDecimationQuadEdgeMeshFilter:
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
  /** \brief Constructor */
  QuadricDecimationQuadEdgeMeshFilter();

  /** \brief Destructor */
  virtual ~QuadricDecimationQuadEdgeMeshFilter() ITK_OVERRIDE;

  /** \brief Compute the quadric error at the origin of the edge
   *  \param[in] iEdge input edge
   *  \param[in,out] oQ quadric element to be modified
   *  \param[in] outputMesh mesh to be processed
   */
  inline void QuadricAtOrigin(OutputQEType *iEdge, QuadricElementType & oQ, OutputMeshType *outputMesh)
  {
    OutputPointIdentifier id[3];

    id[0] = iEdge->GetOrigin();
    id[1] = iEdge->GetDestination();
    id[2] = iEdge->GetOnext()->GetDestination();

    OutputPointType p[3];

    for ( int i = 0; i < 3; i++ )
      {
      p[i] = outputMesh->GetPoint(id[i]);
      }

    oQ.AddTriangle(p[0], p[1], p[2]);
  }

  /** \brief Compute the measure value for iEdge
   * \param[in] iEdge input edge
   * \return measure value, here the corresponding quadric error
   */
  MeasureType MeasureEdge(OutputQEType *iEdge) ITK_OVERRIDE
  {
    OutputPointIdentifier id_org = iEdge->GetOrigin();
    OutputPointIdentifier id_dest = iEdge->GetDestination();
    QuadricElementType    Q = m_Quadric[id_org] + m_Quadric[id_dest];

    OutputPointType org = this->m_OutputMesh->GetPoint(id_org);
    OutputPointType dest = this->m_OutputMesh->GetPoint(id_dest);

    OutputPointType mid;

    mid.SetToMidPoint(org, dest);
    OutputPointType p = Q.ComputeOptimalLocation(mid);

    return static_cast< MeasureType >( Q.ComputeError(p) );
  }

  /** \brief Delete point
   * \param[in] iIdToBeDeleted id of the point to be deleted
   * \param[in] iRemaining  id of the point to be kept
   */
  virtual void DeletePoint(const OutputPointIdentifier & iIdToBeDeleted,
                           const OutputPointIdentifier & iRemaining) ITK_OVERRIDE;

  /** \brief Compute the optimal position for a given edge iEdge
  * \param[in] iEdge
  * \return the optimal point location
  */
  OutputPointType Relocate(OutputQEType *iEdge) ITK_OVERRIDE;

  /** \brief Compute Quadric error for all edges */
  virtual void Initialize() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(QuadricDecimationQuadEdgeMeshFilter);

  QuadricElementMapType m_Quadric;
};
}
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkQuadricDecimationQuadEdgeMeshFilter.hxx"
#endif
#endif
