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
#ifndef itkDelaunayConformingQuadEdgeMeshFilter_h
#define itkDelaunayConformingQuadEdgeMeshFilter_h

#include "itkIntTypes.h"
#include "itkPriorityQueueContainer.h"
#include "itkQuadEdgeMeshToQuadEdgeMeshFilter.h"
#include "itkQuadEdgeMeshEulerOperatorFlipEdgeFunction.h"
#include "itkMath.h"

namespace itk
{
/**
 *  \class DelaunayConformingQuadEdgeMeshFilter
 *
 *  \brief FIXME Add documentation
 *
 * \ingroup ITKQuadEdgeMeshFiltering
 */
template <typename TInputMesh, typename TOutputMesh = TInputMesh>
class ITK_TEMPLATE_EXPORT DelaunayConformingQuadEdgeMeshFilter
  : public QuadEdgeMeshToQuadEdgeMeshFilter<TInputMesh, TOutputMesh>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(DelaunayConformingQuadEdgeMeshFilter);

  /** Basic types. */
  using Self = DelaunayConformingQuadEdgeMeshFilter;
  using Superclass = QuadEdgeMeshToQuadEdgeMeshFilter<TInputMesh, TOutputMesh>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Input types. */
  using InputMeshType = TInputMesh;
  using InputMeshPointer = typename InputMeshType::Pointer;
  using InputCoordRepType = typename InputMeshType::CoordRepType;
  using InputPointType = typename InputMeshType::PointType;
  using InputPointVectorType = typename InputPointType::VectorType;
  using InputPointIdentifier = typename InputMeshType::PointIdentifier;
  using InputQEType = typename InputMeshType::QEType;
  using InputVectorType = typename InputMeshType::VectorType;
  using InputEdgeListType = typename InputMeshType::EdgeListType;
  using InputPixelType = typename InputMeshType::PixelType;
  using InputTraits = typename InputMeshType::Traits;

  static constexpr unsigned int InputVDimension = InputMeshType::PointDimension;

  using InputPointsContainer = typename InputMeshType::PointsContainer;
  using InputPointsContainerConstIterator = typename InputMeshType::PointsContainerConstIterator;
  using InputCellsContainerConstIterator = typename InputMeshType::CellsContainerConstIterator;
  using InputEdgeCellType = typename InputMeshType::EdgeCellType;
  using InputPolygonCellType = typename InputMeshType::PolygonCellType;
  using InputPointIdList = typename InputMeshType::PointIdList;

  using InputQEIterator = typename InputQEType::IteratorGeom;

  /** Output types. */
  using OutputMeshType = TOutputMesh;
  using OutputMeshPointer = typename OutputMeshType::Pointer;
  using OutputCoordRepType = typename OutputMeshType::CoordRepType;
  using OutputPointType = typename OutputMeshType::PointType;
  using OutputPointIdentifier = typename OutputMeshType::PointIdentifier;
  using OutputCellType = typename OutputMeshType::CellType;
  using OutputCellIdentifier = typename OutputMeshType::CellIdentifier;
  using OutputEdgeCellType = typename OutputMeshType::EdgeCellType;
  using OutputQEType = typename OutputMeshType::QEType;
  using OutputLineCellIdentifier = typename OutputQEType::LineCellIdentifier;
  using OutputVectorType = typename OutputMeshType::VectorType;
  using OutputQEIterator = typename OutputQEType::IteratorGeom;
  using OutputPointsContainerPointer = typename OutputMeshType::PointsContainerPointer;
  using OutputPointsContainerIterator = typename OutputMeshType::PointsContainerIterator;
  using OutputCellsContainer = typename OutputMeshType::CellsContainer;
  using OutputCellsContainerIterator = typename OutputMeshType::CellsContainerIterator;

  static constexpr unsigned int OutputVDimension = OutputMeshType::PointDimension;

  itkNewMacro(Self);
  itkTypeMacro(DelaunayConformingQuadEdgeMeshFilter, QuadEdgeMeshToQuadEdgeMeshFilter);

  itkGetConstMacro(NumberOfEdgeFlips, SizeValueType);

public:
  using OutputEdgeCellListType = std::list<OutputEdgeCellType *>;
  using OutputEdgeCellListIterator = typename OutputEdgeCellListType::iterator;

  using CriterionValueType = double;
  using PriorityType = std::pair<bool, CriterionValueType>;

  using PriorityQueueItemType = MaxPriorityQueueElementWrapper<OutputEdgeCellType *, PriorityType, long>;

  using PriorityQueueType = PriorityQueueContainer<PriorityQueueItemType *,
                                                   ElementWrapperPointerInterface<PriorityQueueItemType *>,
                                                   PriorityType,
                                                   long>;

  using PriorityQueuePointer = typename PriorityQueueType::Pointer;
  using QueueMapType = std::map<OutputEdgeCellType *, PriorityQueueItemType *>;
  using QueueMapIterator = typename QueueMapType::iterator;

  using FlipEdgeFunctionType = QuadEdgeMeshEulerOperatorFlipEdgeFunction<OutputMeshType, OutputQEType>;
  using FlipEdgeFunctionPointer = typename FlipEdgeFunctionType::Pointer;

  void
  SetListOfConstrainedEdges(const OutputEdgeCellListType & iList)
  {
    m_ListOfConstrainedEdges = iList;
  }

protected:
  DelaunayConformingQuadEdgeMeshFilter();
  ~DelaunayConformingQuadEdgeMeshFilter() override;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  OutputEdgeCellListType m_ListOfConstrainedEdges;
  PriorityQueuePointer   m_PriorityQueue;
  QueueMapType           m_QueueMapper;

  SizeValueType           m_NumberOfEdgeFlips;
  FlipEdgeFunctionPointer m_FlipEdge;

  void
  GenerateData() override;

  void
  InitializePriorityQueue();

  void
  Process();

  void
  ReassignCellData(const OutputCellIdentifier & in, const OutputCellIdentifier & out);

  inline CriterionValueType
  Dyer07Criterion(OutputMeshType * iMesh, OutputQEType * iEdge) const
  {
    OutputPointIdentifier id1 = iEdge->GetOrigin();
    OutputPointIdentifier id2 = iEdge->GetDestination();

    OutputPointIdentifier idA = iEdge->GetLnext()->GetDestination();
    OutputPointIdentifier idB = iEdge->GetRnext()->GetOrigin();

    OutputPointType pt1 = iMesh->GetPoint(id1);
    OutputPointType pt2 = iMesh->GetPoint(id2);
    OutputPointType ptA = iMesh->GetPoint(idA);
    OutputPointType ptB = iMesh->GetPoint(idB);

    OutputVectorType v1A = ptA - pt1;
    OutputVectorType v1B = ptB - pt1;
    OutputVectorType v2A = ptA - pt2;
    OutputVectorType v2B = ptB - pt2;

    OutputCoordRepType sq_norm1A = v1A * v1A;
    OutputCoordRepType sq_norm1B = v1B * v1B;
    OutputCoordRepType sq_norm2A = v2A * v2A;
    OutputCoordRepType sq_norm2B = v2B * v2B;

    auto dotA = static_cast<CriterionValueType>(v1A * v2A);
    auto dotB = static_cast<CriterionValueType>(v1B * v2B);
    auto den = static_cast<CriterionValueType>(sq_norm1A * sq_norm2A);

    if (den != 0.)
    {
      dotA /= std::sqrt(den);
    }

    if (dotA > 1.)
    {
      dotA = 1.;
    }

    if (dotA < -1.)
    {
      dotA = -1.;
    }

    den = static_cast<CriterionValueType>(sq_norm1B * sq_norm2B);

    if (den != 0.)
    {
      dotB /= std::sqrt(den);
    }

    if (dotB > 1.)
    {
      dotB = 1.;
    }

    if (dotB < -1.)
    {
      dotB = -1.;
    }

    return (std::acos(dotA) + std::acos(dotB) - itk::Math::pi);
  }
};
} // end namespace itk

#include "itkDelaunayConformingQuadEdgeMeshFilter.hxx"

#endif
