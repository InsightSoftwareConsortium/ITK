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
#ifndef itkQuadEdgeMeshToQuadEdgeMeshFilter_h
#define itkQuadEdgeMeshToQuadEdgeMeshFilter_h

#include "itkMeshToMeshFilter.h"

namespace itk
{
/**
 *\class QuadEdgeMeshToQuadEdgeMeshFilter
 *  \brief Duplicates the content of a Mesh
 *
 * \author Alexandre Gouaillard, Leonardo Florez-Valencia, Eric Boix
 *
 * This implementation was contributed as a paper to the Insight Journal
 * https://www.insight-journal.org/browse/publication/122
 *
 * \ingroup ITKQuadEdgeMesh
 */
template <typename TInputMesh, typename TOutputMesh>
class ITK_TEMPLATE_EXPORT QuadEdgeMeshToQuadEdgeMeshFilter : public MeshToMeshFilter<TInputMesh, TOutputMesh>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(QuadEdgeMeshToQuadEdgeMeshFilter);

  /** Basic types. */
  using Self = QuadEdgeMeshToQuadEdgeMeshFilter;
  using Superclass = MeshToMeshFilter<TInputMesh, TOutputMesh>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Input types. */
  using InputMeshType = TInputMesh;
  using InputMeshPointer = typename InputMeshType::Pointer;
  using InputMeshConstPointer = typename InputMeshType::ConstPointer;
  using InputCoordRepType = typename InputMeshType::CoordRepType;
  using InputPointType = typename InputMeshType::PointType;
  using InputPointIdentifier = typename InputMeshType::PointIdentifier;
  using InputQEPrimal = typename InputMeshType::QEPrimal;
  using InputVectorType = typename InputMeshType::VectorType;

  using InputPointDataContainer = typename InputMeshType::PointDataContainer;
  using InputCellDataContainer = typename InputMeshType::CellDataContainer;

  using InputPointDataContainerConstPointer = typename InputPointDataContainer::ConstPointer;
  using InputPointsContainerConstIterator = typename InputMeshType::PointsContainerConstIterator;
  using InputPointsContainerConstPointer = typename InputMeshType::PointsContainerConstPointer;
  using InputCellsContainerConstIterator = typename InputMeshType::CellsContainerConstIterator;
  using InputCellsContainerConstPointer = typename InputMeshType::CellsContainerConstPointer;

  using InputEdgeCellType = typename InputMeshType::EdgeCellType;
  using InputPolygonCellType = typename InputMeshType::PolygonCellType;
  using InputPointIdList = typename InputMeshType::PointIdList;
  using InputCellTraits = typename InputMeshType::CellTraits;
  using InputPointsIdInternalIterator = typename InputCellTraits::PointIdInternalIterator;

  using InputQEIterator = typename InputQEPrimal::IteratorGeom;

  /** Output types. */
  using OutputMeshType = TOutputMesh;
  using OutputMeshPointer = typename OutputMeshType::Pointer;
  using OutputMeshConstPointer = typename OutputMeshType::ConstPointer;
  using OutputCoordRepType = typename OutputMeshType::CoordRepType;
  using OutputPointType = typename OutputMeshType::PointType;
  using OutputPointIdentifier = typename OutputMeshType::PointIdentifier;
  using OutputQEPrimal = typename OutputMeshType::QEPrimal;
  using OutputVectorType = typename OutputMeshType::VectorType;
  using OutputQEIterator = typename OutputQEPrimal::IteratorGeom;
  using OutputPointsContainerIterator = typename OutputMeshType::PointsContainerIterator;
  using OutputPointsContainerPointer = typename OutputMeshType::PointsContainerPointer;
  using OutputPointsContainerConstPointer = typename OutputMeshType::PointsContainerConstPointer;

  using OutputPointDataContainer = typename OutputMeshType::PointDataContainer;
  using OutputCellDataContainer = typename OutputMeshType::CellDataContainer;

public:
  itkNewMacro(Self);
  itkTypeMacro(QuadEdgeMeshToQuadEdgeMeshFilter, MeshToMeshFilter);

protected:
  QuadEdgeMeshToQuadEdgeMeshFilter();
  ~QuadEdgeMeshToQuadEdgeMeshFilter() override = default;

  virtual void
  CopyInputMeshToOutputMesh();

  virtual void
  CopyInputMeshToOutputMeshGeometry();

  virtual void
  CopyInputMeshToOutputMeshPoints();

  virtual void
  CopyInputMeshToOutputMeshCells();

  virtual void
  CopyInputMeshToOutputMeshEdgeCells();

  virtual void
  CopyInputMeshToOutputMeshFieldData();

  virtual void
  CopyInputMeshToOutputMeshPointData();

  virtual void
  CopyInputMeshToOutputMeshCellData();
};

//
// Helper functions that copy selected pieces of a Mesh.
// These functions should be templated here in order to
// facilitate their reuse in multiple scenarios.
//
template <typename TInputMesh, typename TOutputMesh>
void
CopyMeshToMesh(const TInputMesh * in, TOutputMesh * out)
{
  CopyMeshToMeshPoints(in, out);
  CopyMeshToMeshEdgeCells(in, out);
  CopyMeshToMeshCells(in, out);
  CopyMeshToMeshPointData(in, out);
  CopyMeshToMeshCellData(in, out);
}

// ---------------------------------------------------------------------
template <typename TInputMesh, typename TOutputMesh>
void
CopyMeshToMeshCellData(const TInputMesh * in, TOutputMesh * out)
{
  using InputCellDataContainer = typename TInputMesh::CellDataContainer;
  using OutputCellDataContainer = typename TOutputMesh::CellDataContainer;
  using InputCellDataContainerConstPointer = typename InputCellDataContainer::ConstPointer;
  using OutputCellDataContainerPointer = typename OutputCellDataContainer::Pointer;

  InputCellDataContainerConstPointer inputCellData = in->GetCellData();

  if (inputCellData == nullptr)
  {
    // There is nothing to copy
    return;
  }

  OutputCellDataContainerPointer outputCellData = OutputCellDataContainer::New();
  outputCellData->Reserve(inputCellData->Size());

  // Copy point data
  using InputCellDataContainerConstIterator = typename InputCellDataContainer::ConstIterator;
  InputCellDataContainerConstIterator inIt = inputCellData->Begin();
  while (inIt != inputCellData->End())
  {
    typename OutputCellDataContainer::Element point(inIt.Value());
    outputCellData->SetElement(inIt.Index(), point);
    ++inIt;
  }

  out->SetCellData(outputCellData);
}

// ---------------------------------------------------------------------
template <typename TInputMesh, typename TOutputMesh>
void
CopyMeshToMeshPointData(const TInputMesh * in, TOutputMesh * out)
{
  using OutputPointDataContainer = typename TOutputMesh::PointDataContainer;
  using OutputPointDataContainerPointer = typename OutputPointDataContainer::Pointer;
  using InputPointDataContainer = typename TInputMesh::PointDataContainer;

  const InputPointDataContainer * inputPointData = in->GetPointData();

  if (inputPointData == nullptr)
  {
    // There is nothing to copy
    return;
  }

  OutputPointDataContainerPointer outputPointData = OutputPointDataContainer::New();
  outputPointData->Reserve(inputPointData->Size());

  // Copy point data
  using InputPointDataContainerConstIterator = typename InputPointDataContainer::ConstIterator;
  InputPointDataContainerConstIterator inIt = inputPointData->Begin();
  while (inIt != inputPointData->End())
  {
    typename OutputPointDataContainer::Element point(inIt.Value());
    outputPointData->SetElement(inIt.Index(), point);
    ++inIt;
  }

  out->SetPointData(outputPointData);
}

// ---------------------------------------------------------------------
template <typename TInputMesh, typename TOutputMesh>
void
CopyMeshToMeshCells(const TInputMesh * in, TOutputMesh * out)
{
  // Copy cells
  using InputCellsContainer = typename TInputMesh::CellsContainer;
  using InputCellsContainerConstPointer = typename InputCellsContainer::ConstPointer;
  using InputCellsContainerConstIterator = typename InputCellsContainer::ConstIterator;
  using InputPolygonCellType = typename TInputMesh::PolygonCellType;
  using InputPointIdList = typename TInputMesh::PointIdList;
  using InputCellTraits = typename TInputMesh::CellTraits;
  using InputPointsIdInternalIterator = typename InputCellTraits::PointIdInternalIterator;

  out->SetCellsAllocationMethod(MeshEnums::MeshClassCellsAllocationMethod::CellsAllocatedDynamicallyCellByCell);

  InputCellsContainerConstPointer inCells = in->GetCells();

  if (inCells == nullptr)
  {
    // There is nothing to copy
    return;
  }

  InputCellsContainerConstIterator cIt = inCells->Begin();
  InputCellsContainerConstIterator cEnd = inCells->End();
  while (cIt != cEnd)
  {
    auto * pe = dynamic_cast<InputPolygonCellType *>(cIt.Value());
    if (pe)
    {
      InputPointIdList              points;
      InputPointsIdInternalIterator pIt = pe->InternalPointIdsBegin();
      InputPointsIdInternalIterator pEnd = pe->InternalPointIdsEnd();

      while (pIt != pEnd)
      {
        points.push_back((*pIt));
        ++pIt;
      }
      out->AddFaceWithSecurePointList(points, false);
    }
    ++cIt;
  }
}

// ---------------------------------------------------------------------
template <typename TInputMesh, typename TOutputMesh>
void
CopyMeshToMeshEdgeCells(const TInputMesh * in, TOutputMesh * out)
{
  // Copy Edge Cells
  using InputCellsContainer = typename TInputMesh::CellsContainer;
  using InputCellsContainerConstPointer = typename InputCellsContainer::ConstPointer;
  using InputCellsContainerConstIterator = typename InputCellsContainer::ConstIterator;
  using InputEdgeCellType = typename TInputMesh::EdgeCellType;

  InputCellsContainerConstPointer inEdgeCells = in->GetEdgeCells();

  if (inEdgeCells == nullptr)
  {
    // There is nothing to copy
    return;
  }

  InputCellsContainerConstIterator ecIt = inEdgeCells->Begin();
  InputCellsContainerConstIterator ecEnd = inEdgeCells->End();

  while (ecIt != ecEnd)
  {
    auto * pe = dynamic_cast<InputEdgeCellType *>(ecIt.Value());
    if (pe)
    {
      out->AddEdgeWithSecurePointList(pe->GetQEGeom()->GetOrigin(), pe->GetQEGeom()->GetDestination());
    }
    ++ecIt;
  }
}

// ---------------------------------------------------------------------
template <typename TInputMesh, typename TOutputMesh>
void
CopyMeshToMeshPoints(const TInputMesh * in, TOutputMesh * out)
{
  // Copy points
  using InputPointsContainerConstPointer = typename TInputMesh::PointsContainerConstPointer;
  using InputPointsContainerConstIterator = typename TInputMesh::PointsContainerConstIterator;

  using OutputPointsContainer = typename TOutputMesh::PointsContainer;
  using OutputPointsContainerPointer = typename TOutputMesh::PointsContainerPointer;
  using OutputPointType = typename TOutputMesh::PointType;

  InputPointsContainerConstPointer inPoints = in->GetPoints();

  if (inPoints == nullptr)
  {
    // There is nothing to copy
    return;
  }

  InputPointsContainerConstIterator inIt = inPoints->Begin();
  InputPointsContainerConstIterator inEnd = inPoints->End();

  OutputPointsContainerPointer oPoints = out->GetPoints();
  if (oPoints == nullptr)
  {
    oPoints = OutputPointsContainer::New();
    out->SetPoints(oPoints);
  }
  OutputPointType pOut;

  while (inIt != inEnd)
  {
    pOut.CastFrom(inIt.Value());
    oPoints->InsertElement(inIt.Index(), pOut);
    ++inIt;
  }
}
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkQuadEdgeMeshToQuadEdgeMeshFilter.hxx"
#endif

#endif
