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
#ifndef itkPolyDataToMeshFilter_hxx
#define itkPolyDataToMeshFilter_hxx

#include "itkPolyDataToMeshFilter.h"

#include "itkVertexCell.h"
#include "itkLineCell.h"
#include "itkMesh.h"
#include "itkTriangleCell.h"
#include "itkQuadrilateralCell.h"
#include "itkPolygonCell.h"

namespace itk
{

template <typename TInputPolyData>
PolyDataToMeshFilter<TInputPolyData>::PolyDataToMeshFilter()
{
  // Modify superclass default values, can be overridden by subclasses
  this->SetNumberOfRequiredInputs(1);

  typename MeshType::Pointer output = static_cast<MeshType *>(this->MakeOutput(0).GetPointer());
  this->ProcessObject::SetNumberOfRequiredOutputs(1);
  this->ProcessObject::SetNthOutput(0, output.GetPointer());
}


template <typename TInputPolyData>
void
PolyDataToMeshFilter<TInputPolyData>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}


template <typename TInputPolyData>
void
PolyDataToMeshFilter<TInputPolyData>::SetInput(const TInputPolyData * input)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput(0, const_cast<TInputPolyData *>(input));
}


template <typename TInputPolyData>
const typename PolyDataToMeshFilter<TInputPolyData>::InputPolyDataType *
PolyDataToMeshFilter<TInputPolyData>::GetInput() const
{
  return itkDynamicCastInDebugMode<const TInputPolyData *>(this->GetPrimaryInput());
}


template <typename TInputPolyData>
const typename PolyDataToMeshFilter<TInputPolyData>::InputPolyDataType *
PolyDataToMeshFilter<TInputPolyData>::GetInput(unsigned int idx) const
{
  return dynamic_cast<const InputPolyDataType *>(this->ProcessObject::GetInput(idx));
}


template <typename TInputPolyData>
ProcessObject::DataObjectPointer PolyDataToMeshFilter<TInputPolyData>::MakeOutput(
  ProcessObject::DataObjectPointerArraySizeType)
{
  return MeshType::New().GetPointer();
}


template <typename TInputPolyData>
ProcessObject::DataObjectPointer
PolyDataToMeshFilter<TInputPolyData>::MakeOutput(const ProcessObject::DataObjectIdentifierType &)
{
  return MeshType::New().GetPointer();
}


template <typename TInputPolyData>
typename PolyDataToMeshFilter<TInputPolyData>::MeshType *
PolyDataToMeshFilter<TInputPolyData>::GetOutput()
{
  // we assume that the first output is of the templated type
  return itkDynamicCastInDebugMode<MeshType *>(this->GetPrimaryOutput());
}


template <typename TInputPolyData>
const typename PolyDataToMeshFilter<TInputPolyData>::MeshType *
PolyDataToMeshFilter<TInputPolyData>::GetOutput() const
{
  // we assume that the first output is of the templated type
  return itkDynamicCastInDebugMode<const MeshType *>(this->GetPrimaryOutput());
}


template <typename TInputPolyData>
typename PolyDataToMeshFilter<TInputPolyData>::MeshType *
PolyDataToMeshFilter<TInputPolyData>::GetOutput(unsigned int idx)
{
  auto * out = dynamic_cast<MeshType *>(this->ProcessObject::GetOutput(idx));

  if (out == nullptr && this->ProcessObject::GetOutput(idx) != nullptr)
  {
    itkWarningMacro(<< "Unable to convert output number " << idx << " to type " << typeid(MeshType).name());
  }
  return out;
}

template <typename TInputPolyData>
void
PolyDataToMeshFilter<TInputPolyData>::GenerateOutputInformation()
{
  // Polydata does not contain sufficient metadata to inform a PointSet or Mesh object
  // so do nothing and leave default point set metadata values

  // m_MaximumNumberOfRegions = 1;
  // m_NumberOfRegions = 1;
  // m_BufferedRegion = -1;
  // m_RequestedNumberOfRegions = 0;
  // m_RequestedRegion = -1;
}

template <typename TInputPolyData>
void
PolyDataToMeshFilter<TInputPolyData>::GenerateData()
{
  const InputPolyDataType * inputPolyData = this->GetInput();
  MeshType *                outputMesh = this->GetOutput();

  // Set points in output mesh
  using PolyDataPointsContainerType = typename InputPolyDataType::PointsContainer;
  using MeshPointsContainerType = typename OutputMeshType::PointsContainer;
  const PolyDataPointsContainerType *       inputPoints = inputPolyData->GetPoints();
  typename MeshPointsContainerType::Pointer outputPoints = MeshPointsContainerType::New();
  outputPoints->resize(inputPoints->Size());

  typename PolyDataPointsContainerType::ConstIterator inputPointItr = inputPoints->Begin();
  typename PolyDataPointsContainerType::ConstIterator inputPointEnd = inputPoints->End();

  typename MeshPointsContainerType::Iterator outputPointItr = outputPoints->Begin();
  while (inputPointItr != inputPointEnd)
  {
    for (unsigned int ii = 0; ii < InputPolyDataType::PointDimension; ++ii)
    {
      outputPointItr.Value()[ii] = inputPointItr.Value()[ii];
    }
    ++inputPointItr;
    ++outputPointItr;
  }
  outputMesh->SetPoints(outputPoints);

  // Set point data in output mesh
  using PointDataContainerType = typename InputPolyDataType::PointDataContainer;
  const PointDataContainerType * inputPointData = inputPolyData->GetPointData();
  if (inputPointData)
  {
    typename PointDataContainerType::Pointer outputPointData = PointDataContainerType::New();
    outputPointData->Reserve(inputPointData->Size());

    typename PointDataContainerType::ConstIterator inputPointDataItr = inputPointData->Begin();
    typename PointDataContainerType::ConstIterator inputPointDataEnd = inputPointData->End();

    typename PointDataContainerType::Iterator outputPointDataItr = outputPointData->Begin();

    while (inputPointDataItr != inputPointDataEnd)
    {
      outputPointDataItr.Value() = inputPointDataItr.Value();
      ++inputPointDataItr;
      ++outputPointDataItr;
    }
    outputMesh->SetPointData(outputPointData);
  }

  // Set different cell types
  using CellContainerType = typename InputPolyDataType::CellsContainer;
  using CellType = typename OutputMeshType::CellType;
  typename CellContainerType::ConstIterator inputCellItr;
  typename CellContainerType::ConstIterator inputCellEnd;

  IdentifierType cellId = 0;

  // Set vertex cells
  using VertexCellType = itk::VertexCell<CellType>;
  if (inputPolyData->GetVertices() != nullptr && !inputPolyData->GetVertices()->empty())
  {
    const CellContainerType * inputVertices = inputPolyData->GetVertices();
    inputCellItr = inputVertices->Begin();
    inputCellEnd = inputVertices->End();

    while (inputCellItr != inputCellEnd)
    {
      auto numPoints = inputCellItr.Value();
      ++inputCellItr;

      // Verify vertex contains exactly one point ID
      itkAssertInDebugAndIgnoreInReleaseMacro(numPoints == VertexCellType::NumberOfPoints);

      // Create cell
      typename CellType::CellAutoPointer cell;
      cell.TakeOwnership(new VertexCellType);

      cell->SetPointId(0, inputCellItr.Value());
      ++inputCellItr;

      outputMesh->SetCell(cellId, cell);
      cellId++;
    }
  }

  // Set line cells
  using LineCellType = itk::LineCell<CellType>;
  if (inputPolyData->GetLines() != nullptr && !inputPolyData->GetLines()->empty())
  {
    const CellContainerType * inputLines = inputPolyData->GetLines();
    inputCellItr = inputLines->Begin();
    inputCellEnd = inputLines->End();

    while (inputCellItr != inputCellEnd)
    {
      auto numPoints = inputCellItr.Value();
      ++inputCellItr;

      // Verify lines contain exactly two point IDs
      itkAssertInDebugAndIgnoreInReleaseMacro(numPoints == LineCellType::NumberOfPoints);

      // Create cell
      typename CellType::CellAutoPointer cell;
      cell.TakeOwnership(new LineCellType);

      for (unsigned int i = 0; i < numPoints; i++)
      {
        cell->SetPointId(i, inputCellItr.Value());
        ++inputCellItr;
      }

      outputMesh->SetCell(cellId, cell);
      cellId++;
    }
  }

  // Set triangle cells from strips
  using TriangleCellType = itk::TriangleCell<CellType>;
  if (inputPolyData->GetTriangleStrips() != nullptr && !inputPolyData->GetTriangleStrips()->empty())
  {
    const CellContainerType * inputStrips = inputPolyData->GetTriangleStrips();
    inputCellItr = inputStrips->Begin();
    inputCellEnd = inputStrips->End();

    while (inputCellItr != inputCellEnd)
    {
      auto numPoints = inputCellItr.Value();
      ++inputCellItr;

      // Verify at least one strip is described
      itkAssertInDebugAndIgnoreInReleaseMacro(numPoints >= TriangleCellType::NumberOfPoints);

      // Create cell
      typename CellContainerType::ConstIterator stripCellEnd = inputCellItr;

      for (unsigned int i = 0; i < numPoints - 2; i++)
      {
        typename CellType::CellAutoPointer cell;
        cell.TakeOwnership(new TriangleCellType);

        cell->SetPointId(0, inputCellItr.Value());
        inputCellItr++;
        cell->SetPointId(1, inputCellItr.Value());
        inputCellItr++;
        cell->SetPointId(2, inputCellItr.Value());
        inputCellItr--;

        outputMesh->SetCell(cellId, cell);
        cellId++;
      }
      inputCellItr += 2;
    }
  }

  // Set polygons
  const auto * inputPolygons = inputPolyData->GetPolygons();
  if (inputPolygons != nullptr && !inputPolygons->empty())
  {
    inputCellItr = inputPolygons->Begin();
    inputCellEnd = inputPolygons->End();

    using QuadrilateralCellType = itk::QuadrilateralCell<CellType>;
    using PolygonCellType = itk::PolygonCell<CellType>;

    // Polygons are stored in a 1D list as [# points] [p1] [p2] ... [# points] [p1] [p2] ... etc
    while (inputCellItr != inputCellEnd)
    {
      auto numPoints = inputCellItr.Value();
      ++inputCellItr;

      typename CellType::CellAutoPointer cell;
      if (numPoints == VertexCellType::NumberOfPoints)
      {
        cell.TakeOwnership(new VertexCellType);
      }
      else if (numPoints == LineCellType::NumberOfPoints)
      {
        cell.TakeOwnership(new LineCellType);
      }
      else if (numPoints == TriangleCellType::NumberOfPoints)
      {
        cell.TakeOwnership(new TriangleCellType);
      }
      else if (numPoints == QuadrilateralCellType::NumberOfPoints)
      {
        cell.TakeOwnership(new QuadrilateralCellType);
      }
      else
      {
        cell.TakeOwnership(new PolygonCellType(numPoints));
      }

      for (unsigned int i = 0; i < numPoints; i++)
      {
        cell->SetPointId(i, inputCellItr.Value());
        ++inputCellItr;
      }

      outputMesh->SetCell(cellId, cell);
      cellId++;
    }
  }

  // Set cell data in output mesh
  using CellDataContainerType = typename InputPolyDataType::CellDataContainer;
  const CellDataContainerType * inputCellData = inputPolyData->GetCellData();
  if (inputCellData)
  {
    typename CellDataContainerType::Pointer outputCellData = CellDataContainerType::New();
    outputCellData->Reserve(inputCellData->Size());

    typename CellDataContainerType::ConstIterator inputCellDataItr = inputCellData->Begin();
    typename CellDataContainerType::ConstIterator inputCellDataEnd = inputCellData->End();

    typename CellDataContainerType::Iterator outputCellDataItr = outputCellData->Begin();

    while (inputCellDataItr != inputCellDataEnd)
    {
      outputCellDataItr.Value() = inputCellDataItr.Value();
      ++inputCellDataItr;
      ++outputCellDataItr;
    }
    outputMesh->SetCellData(outputCellData);
  }
}

} // end namespace itk

#endif // itkPolyDataToMeshFilter_hxx
