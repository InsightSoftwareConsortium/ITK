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

#ifndef itkTriangleCellSubdivisionQuadEdgeMeshFilter_hxx
#define itkTriangleCellSubdivisionQuadEdgeMeshFilter_hxx

#include "itkTriangleCellSubdivisionQuadEdgeMeshFilter.h"

namespace itk
{
template <typename TInputMesh, typename TOutputMesh>
TriangleCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TOutputMesh>::TriangleCellSubdivisionQuadEdgeMeshFilter()
{
  this->m_Uniform = true;
}

template <typename TInputMesh, typename TOutputMesh>
void
TriangleCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TOutputMesh>::SetCellsToBeSubdivided(
  const SubdivisionCellContainer & cellIdList)
{
  this->m_CellsToBeSubdivided = cellIdList;
  this->Modified();
}

template <typename TInputMesh, typename TOutputMesh>
void
TriangleCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TOutputMesh>::AddSubdividedCellId(OutputCellIdentifier cellId)
{
  this->m_CellsToBeSubdivided.push_back(cellId);
  this->Modified();
}

template <typename TInputMesh, typename TOutputMesh>
void
TriangleCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TOutputMesh>::GenerateOutputPoints()
{
  this->CopyInputMeshToOutputMeshPoints();

  this->m_EdgesPointIdentifier->Initialize();

  this->m_Uniform = this->m_CellsToBeSubdivided.empty();

  const InputCellsContainer * cells = this->GetInput()->GetCells();

  if (this->m_Uniform)
  {
    InputCellsContainerConstIterator cellIt = cells->Begin();
    while (cellIt != cells->End())
    {
      this->AddNewCellPoints(cellIt->Value());
      ++cellIt;
    }
  }
  else
  {
    SubdivisionCellContainerConstIterator it = this->m_CellsToBeSubdivided.begin();
    SubdivisionCellContainerConstIterator end = this->m_CellsToBeSubdivided.end();
    while (it != end)
    {
      InputCellType * cell = cells->GetElement(static_cast<InputCellIdentifier>(*it));
      if (cell)
      {
        this->AddNewCellPoints(cell);
      }
      ++it;
    }
  }
}

template <typename TInputMesh, typename TOutputMesh>
void
TriangleCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TOutputMesh>::GenerateOutputCells()
{
  InputMeshConstPointer input = this->GetInput();
  OutputMeshPointer     output = this->GetOutput();

  this->m_CellsToBeSubdivided.clear();

  const InputCellsContainer * cells = input->GetCells();

  InputCellsContainerConstIterator cellIt = cells->Begin();

  while (cellIt != cells->End())
  {
    InputCellType * cell = cellIt->Value();

    if (!cell || cell->GetType() != InputCellType::POLYGON_CELL || cell->GetNumberOfPoints() != 3)
    {
      continue;
    }

    InputPointIdentifier  inputPointIdArray[3];
    OutputPointIdentifier trianglePointIds[3];
    OutputPointIdentifier edgePointIds[3];

    InputPointIdIterator it = cell->PointIdsBegin();
    unsigned int         n = 0;

    while (it != cell->PointIdsEnd())
    {
      inputPointIdArray[n] = *it;
      trianglePointIds[n] = static_cast<OutputPointIdentifier>(inputPointIdArray[n]);
      ++it;
      ++n;
    }

    unsigned int splitEdges[3];
    n = 0;
    InputQEType * edge;
    for (unsigned int ii = 0; ii < 3; ++ii)
    {
      unsigned int jj = (ii + 1) % 3;

      edge = input->FindEdge(inputPointIdArray[ii], inputPointIdArray[jj]);

      if (this->m_EdgesPointIdentifier->IndexExists(edge))
      {
        edgePointIds[ii] = this->m_EdgesPointIdentifier->GetElement(edge);
        splitEdges[n] = ii;
        ++n;
      }
    }

    if (n == 0)
    {
      // this face has no subdivided face as neighbor, copy it
      output->AddFaceTriangle(trianglePointIds[0], trianglePointIds[1], trianglePointIds[2]);
    }
    else if (n == 1)
    {
      SplitTriangleFromOneEdge(output, trianglePointIds, edgePointIds, splitEdges);
    }
    else if (n == 2)
    {
      SplitTriangleFromTwoEdges(output, trianglePointIds, edgePointIds, splitEdges);
    }
    else if (n == 3)
    {
      // this face was not supposed to be subdivided but all neighbors are
      SplitTriangleFromThreeEdges(output, trianglePointIds, edgePointIds);
    }

    ++cellIt;
  }
}

template <typename TInputMesh, typename TOutputMesh>
void
TriangleCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TOutputMesh>::SplitTriangleFromOneEdge(
  OutputMeshType *              output,
  const OutputPointIdentifier * trianglePointIds,
  const OutputPointIdentifier * edgePointIds,
  const unsigned int *          splitEdges)
{
  unsigned int ii = splitEdges[0];
  unsigned int jj = (ii + 1) % 3;
  unsigned int kk = (ii + 2) % 3;

  output->AddFaceTriangle(edgePointIds[ii], trianglePointIds[jj], trianglePointIds[kk]);
  output->AddFaceTriangle(edgePointIds[ii], trianglePointIds[kk], trianglePointIds[ii]);
}

template <typename TInputMesh, typename TOutputMesh>
void
TriangleCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TOutputMesh>::SplitTriangleFromTwoEdges(
  OutputMeshType *              output,
  const OutputPointIdentifier * trianglePointIds,
  const OutputPointIdentifier * edgePointIds,
  const unsigned int *          splitEdges)
{
  unsigned int ii = splitEdges[0];
  unsigned int jj = splitEdges[1];

  if (ii == 0 && jj == 1)
  {
    // ii = 0, jj = 1
    output->AddFaceTriangle(trianglePointIds[2], trianglePointIds[0], edgePointIds[0]);
    output->AddFaceTriangle(trianglePointIds[2], edgePointIds[0], edgePointIds[1]);
    output->AddFaceTriangle(edgePointIds[0], trianglePointIds[1], edgePointIds[1]);
  }
  else if (ii == 0 && jj == 2)
  {
    // ii = 0, jj = 2
    output->AddFaceTriangle(trianglePointIds[1], trianglePointIds[2], edgePointIds[0]);
    output->AddFaceTriangle(trianglePointIds[2], edgePointIds[2], edgePointIds[0]);
    output->AddFaceTriangle(edgePointIds[2], trianglePointIds[0], edgePointIds[0]);
  }
  else if (ii == 1 && jj == 2)
  {
    // ii = 1, jj = 2
    output->AddFaceTriangle(trianglePointIds[0], trianglePointIds[1], edgePointIds[1]);
    output->AddFaceTriangle(trianglePointIds[0], edgePointIds[1], edgePointIds[2]);
    output->AddFaceTriangle(edgePointIds[1], trianglePointIds[2], edgePointIds[2]);
  }
}

template <typename TInputMesh, typename TOutputMesh>
void
TriangleCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TOutputMesh>::SplitTriangleFromThreeEdges(
  OutputMeshType *              output,
  const OutputPointIdentifier * trianglePointIds,
  const OutputPointIdentifier * edgePointIds)
{
  // this face was not supposed to be subdivided but all neighbors are
  if (this->m_Uniform)
  {
    output->AddFaceTriangle(trianglePointIds[0], edgePointIds[0], edgePointIds[2]);
    output->AddFaceTriangle(edgePointIds[0], trianglePointIds[1], edgePointIds[1]);
    output->AddFaceTriangle(edgePointIds[1], trianglePointIds[2], edgePointIds[2]);
    output->AddFaceTriangle(edgePointIds[0], edgePointIds[1], edgePointIds[2]);
  }
  else
  {
    OutputQEType * newTriangleEdge = output->AddFaceTriangle(trianglePointIds[0], edgePointIds[0], edgePointIds[2]);
    this->m_CellsToBeSubdivided.push_back(newTriangleEdge->GetLeft());

    newTriangleEdge = output->AddFaceTriangle(edgePointIds[0], trianglePointIds[1], edgePointIds[1]);
    this->m_CellsToBeSubdivided.push_back(newTriangleEdge->GetLeft());

    newTriangleEdge = output->AddFaceTriangle(edgePointIds[1], trianglePointIds[2], edgePointIds[2]);
    this->m_CellsToBeSubdivided.push_back(newTriangleEdge->GetLeft());

    newTriangleEdge = output->AddFaceTriangle(edgePointIds[0], edgePointIds[1], edgePointIds[2]);
    this->m_CellsToBeSubdivided.push_back(newTriangleEdge->GetLeft());
  }
}

template <typename TInputMesh, typename TOutputMesh>
void
TriangleCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TOutputMesh>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Is Uniform Subdivision: " << m_Uniform << std::endl;
}
} // namespace itk
#endif
