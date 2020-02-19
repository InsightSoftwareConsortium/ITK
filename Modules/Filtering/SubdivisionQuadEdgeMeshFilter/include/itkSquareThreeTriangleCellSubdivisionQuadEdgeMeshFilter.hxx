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
#ifndef itkSquareThreeTriangleCellSubdivisionQuadEdgeMeshFilter_hxx
#define itkSquareThreeTriangleCellSubdivisionQuadEdgeMeshFilter_hxx

#include "itkSquareThreeTriangleCellSubdivisionQuadEdgeMeshFilter.h"

namespace itk
{
template <typename TInputMesh, typename TOutputMesh>
void
SquareThreeTriangleCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TOutputMesh>::AddNewCellPoints(InputCellType * cell)
{
  if (cell->GetType() != InputCellType::POLYGON_CELL || cell->GetNumberOfPoints() != 3)
  {
    itkExceptionMacro(<< " The input cell is not a triangle cell");
  }

  const InputMeshType * input = this->GetInput();
  OutputMeshType *      output = this->GetOutput();

  InputPointIdentifier pointIdArray[3];

  InputPointType newPoint;
  newPoint.Fill(NumericTraits<typename OutputPointType::ValueType>::Zero);

  InputPointIdIterator pter = cell->PointIdsBegin();
  InputPointIdentifier nn = 0;

  while (pter != cell->PointIdsEnd())
  {
    pointIdArray[nn] = *pter;
    InputPointType cellPoint = input->GetPoints()->ElementAt(*pter);
    newPoint += cellPoint.GetVectorFromOrigin();
    ++pter;
    ++nn;
  }

  typename InputPointType::ValueType den = 1. / static_cast<typename InputPointType::ValueType>(nn);

  if (den < NumericTraits<typename InputPointType::ValueType>::epsilon())
  {
    itkExceptionMacro("Exception caused by unusual large number of points inside a cell");
  }

  for (unsigned int kk = 0; kk < InputPointType::PointDimension; ++kk)
  {
    newPoint[kk] *= den;
  }

  OutputPointType outPoint;
  outPoint.CastFrom(newPoint);
  OutputPointIdentifier newPointId = output->GetNumberOfPoints();
  output->SetPoint(newPointId, outPoint);

  for (unsigned int ii = 0; ii < 3; ++ii)
  {
    unsigned int  jj = (ii + 1) % 3;
    InputQEType * edge = input->FindEdge(pointIdArray[ii], pointIdArray[jj]);

    if (edge && !this->m_EdgesPointIdentifier->IndexExists(edge))
    {
      this->m_EdgesPointIdentifier->InsertElement(edge, newPointId);
    }
  }
}

template <typename TInputMesh, typename TOutputMesh>
void
SquareThreeTriangleCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TOutputMesh>::GenerateOutputCells()
{
  // 1. Get input and output
  const InputMeshType * input = this->GetInput();
  OutputMeshType *      output = this->GetOutput();

  // 2. Get cells container from input
  const InputCellsContainer * cells = input->GetCells();

  // 3. Clearn cells to be subdivided.
  this->m_CellsToBeSubdivided.clear();

  // 4. Copy all the cell that does not insert a new point.
  InputCellsContainerConstIterator cellIt = cells->Begin();
  while (cellIt != cells->End())
  {
    InputCellType * cell = cellIt->Value();

    if (cell->GetType() != InputCellType::POLYGON_CELL || cell->GetNumberOfPoints() != 3)
    {
      continue;
    }

    InputPointIdentifier cellPointIdArray[3];

    InputPointIdIterator it = cell->PointIdsBegin();
    unsigned int         n = 0;

    while (it != cell->PointIdsEnd())
    {
      cellPointIdArray[n] = *it;
      ++it;
      ++n;
    }

    for (unsigned int ii = 0; ii < 3; ++ii)
    {
      unsigned int jj = (ii + 1) % 3;

      InputQEType * edge = input->FindEdge(cellPointIdArray[ii], cellPointIdArray[jj]);

      if (!this->m_EdgesPointIdentifier->IndexExists(edge))
      {
        // No added new point in this triangle cell
        output->AddFaceTriangle(static_cast<OutputPointIdentifier>(cellPointIdArray[0]),
                                static_cast<OutputPointIdentifier>(cellPointIdArray[1]),
                                static_cast<OutputPointIdentifier>(cellPointIdArray[2]));
        break;
      }
      else if (this->m_EdgesPointIdentifier->ElementAt(edge) != NumericTraits<OutputPointIdentifier>::max())
      {
        // With a new added point in this triangle cell.
        OutputPointIdentifier pointIdArray[2][2];
        pointIdArray[0][0] = static_cast<OutputPointIdentifier>(edge->GetOrigin());
        pointIdArray[0][1] = static_cast<OutputPointIdentifier>(edge->GetDestination());
        pointIdArray[1][0] = this->m_EdgesPointIdentifier->ElementAt(edge);

        if (edge->IsAtBorder() || !this->m_EdgesPointIdentifier->IndexExists(edge->GetSym()))
        {
          // There is no neighbor triangle cell or no splitting of the neighbor triangle cell.
          if (this->m_Uniform)
          {
            output->AddFaceTriangle(pointIdArray[0][0], pointIdArray[0][1], pointIdArray[1][0]);
          }
          else
          {
            OutputQEType * newTriangleEdge =
              output->AddFaceTriangle(pointIdArray[0][0], pointIdArray[0][1], pointIdArray[1][0]);
            this->m_CellsToBeSubdivided.push_back(newTriangleEdge->GetLeft());
          }
        }
        else
        {
          // There is neighbor triangle cell was split as well, swapping edges was conducted.
          pointIdArray[1][1] = this->m_EdgesPointIdentifier->ElementAt(edge->GetSym());
          if (this->m_Uniform)
          {
            output->AddFaceTriangle(pointIdArray[1][0], pointIdArray[1][1], pointIdArray[0][1]);
            output->AddFaceTriangle(pointIdArray[1][1], pointIdArray[1][0], pointIdArray[0][0]);
          }
          else
          {
            OutputQEType * newTriangleEdge =
              output->AddFaceTriangle(pointIdArray[1][0], pointIdArray[1][1], pointIdArray[0][1]);
            this->m_CellsToBeSubdivided.push_back(newTriangleEdge->GetLeft());

            newTriangleEdge = output->AddFaceTriangle(pointIdArray[1][1], pointIdArray[1][0], pointIdArray[0][0]);
            this->m_CellsToBeSubdivided.push_back(newTriangleEdge->GetLeft());
          }
          this->m_EdgesPointIdentifier->SetElement(edge->GetSym(), NumericTraits<OutputPointIdentifier>::max());
        }
      }
    }
    ++cellIt;
  }
}

} // namespace itk
#endif
