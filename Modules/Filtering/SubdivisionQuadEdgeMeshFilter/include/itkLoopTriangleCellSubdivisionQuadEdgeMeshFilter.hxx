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
#ifndef itkLoopTriangleCellSubdivisionQuadEdgeMeshFilter_hxx
#define itkLoopTriangleCellSubdivisionQuadEdgeMeshFilter_hxx

#include "itkMath.h"
#include <set>

namespace itk
{
template <typename TInputMesh, typename TOutputMesh>
void
LoopTriangleCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TOutputMesh>::AddNewCellPoints(InputCellType * cell)
{
  if (cell->GetType() != CellGeometryEnum::POLYGON_CELL || cell->GetNumberOfPoints() != 3)
  {
    itkExceptionMacro(<< " The input cell is not a triangle cell");
  }

  const InputMeshType * input = this->GetInput();
  OutputMeshType *      output = this->GetOutput();

  InputPointIdentifier  n = 0;
  OutputPointIdentifier pointIdArray[3];

  InputCoordType pointWeight[4] = { 0.375, 0.375, 0.125, 0.125 };
  InputPointType pointArray[4];

  OutputPointIdentifier numberOfPoints = output->GetNumberOfPoints();

  InputPointIdIterator it = cell->PointIdsBegin();
  while (it != cell->PointIdsEnd())
  {
    pointIdArray[n++] = *it;
    ++it;
  }

  for (unsigned int ii = 0; ii < 3; ++ii)
  {
    unsigned int jj = (ii + 1) % 3;

    InputQEType * edge = input->FindEdge(pointIdArray[ii], pointIdArray[jj]);

    if (!this->m_EdgesPointIdentifier->IndexExists(edge))
    {
      if (edge->IsInternal())
      {
        InputPointType newPoint;
        newPoint.Fill(NumericTraits<typename InputPointType::ValueType>::Zero);

        input->GetPoint(pointIdArray[ii], &pointArray[0]);
        input->GetPoint(pointIdArray[jj], &pointArray[1]);

        if (edge->GetLnext())
        {
          input->GetPoint(edge->GetLnext()->GetDestination(), &pointArray[2]);
        }
        else
        {
          pointArray[2].Fill(NumericTraits<typename InputPointType::ValueType>::Zero);
        }

        if (edge->GetRprev())
        {
          input->GetPoint(edge->GetRprev()->GetDestination(), &pointArray[3]);
        }
        else
        {
          pointArray[3].Fill(NumericTraits<typename InputPointType::ValueType>::Zero);
        }

        for (unsigned int kk = 0; kk < InputMeshType::PointDimension; kk++)
        {
          for (unsigned int mm = 0; mm < 4; mm++)
          {
            newPoint[kk] += pointWeight[mm] * pointArray[mm][kk];
          }
        }

        this->m_EdgesPointIdentifier->InsertElement(edge, numberOfPoints);
        this->m_EdgesPointIdentifier->InsertElement(edge->GetSym(), numberOfPoints);

        OutputPointType outPoint;
        outPoint.CastFrom(newPoint);
        output->SetPoint(numberOfPoints, outPoint);
      }
      else if (edge->IsAtBorder())
      {
        input->GetPoint(pointIdArray[ii], &pointArray[0]);
        input->GetPoint(pointIdArray[jj], &pointArray[1]);

        InputPointType newPoint;
        newPoint.SetToMidPoint(pointArray[0], pointArray[1]);

        this->m_EdgesPointIdentifier->InsertElement(edge, numberOfPoints);
        this->m_EdgesPointIdentifier->InsertElement(edge->GetSym(), numberOfPoints);

        OutputPointType outPoint;
        outPoint.CastFrom(newPoint);
        output->SetPoint(numberOfPoints, outPoint);
      }
      else
      {
        itkExceptionMacro(<< "Wire edge detected");
      }
      ++numberOfPoints;
    }
  }
}

template <typename TInputMesh, typename TOutputMesh>
void
LoopTriangleCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TOutputMesh>::CopyInputMeshToOutputMeshPoints()
{
  const InputMeshType * input = this->GetInput();
  OutputMeshType *      output = this->GetOutput();

  const InputPointsContainer * points = input->GetPoints();
  output->GetPoints()->Reserve(input->GetNumberOfPoints());

  const InputCellsContainer * cells = input->GetCells();

  if (this->m_Uniform)
  {
    InputPointsContainerConstIterator ptIt = points->Begin();
    while (ptIt != points->End())
    {
      OutputPointType outpoint;
      //      outpoint.CastFrom( ptIt->Value() );
      outpoint.CastFrom(this->SmoothingPoint(ptIt->Value(), points));
      output->SetPoint(ptIt->Index(), outpoint);
      ++ptIt;
    }
  }
  else
  {
    std::set<InputPointIdentifier>        smoothedPointSet;
    SubdivisionCellContainerConstIterator it = this->m_CellsToBeSubdivided.begin();
    SubdivisionCellContainerConstIterator end = this->m_CellsToBeSubdivided.end();
    while (it != end)
    {
      InputCellType *      cell = cells->GetElement(static_cast<InputCellIdentifier>(*it));
      InputPointIdIterator pter = cell->PointIdsBegin();
      while (pter != cell->PointIdsEnd())
      {
        smoothedPointSet.insert(*it);
        ++pter;
      }

      ++it;
    }

    InputPointsContainerConstIterator ptIt = points->Begin();
    while (ptIt != points->End())
    {
      InputPointType ipt = ptIt->Value();

      if (smoothedPointSet.count(ptIt->Index()))
      {
        /*
        InputPointType opt;
        opt.Fill( NumericTraits< typename InputPointType::ValueType >::Zero );
        unsigned int nn = 0;

        InputPointType bpt;
        bpt.Fill( NumericTraits< typename InputPointType::ValueType >::Zero );
        unsigned int nb = 0;

        InputQEType *edge = ptIt->Value().GetEdge();
        typename InputQEType::IteratorGeom q_it = edge->BeginGeomOnext();
        while ( q_it != edge->EndGeomOnext() )
          {
          if ( q_it.Value()->IsAtBorder() )
            {
            bpt += points->ElementAt( q_it.Value()->GetDestination() ).GetVectorFromOrigin();
            ++nb;
            }

          opt += points->ElementAt( q_it.Value()->GetDestination() ).GetVectorFromOrigin();
          ++nn;
          ++q_it;
          }

        if ( nb )
          {
          for ( unsigned int kk = 0; kk < InputMeshType::PointDimension; ++kk )
            {
            opt[kk] = 0.75 * ipt[kk] + 0.125 * bpt[kk];
            }
          }
        else
          {
          InputCoordType var  = 0.375 + 0.25 * std::cos(2.0 * itk::Math::pi / nn);
          InputCoordType beta = ( 0.625 - var * var ) / nn;
          for ( unsigned int kk = 0; kk < InputMeshType::PointDimension; ++kk )
            {
            opt[kk] = ( 1.0 - nn * beta ) * ipt[kk] + beta * opt[kk];
            }
          }
        OutputPointType outpoint;
        outpoint.CastFrom( opt );
        output->SetPoint( ptIt->Index(), outpoint );
        */
        OutputPointType outpoint;
        outpoint.CastFrom(this->SmoothingPoint(ptIt->Value(), points));
        output->SetPoint(ptIt->Index(), outpoint);
      }
      else
      {
        OutputPointType outpoint;
        outpoint.CastFrom(ipt);
        output->SetPoint(ptIt->Index(), outpoint);
      }
      ++ptIt;
    }
  }
}

template <typename TInputMesh, typename TOutputMesh>
typename LoopTriangleCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TOutputMesh>::InputPointType
LoopTriangleCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TOutputMesh>::SmoothingPoint(
  const InputPointType &       ipt,
  const InputPointsContainer * points)
{
  InputPointType opt;
  opt.Fill(NumericTraits<typename InputPointType::ValueType>::Zero);
  unsigned int nn = 0;

  InputPointType bpt;
  bpt.Fill(NumericTraits<typename InputPointType::ValueType>::Zero);
  unsigned int nb = 0;

  InputQEType *                      edge = ipt.GetEdge();
  typename InputQEType::IteratorGeom q_it = edge->BeginGeomOnext();
  while (q_it != edge->EndGeomOnext())
  {
    if (q_it.Value()->IsAtBorder())
    {
      bpt += points->ElementAt(q_it.Value()->GetDestination()).GetVectorFromOrigin();
      ++nb;
    }

    opt += points->ElementAt(q_it.Value()->GetDestination()).GetVectorFromOrigin();
    ++nn;
    ++q_it;
  }

  if (nb)
  {
    for (unsigned int kk = 0; kk < InputMeshType::PointDimension; ++kk)
    {
      opt[kk] = 0.75 * ipt[kk] + 0.125 * bpt[kk];
    }
  }
  else
  {
    InputCoordType var = 0.375 + 0.25 * std::cos(2.0 * itk::Math::pi / nn);
    InputCoordType beta = (0.625 - var * var) / nn;
    for (unsigned int kk = 0; kk < InputMeshType::PointDimension; ++kk)
    {
      opt[kk] = (1.0 - nn * beta) * ipt[kk] + beta * opt[kk];
    }
  }

  return opt;
}
} // namespace itk
#endif
