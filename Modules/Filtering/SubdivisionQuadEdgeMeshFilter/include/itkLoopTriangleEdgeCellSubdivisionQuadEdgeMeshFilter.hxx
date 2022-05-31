/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkLoopTriangleEdgeCellSubdivisionQuadEdgeMeshFilter_hxx
#define itkLoopTriangleEdgeCellSubdivisionQuadEdgeMeshFilter_hxx

#include "itkMath.h"
#include <algorithm>

namespace itk
{
template <typename TInputMesh, typename TOutputMesh>
void
LoopTriangleEdgeCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TOutputMesh>::AddNewEdgePoints(InputQEType * edge)
{
  const InputMeshType * input = this->GetInput();
  OutputMeshType *      output = this->GetOutput();

  if (edge->IsInternal())
  {
    InputCoordType pointWeight[4] = { 0.375, 0.375, 0.125, 0.125 };
    InputPointType pointArray[4];

    InputPointType newPoint;
    newPoint.Fill(NumericTraits<typename InputPointType::ValueType>::Zero);

    input->GetPoint(edge->GetOrigin(), &pointArray[0]);
    input->GetPoint(edge->GetDestination(), &pointArray[1]);

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

    OutputPointIdentifier numberOfPoints = output->GetNumberOfPoints();
    this->m_EdgesPointIdentifier->InsertElement(edge, numberOfPoints);
    this->m_EdgesPointIdentifier->InsertElement(edge->GetSym(), numberOfPoints);

    OutputPointType outPoint;
    outPoint.CastFrom(newPoint);
    output->SetPoint(numberOfPoints, outPoint);
  }
  else if (edge->IsAtBorder())
  {
    InputPointType pointArray[2];

    input->GetPoint(edge->GetOrigin(), &pointArray[0]);
    input->GetPoint(edge->GetDestination(), &pointArray[1]);

    InputPointType newPoint;
    newPoint.SetToMidPoint(pointArray[0], pointArray[1]);

    OutputPointIdentifier numberOfPoints = output->GetNumberOfPoints();
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

  return;
}

template <typename TInputMesh, typename TOutputMesh>
void
LoopTriangleEdgeCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TOutputMesh>::CopyInputMeshToOutputMeshPoints()
{
  const InputMeshType * input = this->GetInput();
  OutputMeshType *      output = this->GetOutput();

  const InputPointsContainer * points = input->GetPoints();
  output->GetPoints()->Reserve(input->GetNumberOfPoints());

  InputPointsContainerConstIterator ptIt = points->Begin();
  while (ptIt != points->End())
  {
    InputQEType * edge = ptIt->Value().GetEdge();
    InputQEType * qter0 = edge;
    InputQEType * qter1 = edge->GetOnext();

    bool copyFlag = true;
    while (qter1 != edge)
    {
      if (std::find(this->m_EdgesToBeSubdivided.begin(), this->m_EdgesToBeSubdivided.end(), qter0) !=
          this->m_EdgesToBeSubdivided.end())
      {
        break;
        copyFlag = false;
      }

      qter0 = qter1;
      qter1 = qter0->GetOnext();
    }

    if (copyFlag)
    {
      OutputPointType outpoint;
      outpoint.CastFrom(ptIt->Value());
      this->GetOutput()->SetPoint(ptIt->Index(), outpoint);
    }
    else
    {
      this->AverageOriginOfEdge(edge, points);
    }
    ++ptIt;
  }
}

template <typename TInputMesh, typename TOutputMesh>
void
LoopTriangleEdgeCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TOutputMesh>::AverageOriginOfEdge(
  InputQEType *                edge,
  const InputPointsContainer * points)
{
  InputPointType opt;
  opt.Fill(NumericTraits<typename InputPointType::ValueType>::Zero);

  InputPointType bpt;
  bpt.Fill(NumericTraits<typename InputPointType::ValueType>::Zero);

  InputPointType ipt = points->ElementAt(edge->GetOrigin());

  unsigned int nb = 0;
  unsigned int nn = 0;

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

  OutputPointType outpoint;
  outpoint.CastFrom(opt);
  this->GetOutput()->SetPoint(edge->GetOrigin(), outpoint);
}

} // namespace itk
#endif
