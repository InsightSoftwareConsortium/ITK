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
#ifndef itkModifiedButterflyTriangleEdgeCellSubdivisionQuadEdgeMeshFilter_hxx
#define itkModifiedButterflyTriangleEdgeCellSubdivisionQuadEdgeMeshFilter_hxx


namespace itk
{
template <typename TInputMesh, typename TOutputMesh>
void
ModifiedButterflyTriangleEdgeCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TOutputMesh>::AddNewEdgePoints(
  InputQEType * edge)
{
  const InputMeshType * input = this->GetInput();
  OutputMeshType *      output = this->GetOutput();

  InputCoordType pointWeight[8] = { 0.5, 0.5, 0.125, 0.125, -0.0625, -0.0625, -0.0625, -0.0625 };
  InputPointType pointArray[8];

  InputPointType newPoint;
  newPoint.Fill(NumericTraits<typename InputPointType::ValueType>::Zero);

  input->GetPoint(edge->GetOrigin(), &pointArray[0]);
  input->GetPoint(edge->GetDestination(), &pointArray[1]);

  if (edge->GetLnext())
  {
    InputPointIdentifier ptId = edge->GetLnext()->GetDestination();
    input->GetPoint(ptId, &pointArray[2]);

    if (edge->GetLnext()->GetRprev())
    {
      ptId = edge->GetLnext()->GetRprev()->GetDestination();
      input->GetPoint(ptId, &pointArray[4]);
    }
    else
    {
      pointArray[4].Fill(NumericTraits<typename InputPointType::ValueType>::Zero);
    }
  }
  else
  {
    pointArray[2].Fill(NumericTraits<typename InputPointType::ValueType>::Zero);
    pointArray[4].Fill(NumericTraits<typename InputPointType::ValueType>::Zero);
  }

  if (edge->GetRprev())
  {
    InputPointIdentifier ptId = edge->GetRprev()->GetDestination();
    input->GetPoint(ptId, &pointArray[3]);
    if (edge->GetRprev()->GetLnext())
    {
      ptId = edge->GetRprev()->GetLnext()->GetDestination();
      input->GetPoint(ptId, &pointArray[5]);
    }
    else
    {
      pointArray[5].Fill(NumericTraits<typename InputPointType::ValueType>::Zero);
    }
  }
  else
  {
    pointArray[3].Fill(NumericTraits<typename InputPointType::ValueType>::Zero);
    pointArray[5].Fill(NumericTraits<typename InputPointType::ValueType>::Zero);
  }

  if (edge->GetLprev() && edge->GetLprev()->GetRprev())
  {
    InputPointIdentifier ptId = edge->GetLprev()->GetRprev()->GetDestination();
    input->GetPoint(ptId, &pointArray[6]);
  }
  else
  {
    pointArray[6].Fill(NumericTraits<typename InputPointType::ValueType>::Zero);
  }

  if (edge->GetRnext() && edge->GetRnext()->GetLnext())
  {
    InputPointIdentifier ptId = edge->GetRnext()->GetLnext()->GetDestination();
    input->GetPoint(ptId, &pointArray[7]);
  }
  else
  {
    pointArray[7].Fill(NumericTraits<typename InputPointType::ValueType>::Zero);
  }

  for (unsigned int kk = 0; kk < InputMeshType::PointDimension; ++kk)
  {
    for (unsigned int mm = 0; mm < 8; ++mm)
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

  return;
}
} // namespace itk
#endif
