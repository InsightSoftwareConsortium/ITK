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
#ifndef itkLinearTriangleEdgeCellSubdivisionQuadEdgeMeshFilter_hxx
#define itkLinearTriangleEdgeCellSubdivisionQuadEdgeMeshFilter_hxx


namespace itk
{
template <typename TInputMesh, typename TOutputMesh>
void
LinearTriangleEdgeCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TOutputMesh>::AddNewEdgePoints(InputQEType * edge)
{
  OutputMeshType * output = this->GetOutput();

  InputPointType pointArray[3];
  this->GetInput()->GetPoint(edge->GetOrigin(), &pointArray[0]);
  this->GetInput()->GetPoint(edge->GetDestination(), &pointArray[1]);
  pointArray[2].SetToMidPoint(pointArray[0], pointArray[1]);

  OutputPointType outpoint;
  outpoint.CastFrom(pointArray[2]);

  OutputPointIdentifier numberOfPoints = output->GetNumberOfPoints();

  this->m_EdgesPointIdentifier->InsertElement(edge, numberOfPoints);
  this->m_EdgesPointIdentifier->InsertElement(edge->GetSym(), numberOfPoints);
  output->SetPoint(numberOfPoints, outpoint);

  return;
}
} // namespace itk
#endif
