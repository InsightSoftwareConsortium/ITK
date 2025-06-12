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
#ifndef itkQuadricDecimationQuadEdgeMeshFilter_hxx
#define itkQuadricDecimationQuadEdgeMeshFilter_hxx


namespace itk
{

template <typename TInput, typename TOutput, typename TCriterion>
void
QuadricDecimationQuadEdgeMeshFilter<TInput, TOutput, TCriterion>::Initialize()
{
  const OutputMeshPointer            output = this->GetOutput();
  const OutputPointsContainerPointer points = output->GetPoints();
  OutputPointsContainerIterator      it = points->Begin();

  OutputMeshType * outputMesh = this->GetOutput();
  while (it != points->End())
  {
    OutputPointIdentifier p_id = it->Index();

    OutputQEType * qe = output->FindEdge(p_id);
    if (qe != nullptr)
    {
      OutputQEType * qe_it = qe;
      do
      {
        QuadricAtOrigin(qe_it, m_Quadric[p_id], outputMesh);
        qe_it = qe_it->GetOnext();
      } while (qe_it != qe);
    }
    ++it;
  }
}

template <typename TInput, typename TOutput, typename TCriterion>
void
QuadricDecimationQuadEdgeMeshFilter<TInput, TOutput, TCriterion>::DeletePoint(
  const OutputPointIdentifier & iIdToBeDeleted,
  const OutputPointIdentifier & iRemaining)
{
  Superclass::DeletePoint(iIdToBeDeleted, iRemaining);

  auto it = m_Quadric.find(iIdToBeDeleted);
  m_Quadric[iRemaining] += it->second;
  m_Quadric.erase(it);
}

template <typename TInput, typename TOutput, typename TCriterion>
auto
QuadricDecimationQuadEdgeMeshFilter<TInput, TOutput, TCriterion>::Relocate(OutputQEType * iEdge) -> OutputPointType
{
  const OutputPointIdentifier id_org = iEdge->GetOrigin();
  const OutputPointIdentifier id_dest = iEdge->GetDestination();
  QuadricElementType          Q = m_Quadric[id_org] + m_Quadric[id_dest];

  const OutputMeshPointer output = this->GetOutput();

  const OutputPointType org = output->GetPoint(id_org);
  const OutputPointType dest = output->GetPoint(id_dest);

  OutputPointType mid;

  mid.SetToMidPoint(org, dest);

  return Q.ComputeOptimalLocation(mid);
}
} // namespace itk
#endif
