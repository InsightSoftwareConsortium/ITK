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
#ifndef itkQuadricDecimationQuadEdgeMeshFilter_hxx
#define itkQuadricDecimationQuadEdgeMeshFilter_hxx

#include "itkQuadricDecimationQuadEdgeMeshFilter.h"

namespace itk
{
template< typename TInput, typename TOutput, typename TCriterion >
QuadricDecimationQuadEdgeMeshFilter< TInput, TOutput, TCriterion >
::QuadricDecimationQuadEdgeMeshFilter()
{}

template< typename TInput, typename TOutput, typename TCriterion >
QuadricDecimationQuadEdgeMeshFilter< TInput, TOutput, TCriterion >
::~QuadricDecimationQuadEdgeMeshFilter()
{}


template< typename TInput, typename TOutput, typename TCriterion >
void
QuadricDecimationQuadEdgeMeshFilter< TInput, TOutput, TCriterion >
::Initialize()
{
  OutputMeshPointer             output = this->GetOutput();
  OutputPointsContainerPointer  points = output->GetPoints();
  OutputPointsContainerIterator it = points->Begin();
  OutputPointIdentifier         p_id;
  OutputQEType *                qe;
  OutputQEType *                qe_it;

  OutputMeshType *outputMesh = this->GetOutput();
  while ( it != points->End() )
    {
    p_id = it->Index();

    qe = output->FindEdge(p_id);
    if ( qe != ITK_NULLPTR )
      {
      qe_it = qe;
      do
        {
        QuadricAtOrigin(qe_it, m_Quadric[p_id], outputMesh);
        qe_it = qe_it->GetOnext();
        }
      while ( qe_it != qe );
      }
    ++it;
    }
}

template< typename TInput, typename TOutput, typename TCriterion >
void
QuadricDecimationQuadEdgeMeshFilter< TInput, TOutput, TCriterion >
::DeletePoint( const OutputPointIdentifier & iIdToBeDeleted,
               const OutputPointIdentifier & iRemaining)
{
  Superclass::DeletePoint(iIdToBeDeleted, iRemaining);

  QuadricElementMapIterator it = m_Quadric.find(iIdToBeDeleted);
  m_Quadric[iRemaining] += it->second;
  m_Quadric.erase(it);
}

template< typename TInput, typename TOutput, typename TCriterion >
typename
QuadricDecimationQuadEdgeMeshFilter< TInput, TOutput, TCriterion >
::OutputPointType
QuadricDecimationQuadEdgeMeshFilter< TInput, TOutput, TCriterion >
::Relocate(OutputQEType *iEdge)
{
  OutputPointIdentifier id_org = iEdge->GetOrigin();
  OutputPointIdentifier id_dest = iEdge->GetDestination();
  QuadricElementType    Q = m_Quadric[id_org] + m_Quadric[id_dest];

  OutputMeshPointer output = this->GetOutput();

  OutputPointType org = output->GetPoint(id_org);
  OutputPointType dest = output->GetPoint(id_dest);

  OutputPointType mid;

  mid.SetToMidPoint(org, dest);

  return Q.ComputeOptimalLocation(mid);
}
}
#endif
