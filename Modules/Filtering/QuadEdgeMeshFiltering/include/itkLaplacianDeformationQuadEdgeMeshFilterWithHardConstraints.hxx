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
#ifndef itkLaplacianDeformationQuadEdgeMeshFilterWithHardConstraints_hxx
#define itkLaplacianDeformationQuadEdgeMeshFilterWithHardConstraints_hxx

#include "itkLaplacianDeformationQuadEdgeMeshFilterWithHardConstraints.h"

namespace itk
{
template< typename TInputMesh, typename TOutputMesh, typename TSolverTraits >
LaplacianDeformationQuadEdgeMeshFilterWithHardConstraints< TInputMesh, TOutputMesh, TSolverTraits >
::LaplacianDeformationQuadEdgeMeshFilterWithHardConstraints()
{}

template< typename TInputMesh, typename TOutputMesh, typename TSolverTraits >
void
LaplacianDeformationQuadEdgeMeshFilterWithHardConstraints< TInputMesh, TOutputMesh, TSolverTraits >
::ComputeVertexIdMapping()
{
  OutputMeshType* output   = this->GetOutput();

  typename OutputMeshType::PointsContainer* points = output->GetPoints();

  typedef typename OutputMeshType::PointsContainerIterator PointsContainerIterator;
  PointsContainerIterator pIt = points->Begin();
  const PointsContainerIterator pEnd = points->End();

  OutputPointIdentifier k = 0;

  while ( pIt != pEnd )
    {
    const OutputPointIdentifier vId = pIt->Index();

    if ( this->m_Constraints.find(vId) == this->m_Constraints.end() )
      {
      this->m_InternalMap.insert( typename OutputMapPointIdentifier::value_type(vId, k++) );
      }
    ++pIt;
    }
}

template< typename TInputMesh, typename TOutputMesh, typename TSolverTraits >
void
LaplacianDeformationQuadEdgeMeshFilterWithHardConstraints< TInputMesh, TOutputMesh, TSolverTraits >
::FillMatrix(MatrixType & iM, VectorType & iBx, VectorType & iBy, VectorType & iBz)
{
  OutputMapPointIdentifierConstIterator it = this->m_InternalMap.begin();
  const OutputMapPointIdentifierConstIterator end = this->m_InternalMap.end();

  while ( it != end )
    {
    const OutputPointIdentifier vId1 = it->first;
    const unsigned int internalId1 = static_cast< unsigned int >( it->second );

    RowType row;
    this->FillMatrixRow(vId1, this->m_Order, NumericTraits< OutputCoordRepType >::OneValue(), row);

    RowConstIterator rIt = row.begin();
    const RowConstIterator rEnd = row.end();

    while( rIt != rEnd )
      {
      const OutputPointIdentifier vId2 = rIt->first;
      const OutputCoordRepType weight = rIt->second;

      ConstraintMapConstIterator cIt = this->m_Constraints.find(vId2);
      if ( cIt != this->m_Constraints.end() )
        {
        iBx[internalId1] -= weight * ( cIt->second )[0];
        iBy[internalId1] -= weight * ( cIt->second )[1];
        iBz[internalId1] -= weight * ( cIt->second )[2];
        }
      else
        {
        const unsigned int internalId2 = static_cast< unsigned int >( this->m_InternalMap[vId2] );
        SolverTraits::AddToMatrix(iM, internalId1, internalId2, weight);
        }
      ++rIt;
      }
    ++it;
    }
}

template< typename TInputMesh, typename TOutputMesh, typename TSolverTraits >
void
LaplacianDeformationQuadEdgeMeshFilterWithHardConstraints< TInputMesh, TOutputMesh, TSolverTraits >
::GenerateData()
{
  this->CopyInputMeshToOutputMesh();

  if ( !this->m_Constraints.empty() )
    {
    OutputMeshType* output = this->GetOutput();

    this->m_CoefficientMap.clear();
    this->m_MixedAreaMap.clear();

    this->ComputeVertexIdMapping();

    const unsigned int N = static_cast< unsigned int >( this->m_InternalMap.size() );

    MatrixType M = SolverTraits::InitializeSparseMatrix(N, N);

    VectorType Bx = SolverTraits::InitializeVector(N);
    Bx.fill(0.);

    VectorType By = SolverTraits::InitializeVector(N);
    By.fill(0.);

    VectorType Bz = SolverTraits::InitializeVector(N);
    Bz.fill(0.);

    this->FillMatrix(M, Bx, By, Bz);

    VectorType X = SolverTraits::InitializeVector(N);
    X.fill(0.);

    VectorType Y = SolverTraits::InitializeVector(N);
    Y.fill(0.);

    VectorType Z = SolverTraits::InitializeVector(N);
    Z.fill(0.);

    this->SolveLinearSystems(M, Bx, By, Bz, X, Y, Z);

    typename OutputMeshType::PointsContainer*  points = output->GetPoints();

    OutputMapPointIdentifierConstIterator it = this->m_InternalMap.begin();
    const OutputMapPointIdentifierConstIterator end = this->m_InternalMap.end();

    while ( it != end )
      {
      const OutputPointIdentifier vId = it->first;
      const unsigned int internalId = static_cast< unsigned int >( it->second );

      OutputPointType& p = points->ElementAt(vId);

      OutputCoordRepType dx = static_cast< OutputCoordRepType >( X[internalId] );
      p[0] += dx;

      OutputCoordRepType dy = static_cast< OutputCoordRepType >( Y[internalId] );
      p[1] += dy;

      OutputCoordRepType dz = static_cast< OutputCoordRepType >( Z[internalId] );
      p[2] += dz;

      ++it;
      }

    ConstraintMapConstIterator cIt  = this->m_Constraints.begin();
    const ConstraintMapConstIterator cEnd = this->m_Constraints.end();

    while( cIt != cEnd )
      {
      const OutputPointIdentifier vId = cIt->first;
      OutputPointType& p = points->ElementAt(vId);
      p += cIt->second;
      ++cIt;
      }
    }
}

template< typename TInputMesh, typename TOutputMesh, typename TSolverTraits >
void
LaplacianDeformationQuadEdgeMeshFilterWithHardConstraints< TInputMesh, TOutputMesh, TSolverTraits >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}
} // end namespace itk

#endif
