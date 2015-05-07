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
#ifndef itkLaplacianDeformationQuadEdgeMeshFilterWithSoftConstraints_hxx
#define itkLaplacianDeformationQuadEdgeMeshFilterWithSoftConstraints_hxx

#include "itkLaplacianDeformationQuadEdgeMeshFilterWithSoftConstraints.h"

namespace itk
{

template< typename TInputMesh, typename TOutputMesh, typename TSolverTraits >
LaplacianDeformationQuadEdgeMeshFilterWithSoftConstraints< TInputMesh, TOutputMesh, TSolverTraits >
::LaplacianDeformationQuadEdgeMeshFilterWithSoftConstraints(): m_Lambda(1), m_LambdaSquare(1)
{}

template< typename TInputMesh, typename TOutputMesh, typename TSolverTraits >
void
LaplacianDeformationQuadEdgeMeshFilterWithSoftConstraints< TInputMesh, TOutputMesh, TSolverTraits >
::ComputeVertexIdMapping()
{
  OutputMeshType* output   = this->GetOutput();

  typename OutputMeshType::PointsContainer* points = output->GetPoints();

  typename OutputMeshType::PointsContainerIterator pIt = points->Begin();
  const typename OutputMeshType::PointsContainerIterator pEnd = points->End();

  OutputPointIdentifier k = 0;

  while ( pIt != pEnd )
    {
    this->m_InternalMap.insert( typename OutputMapPointIdentifier::value_type(pIt->Index(), k++) );
    ++pIt;
    }
}

template< typename TInputMesh, typename TOutputMesh, typename TSolverTraits >
void
LaplacianDeformationQuadEdgeMeshFilterWithSoftConstraints< TInputMesh, TOutputMesh, TSolverTraits >
::SetLocalLambda( OutputPointIdentifier vId, OutputCoordRepType iL )
{
  m_LocalLambdaSquare[ vId ] = iL * iL;
}


template< typename TInputMesh, typename TOutputMesh, typename TSolverTraits >
void
LaplacianDeformationQuadEdgeMeshFilterWithSoftConstraints< TInputMesh, TOutputMesh, TSolverTraits >
::FillMatrix(MatrixType & iM, VectorType & iBx, VectorType & iBy, VectorType & iBz)
{
  OutputMeshType* output = this->GetOutput();

  OutputMapPointIdentifierConstIterator it = this->m_InternalMap.begin();
  OutputMapPointIdentifierConstIterator end = this->m_InternalMap.end();

  while ( it != end )
    {
    const OutputPointIdentifier vId1 = it->first;
    const unsigned int internalId1 = static_cast< unsigned int >( it->second );

    RowType row;
    this->FillMatrixRow(vId1, this->m_Order, NumericTraits< OutputCoordRepType >::OneValue(), row);

    RowConstIterator rIt = row.begin();
    RowConstIterator rEnd = row.end();

    while( rIt != rEnd )
      {
      const OutputPointIdentifier vId2 = rIt->first;
      const OutputCoordRepType weight = rIt->second;

      const OutputPointType p = output->GetPoint( vId2 );
      iBx[internalId1] += weight * p[0];
      iBy[internalId1] += weight * p[1];
      iBz[internalId1] += weight * p[2];

      const unsigned int internalId2 = static_cast< unsigned int >( this->m_InternalMap[vId2] );
      SolverTraits::FillMatrix(iM, internalId1, internalId2, weight);

      ++rIt;
      }
    ++it;
    }
}

template< typename TInputMesh, typename TOutputMesh, typename TSolverTraits >
void
LaplacianDeformationQuadEdgeMeshFilterWithSoftConstraints< TInputMesh, TOutputMesh, TSolverTraits >
::GenerateData()
{
  this->m_LambdaSquare = this->m_Lambda * this->m_Lambda;

  this->CopyInputMeshToOutputMesh();

  if( !this->m_Constraints.empty() )
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

    MatrixType Mt( M.transpose() );

    MatrixType A( Mt * M );

    VectorType Cx, Cy, Cz;
    Mt.mult( Bx, Cx );
    Mt.mult( By, Cy );
    Mt.mult( Bz, Cz );

    typename OutputMeshType::PointsContainer* points = output->GetPoints();

    for ( ConstraintMapConstIterator cIt = this->m_Constraints.begin();
          cIt != this->m_Constraints.end();
          ++cIt )
      {
      const OutputPointIdentifier vId = cIt->first;
      OutputPointType p = points->GetElement(vId);
      p += cIt->second;

      const OutputPointIdentifier index = this->m_InternalMap[ vId ];

      OutputCoordRepType l2 = m_LambdaSquare;

      typename itksys::hash_map< OutputPointIdentifier, OutputCoordRepType >::const_iterator lambdaIt = this->m_LocalLambdaSquare.find( vId );
      if( lambdaIt != this->m_LocalLambdaSquare.end() )
        {
        l2 = lambdaIt->second;
        }

      Cx[ index ] += l2 * p[0];
      Cy[ index ] += l2 * p[1];
      Cz[ index ] += l2 * p[2];

      SolverTraits::AddToMatrix( A, index, index, l2 );
      }

    VectorType X = SolverTraits::InitializeVector(N);
    X.fill(0.);

    VectorType Y = SolverTraits::InitializeVector(N);
    Y.fill(0.);

    VectorType Z = SolverTraits::InitializeVector(N);
    Z.fill(0.);

    this->SolveLinearSystems(A, Cx, Cy, Cz, X, Y, Z);

    OutputMapPointIdentifierConstIterator it  = this->m_InternalMap.begin();
    OutputMapPointIdentifierConstIterator end = this->m_InternalMap.end();

    while ( it != end )
      {
      const OutputPointIdentifier vId = it->first;
      const unsigned int internalId = static_cast< unsigned int >( it->second );

      OutputPointType& p = points->ElementAt(vId);

      OutputCoordRepType x = static_cast< OutputCoordRepType >( X[internalId] );
      p[0] = x;

      OutputCoordRepType y = static_cast< OutputCoordRepType >( Y[internalId] );
      p[1] = y;

      OutputCoordRepType z = static_cast< OutputCoordRepType >( Z[internalId] );
      p[2] = z;

      ++it;
      }
    }
}

template< typename TInputMesh, typename TOutputMesh, typename TSolverTraits >
void
LaplacianDeformationQuadEdgeMeshFilterWithSoftConstraints< TInputMesh, TOutputMesh, TSolverTraits >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}
} // end namespace itk

#endif
