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
#ifndef itkParameterizationQuadEdgeMeshFilter_hxx
#define itkParameterizationQuadEdgeMeshFilter_hxx

#include "itkParameterizationQuadEdgeMeshFilter.h"

namespace itk
{
// ---------------------------------------------------------------------
template< typename TInputMesh, typename TOutputMesh, typename TSolverTraits >
ParameterizationQuadEdgeMeshFilter< TInputMesh, TOutputMesh, TSolverTraits >
::ParameterizationQuadEdgeMeshFilter()
{
  this->m_CoefficientsMethod = ITK_NULLPTR;
  this->m_BorderTransform = ITK_NULLPTR;
}

// ---------------------------------------------------------------------
template< typename TInputMesh, typename TOutputMesh, typename TSolverTraits >
void
ParameterizationQuadEdgeMeshFilter< TInputMesh, TOutputMesh, TSolverTraits >
::CopyToOutputBorder()
{
  OutputMeshType *output = this->GetOutput();

  for ( InputMapPointIdentifierIterator it = m_BoundaryPtMap.begin();
        it != m_BoundaryPtMap.end();
        ++it )
    {
    output->SetPoint(it->first, m_Border[it->second]);
    }
}

// ---------------------------------------------------------------------
template< typename TInputMesh, typename TOutputMesh, typename TSolverTraits >
void
ParameterizationQuadEdgeMeshFilter< TInputMesh, TOutputMesh, TSolverTraits >
::ComputeListOfInteriorVertices()
{
  InputMeshConstPointer input = this->GetInput();

  typename InputPointsContainer::ConstPointer points = input->GetPoints();

  InputPointIdentifier k(0);
  InputPointIdentifier id(0);

  for ( InputPointsContainerConstIterator it = points->Begin();
        it != points->End();
        ++it )
    {
    id = it->Index();

    // if the id point is not in m_BoundaryPtMap
    // add this id point to m_InternalPtMap
    if ( m_BoundaryPtMap.find(id) == m_BoundaryPtMap.end() )
      {
      m_InternalPtMap[id] = k++;
      }
    }
}

// ---------------------------------------------------------------------
template< typename TInputMesh, typename TOutputMesh, typename TSolverTraits >
void
ParameterizationQuadEdgeMeshFilter< TInputMesh, TOutputMesh, TSolverTraits >
::SolveLinearSystems(const MatrixType & iM, const VectorType & iBx,
                     const VectorType & iBy, VectorType & oX, VectorType & oY)
{
  SolverTraits::Solve(iM, iBx, iBy, oX, oY);
}

// ---------------------------------------------------------------------
template< typename TInputMesh, typename TOutputMesh, typename TSolverTraits >
void
ParameterizationQuadEdgeMeshFilter< TInputMesh, TOutputMesh, TSolverTraits >
::FillMatrix(MatrixType & iM, VectorType & ioBx, VectorType & ioBy)
{
  InputMeshConstPointer input   = this->GetInput();
  OutputMeshPointer     output = this->GetOutput();

  InputCoordRepType value;

  InputMapPointIdentifierIterator it;

  InputPointIdentifier id1, id2;
  InputPointIdentifier InternalId1, InternalId2;

  OutputPointType pt2;

  ValueType k[2];

  for ( InputMapPointIdentifierIterator
        InternalPtIterator = m_InternalPtMap.begin();
        InternalPtIterator != m_InternalPtMap.end();
        ++InternalPtIterator )
    {
    id1 = InternalPtIterator->first;
    InternalId1 = InternalPtIterator->second;

    k[0] = 0.;
    k[1] = 0.;

    InputQEType *edge = input->FindEdge(id1);
    InputQEType *temp = edge;

    do
      {
      id2 = temp->GetDestination();

      it = m_BoundaryPtMap.find(id2);

      if ( it != m_BoundaryPtMap.end() )
        {
        value = ( *m_CoefficientsMethod )( input, temp );
        pt2 = output->GetPoint(it->first);
        SolverTraits::AddToMatrix(iM, InternalId1, InternalId1, value);
        k[0] += static_cast< ValueType >( pt2[0] * value );
        k[1] += static_cast< ValueType >( pt2[1] * value );
        }
      else
        {
        InternalId2 = m_InternalPtMap[id2];
        if ( InternalId1 < InternalId2 )
          {
          value = ( *m_CoefficientsMethod )( input, temp );
          SolverTraits::FillMatrix(iM, InternalId1, InternalId2, -value);
          SolverTraits::FillMatrix(iM, InternalId2, InternalId1, -value);
          SolverTraits::AddToMatrix(iM, InternalId1, InternalId1, value);
          SolverTraits::AddToMatrix(iM, InternalId2, InternalId2, value);
          }
        }

      temp = temp->GetOnext();
      }
    while ( temp != edge );

    ioBx[InternalId1] = k[0];
    ioBy[InternalId1] = k[1];
    }
}

// ---------------------------------------------------------------------
template< typename TInputMesh, typename TOutputMesh, typename TSolverTraits >
void
ParameterizationQuadEdgeMeshFilter< TInputMesh, TOutputMesh, TSolverTraits >
::GenerateData()
{
  this->CopyInputMeshToOutputMesh();

  InputMeshConstPointer input = this->GetInput();
  OutputMeshType *      output = this->GetOutput();

  if ( m_BorderTransform.IsNotNull() )
    {
    m_BorderTransform->Update();
    m_Border = m_BorderTransform->GetBorder();
    m_BoundaryPtMap = m_BorderTransform->GetBoundaryPtMap();
    }

  itkAssertOrThrowMacro( ( ( m_BoundaryPtMap.size() > 2 ) && ( m_Border.size() > 2 ) ),
                         "BoundaryPtMap and Border must both have greater than 2 elements." );

  this->CopyToOutputBorder();

  this->ComputeListOfInteriorVertices();

  InputPointIdentifier NbOfInteriorPts = static_cast<InputPointIdentifier>( m_InternalPtMap.size() );

  MatrixType Matrix = SolverTraits::InitializeSparseMatrix(NbOfInteriorPts);
  VectorType Bx = SolverTraits::InitializeVector(NbOfInteriorPts);
  VectorType By = SolverTraits::InitializeVector(NbOfInteriorPts);
  VectorType X = SolverTraits::InitializeVector(NbOfInteriorPts);
  VectorType Y = SolverTraits::InitializeVector(NbOfInteriorPts);

  FillMatrix(Matrix, Bx, By);

  SolveLinearSystems(Matrix, Bx, By, X, Y);

  OutputPointType OutputPt;

  for ( InputMapPointIdentifierIterator PtIterator = m_InternalPtMap.begin();
        PtIterator != m_InternalPtMap.end();
        ++PtIterator )
    {
    OutputPointIdentifier id = static_cast< OutputPointIdentifier >( PtIterator->first );
    InputPointIdentifier  InternalId = PtIterator->second;

    OutputPt[0] = X[InternalId];
    OutputPt[1] = Y[InternalId];
    OutputPt[2] = 0.;

    output->SetPoint(id, OutputPt);
    }
}

template< typename TInputMesh, typename TOutputMesh, typename TSolverTraits >
void
ParameterizationQuadEdgeMeshFilter< TInputMesh, TOutputMesh, TSolverTraits >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "BorderTransform: " << m_BorderTransform << std::endl;
  os << indent << "CoefficientsMethod: " << m_CoefficientsMethod << std::endl;

}
} // end namespace itk

#endif
