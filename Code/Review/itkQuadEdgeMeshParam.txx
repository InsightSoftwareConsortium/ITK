/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshParam.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkQuadEdgeMeshParam_txx
#define __itkQuadEdgeMeshParam_txx

#include "itkQuadEdgeMeshParam.h"

namespace itk
{

// ---------------------------------------------------------------------
template< typename TInputMesh, typename TOutputMesh, typename TSolverTraits >
QuadEdgeMeshParam< TInputMesh, TOutputMesh, TSolverTraits >
::QuadEdgeMeshParam()
{
  this->m_CoefficientsMethod = 0;
  this->m_BorderTransform = 0;
}

// ---------------------------------------------------------------------
template< typename TInputMesh, typename TOutputMesh, typename TSolverTraits >
void
QuadEdgeMeshParam< TInputMesh, TOutputMesh, TSolverTraits >
::CopyToOutputBorder( )
{
  OutputMeshType* output = this->GetOutput( );

  for( InputMapPoinIdentifierIterator it = m_BoundaryPtMap.begin( );
       it != m_BoundaryPtMap.end( );
       ++it )
    {
    output->SetPoint( it->first, m_Border[it->second] );
    }
}

// ---------------------------------------------------------------------
template< typename TInputMesh, typename TOutputMesh, typename TSolverTraits >
void
QuadEdgeMeshParam< TInputMesh, TOutputMesh, TSolverTraits >
::ComputeListOfInteriorVertices( )
{
  InputMeshConstPointer input = this->GetInput( );

  typename InputPointsContainer::ConstPointer points = input->GetPoints( );

  InputPointIdentifier k( 0 );
  InputPointIdentifier id( 0 );

  for( InputPointsContainerConstIterator it = points->Begin( );
        it != points->End( );
        ++it )
    {
    id = it->Index( );

    // if the id point is not in m_BoundaryPtMap
    // add this id point to m_InternalPtMap
    if( m_BoundaryPtMap.find( id ) == m_BoundaryPtMap.end( ) )
      {
      m_InternalPtMap[id] = k++;
      }
    }
}

// ---------------------------------------------------------------------
template< typename TInputMesh, typename TOutputMesh, typename TSolverTraits >
void
QuadEdgeMeshParam< TInputMesh, TOutputMesh, TSolverTraits >
::SolveLinearSystems( const MatrixType& iM, const VectorType& iBx,
       const VectorType& iBy, VectorType& oX, VectorType& oY )
{
  SolverTraits traits;
  traits.Solve( iM, iBx, iBy, oX, oY );
}

// ---------------------------------------------------------------------
template< typename TInputMesh, typename TOutputMesh, typename TSolverTraits >
void
QuadEdgeMeshParam< TInputMesh, TOutputMesh, TSolverTraits >
::FillMatrix( MatrixType& iM, VectorType& ioBx, VectorType& ioBy )
{
  InputMeshConstPointer input   = this->GetInput( );
  OutputMeshPointer output = this->GetOutput( );

  SolverTraits traits;

  InputCoordRepType value;

  InputMapPoinIdentifierIterator it;

  InputPointIdentifier id1, id2;
  InputPointIdentifier InternalId1, InternalId2;

  OutputPointType pt2;

  ValueType k[2];

  for( InputMapPoinIdentifierIterator
       InternalPtIterator = m_InternalPtMap.begin( );
       InternalPtIterator != m_InternalPtMap.end( );
       ++InternalPtIterator )
    {
    id1 = InternalPtIterator->first;
    InternalId1 = InternalPtIterator->second;

    k[0] = 0.;
    k[1] = 0.;

    InputQEType* edge = input->FindEdge( id1 );
    InputQEType* temp = edge;

    do
      {
      id2 = temp->GetDestination( );

      it = m_BoundaryPtMap.find( id2 );

      if( it != m_BoundaryPtMap.end( ) )
        {
        value = ( *m_CoefficientsMethod )( input, temp );
        pt2 = output->GetPoint( it->first );
        traits.AddToMatrix( iM, InternalId1, InternalId1, value );
        k[0] += static_cast< ValueType >( pt2[0] * value );
        k[1] += static_cast< ValueType >( pt2[1] * value );
        }
      else
        {
        InternalId2 = m_InternalPtMap[id2];
        if( InternalId1 < InternalId2 )
          {
          value = ( *m_CoefficientsMethod )( input, temp );
          traits.FillMatrix( iM, InternalId1, InternalId2, -value );
          traits.FillMatrix( iM, InternalId2, InternalId1, -value );
          traits.AddToMatrix( iM, InternalId1, InternalId1, value );
          traits.AddToMatrix( iM, InternalId2, InternalId2, value );
          }
       }

      temp = temp->GetOnext( );

      } while( temp != edge );

    ioBx[InternalId1] = k[0];
    ioBy[InternalId1] = k[1];
    }
}

// ---------------------------------------------------------------------
template< typename TInputMesh, typename TOutputMesh, typename TSolverTraits >
void
QuadEdgeMeshParam< TInputMesh, TOutputMesh, TSolverTraits >
::GenerateData( )
{
  this->CopyInputMeshToOutputMesh( );

  InputMeshConstPointer input = this->GetInput( );
  OutputMeshType* output = this->GetOutput( );

  if( m_BorderTransform.IsNotNull( ) )
    {
    m_BorderTransform->Update( );
    m_Border = m_BorderTransform->GetBorder( );
    m_BoundaryPtMap = m_BorderTransform->GetBoundaryPtMap( );
    }

  itkAssertOrThrowMacro( ( ( m_BoundaryPtMap.size( ) > 2 ) && ( m_Border.size( ) > 2 ) ),
    "BoundaryPtMap or Border have less than 2 elements" );

  this->CopyToOutputBorder( );

  this->ComputeListOfInteriorVertices( );

  size_t NbOfInteriorPts = m_InternalPtMap.size( );

  SolverTraits traits;

  MatrixType Matrix = traits.InitializeSparseMatrix( NbOfInteriorPts );
  VectorType Bx = traits.InitializeVector( NbOfInteriorPts );
  VectorType By = traits.InitializeVector( NbOfInteriorPts );
  VectorType X = traits.InitializeVector( NbOfInteriorPts );
  VectorType Y = traits.InitializeVector( NbOfInteriorPts );

  FillMatrix( Matrix, Bx, By );

  SolveLinearSystems( Matrix, Bx, By, X, Y );

  OutputPointType OutputPt;

  for( InputMapPoinIdentifierIterator PtIterator = m_InternalPtMap.begin( );
       PtIterator != m_InternalPtMap.end( );
       ++PtIterator )
    {
    OutputPointIdentifier id = static_cast< OutputPointIdentifier >( PtIterator->first );
    InputPointIdentifier InternalId = PtIterator->second;

    OutputPt[0] = X[InternalId];
    OutputPt[1] = Y[InternalId];
    OutputPt[2] = 0.;

    output->SetPoint( id, OutputPt );
    }
}

} // end namespace itk

#endif
