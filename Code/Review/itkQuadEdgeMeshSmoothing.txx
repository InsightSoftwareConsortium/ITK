/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshSmoothing.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkQuadEdgeMeshSmoothing_txx
#define __itkQuadEdgeMeshSmoothing_txx

#include "itkQuadEdgeMeshSmoothing.h"

namespace itk
{
template< class TInputMesh, class TOutputMesh >
QuadEdgeMeshSmoothing< TInputMesh, TOutputMesh >
::QuadEdgeMeshSmoothing()
{
  this->m_CoefficientsMethod = 0;
  this->m_DelaunayConforming = false;
  this->m_NumberOfIterations = 1;
  this->m_RelaxationFactor = static_cast< OutputCoordType >( 1.0 );

  this->m_InputDelaunayFilter = InputOutputDelaunayConformingType::New();
  this->m_OutputDelaunayFilter = OutputDelaunayConformingType::New();
}

template< class TInputMesh, class TOutputMesh >
QuadEdgeMeshSmoothing< TInputMesh, TOutputMesh >::~QuadEdgeMeshSmoothing()
{
}

template< class TInputMesh, class TOutputMesh >
void QuadEdgeMeshSmoothing< TInputMesh, TOutputMesh >::
GenerateData()
{
  OutputMeshPointer mesh = OutputMeshType::New();

  OutputPointsContainerPointer temp = OutputPointsContainer::New();
  temp->Reserve( this->GetInput()->GetNumberOfPoints() );

  OutputPointsContainerPointer points;
  OutputPointsContainerIterator it;

  OutputPointType p;
  OutputPointType q;
  OutputPointType r;
  OutputVectorType v;

  OutputCoordType coeff;
  OutputCoordType sum_coeff;
  OutputCoordType den;

  OutputQEType * qe;
  OutputQEType * qe_it;

  if( this->m_DelaunayConforming )
    {
    m_InputDelaunayFilter->SetInput( this->GetInput() );
    if( m_NumberOfIterations == 0 )
      {
      m_InputDelaunayFilter->GraftOutput( this->GetOutput() );
      m_InputDelaunayFilter->Update();
      this->GraftOutput( m_InputDelaunayFilter->GetOutput() );
      }
    else
      {
      m_InputDelaunayFilter->Update();
      mesh = m_InputDelaunayFilter->GetOutput();
      }
    }
  else
    {
    if( m_NumberOfIterations == 0 )
      {
      this->CopyInputMeshToOutputMesh();
      }
    else
      {
      this->CopyMeshToMesh(this->GetInput(), mesh);
      }
    }

  for( unsigned int iter = 0; iter < m_NumberOfIterations; ++iter )
    {
    points = mesh->GetPoints();

    for( it = points->Begin(); it != points->End(); ++it )
      {
      p = it.Value();
      qe = p.GetEdge();
      if( qe != 0 )
        {
        r = p;
        v.Fill( 0.0 );
        qe_it = qe;
        sum_coeff = 0.;
        do
          {
          q = mesh->GetPoint( qe_it->GetDestination() );

          coeff = ( *m_CoefficientsMethod )( mesh, qe_it );
          sum_coeff += coeff;

          v += coeff * ( q - p );
          qe_it = qe_it->GetOnext();
          } while( qe_it != qe );

        den = 1.0 / static_cast< OutputCoordType >( sum_coeff );
        v *= den;

        r += m_RelaxationFactor * v;
        r.SetEdge( qe );
        temp->SetElement( it.Index(), r );
        }
      else
        {
        temp->SetElement( it.Index(), p );
        }
      }

    mesh->SetPoints( temp );

    if( this->m_DelaunayConforming )
      {
      mesh->DisconnectPipeline();
      m_OutputDelaunayFilter->SetInput( mesh );

      if( iter + 1 == m_NumberOfIterations )
        {
        m_OutputDelaunayFilter->GraftOutput( this->GetOutput() );
        m_OutputDelaunayFilter->Update();
        this->GraftOutput( m_OutputDelaunayFilter->GetOutput() );
        }
      else
        {
        m_OutputDelaunayFilter->Update();
        mesh = m_OutputDelaunayFilter->GetOutput();
        }
      }

    if( iter + 1 == m_NumberOfIterations )
      {
      this->GraftOutput( mesh );
      }
    }
}
}

#endif
