/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshEulerOperatorCreateCenterVertexFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkQuadEdgeMeshEulerOperatorCreateCenterVertexFunction_txx
#define __itkQuadEdgeMeshEulerOperatorCreateCenterVertexFunction_txx

#include "itkQuadEdgeMeshEulerOperatorCreateCenterVertexFunction.h"

namespace itk
{
template< class TMesh, class TQEType >
typename QuadEdgeMeshEulerOperatorCreateCenterVertexFunction< TMesh, TQEType >::OutputType
QuadEdgeMeshEulerOperatorCreateCenterVertexFunction< TMesh, TQEType >::Evaluate(QEType *e)
{
  // Is there any input ?
#ifndef NDEBUG
  if ( !e )
    {
    itkDebugMacro("Input is not an edge.");
    return ( (QEType *)0 );
    }

  if ( !this->m_Mesh )
    {
    itkDebugMacro("No mesh present.");
    return ( (OutputType)0 );
    }

  // Is left face set ?
  if ( !e->IsLeftSet() )
    {
    itkDebugMacro("Argument edge has no left face.");
    return ( (OutputType)0 );
    }
#endif

  // remove left face
  this->m_Mesh->DeleteFace( e->GetLeft() );

  // create new point geometry
  unsigned int sum = 0;
  VectorType   vec;
  vec.Fill(0);
  PointIdentifier pid = this->m_Mesh->FindFirstUnusedPointIndex();
  typedef std::map< QEType *, PointIdentifier > AssociatedBarycenters;
  AssociatedBarycenters m_AssocBary;
  typedef typename QEType::IteratorGeom QEIterator;
  QEIterator lit = e->BeginGeomLnext();
  while ( lit != e->EndGeomLnext() )
    {
    QEType *g = lit.Value();
    vec += this->m_Mesh->GetVector( g->GetOrigin() );
    sum++;
    m_AssocBary[g] = pid;
    lit++;
    } // rof
  vec /= CoordRepType(sum);
  PointType p;
  for ( unsigned int i = 0; i < 3; i++ )
    {
    p[i] = vec[i];
    }

  // add new point to mesh
  this->m_NewPointID = this->m_Mesh->AddPoint(p);
  PointIdentifier tempPoint;

  // create edges and faces
  tempPoint = e->GetDestination();
  this->m_Mesh->AddFaceTriangle(this->m_NewPointID,
                                e->GetOrigin(),
                                tempPoint);
  QEType *edgeRef = this->m_Mesh->FindEdge(this->m_NewPointID, tempPoint);
  while ( !edgeRef->IsLeftSet() )
    {
    tempPoint = edgeRef->GetLnext()->GetDestination();
    this->m_Mesh->AddFaceTriangle(this->m_NewPointID,
                                  edgeRef->GetLnext()->GetOrigin(),
                                  tempPoint);
    edgeRef = this->m_Mesh->FindEdge(this->m_NewPointID, tempPoint);
    }

  return ( e->GetLnext() );
}
} // namespace itk

#endif

// eof - itkQuadEdgeMeshEulerOperatorCreateCenterVertexFunction.txx
