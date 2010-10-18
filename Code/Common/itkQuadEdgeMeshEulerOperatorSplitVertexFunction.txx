/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshEulerOperatorSplitVertexFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkQuadEdgeMeshEulerOperatorSplitVertexFunction_txx
#define __itkQuadEdgeMeshEulerOperatorSplitVertexFunction_txx

#include "itkQuadEdgeMeshEulerOperatorSplitVertexFunction.h"

namespace itk
{
template< class TMesh, class TQEType >
typename QuadEdgeMeshEulerOperatorSplitVertexFunction< TMesh, TQEType >::OutputType
QuadEdgeMeshEulerOperatorSplitVertexFunction< TMesh, TQEType >::Evaluate(QEType *h, QEType *g)
{
  if ( !this->m_Mesh )
    {
    itkDebugMacro("No mesh present.");
    return ( (QEType *)0 );
    }

  if ( ( h == (QEType *)( 0 ) ) || ( g == (QEType *)( 0 ) ) )
    {
    itkDebugMacro("One or more argument(s) is(are) null.");
    return ( (QEType *)0 );
    }

  if ( h == g )
    {
    itkDebugMacro("The two half-edges are the same. No antenna allowed.");
    return ( (QEType *)0 );
    }

  if ( h->GetDestination() != g->GetDestination() )
    {
    itkDebugMacro("The two half-edges must be incident to the same vertex.");
    return ( (QEType *)0 );
    }

  // delete the faces
  this->m_Mesh->DeleteFace( h->GetRight() );
  this->m_Mesh->DeleteFace( g->GetRight() );

  // splice to create a new point and disconnect the rings
  this->m_NewPoint = this->m_Mesh->Splice( h->GetSym(), g->GetSym() );

  // then add a new edge
  QEType *ReturnedEdge = this->m_Mesh->AddEdge( g->GetDestination(),
                                                h->GetDestination() );

  // Build two new faces
  this->m_Mesh->AddFace( h->GetSym() );
  this->m_Mesh->AddFace( g->GetSym() );
  this->m_Mesh->Modified();

  return ( ReturnedEdge );
}
} // namespace itk

#endif

// eof - itkQuadEdgeMeshEulerOperatorSplitVertexFunction.txx
