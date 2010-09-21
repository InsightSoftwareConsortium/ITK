/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshPoint.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkQuadEdgeMeshPoint_txx
#define __itkQuadEdgeMeshPoint_txx

#include "itkQuadEdgeMeshPoint.h"

namespace itk
{
// ---------------------------------------------------------------------
template< class TCoordRep, unsigned int VPointDimension, typename TQuadEdge >
void
QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge >
::Initialize()
{
  m_Edge = static_cast< TQuadEdge * >( NULL );
}

// ---------------------------------------------------------------------
template< class TCoordRep, unsigned int VPointDimension, typename TQuadEdge >
QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge >
::QuadEdgeMeshPoint()
{
  this->Initialize();
}

// ---------------------------------------------------------------------
template< class TCoordRep, unsigned int VPointDimension, typename TQuadEdge >
QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge >
::QuadEdgeMeshPoint(const Self & r):Superclass(r)
{
  this->Initialize();
  m_Edge = r.m_Edge;
}

// ---------------------------------------------------------------------
template< class TCoordRep, unsigned int VPointDimension, typename TQuadEdge >
QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge >
::QuadEdgeMeshPoint(const Superclass & r):Superclass(r)
{
  this->Initialize();
}

// ---------------------------------------------------------------------
template< class TCoordRep, unsigned int VPointDimension, typename TQuadEdge >
QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge > &
QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge >
::operator=(const Self & r)
{
  this->Superclass::operator=(r);
  m_Edge = r.m_Edge;
  return ( *this );
}

// ---------------------------------------------------------------------
template< class TCoordRep, unsigned int VPointDimension, typename TQuadEdge >
QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge > &
QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge >
::operator=(const Superclass & r)
{
  this->Superclass::operator=(r);
  this->Initialize();
  return ( *this );
}

// ---------------------------------------------------------------------
template< class TCoordRep, unsigned int VPointDimension, typename TQuadEdge >
QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge > &
QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge >
::operator=(const ValueType r[VPointDimension])
{
  this->Superclass::operator=(r);
  this->Initialize();
  return ( *this );
}

template< class TCoordRep, unsigned int VPointDimension, typename TQuadEdge >
bool
QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge >
::IsInternal() const
{
  if ( this->GetEdge() )
    {
    return this->GetEdge()->IsOriginInternal();
    }
  return false;
}

/** Return the valence of this QuadEdgeMeshPoint i.e. the number of edges constituting
 *  the Onext ring to which this point belongs.
 *  @return the valence when an entry in the Onext ring is present,
 *          and -1 otherwise.
 */
template< class TCoordRep, unsigned int VPointDimension, typename TQuadEdge >
int QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge >
::GetValence() const
{
  int valence = -1; // error code by default

  if ( this->GetEdge() )
    {
    valence =  this->GetEdge()->GetOrder();
    }

  return valence;
}

/** Set Edge
 *
 */
template< class TCoordRep, unsigned int VPointDimension, typename TQuadEdge >
void
QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge >
::SetEdge(TQuadEdge *inputEdge)
{
  m_Edge = inputEdge;
}

/** Get Edge
 *
 */
template< class TCoordRep, unsigned int VPointDimension, typename TQuadEdge >
TQuadEdge *
QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge >
::GetEdge() const
{
  return m_Edge;
}

/** Get Edge non-const version
 *
 */
template< class TCoordRep, unsigned int VPointDimension, typename TQuadEdge >
TQuadEdge *
QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge >
::GetEdge()
{
  return ( m_Edge );
}

/** Set Point Coordinates
 *
 */
template< class TCoordRep, unsigned int VPointDimension, typename TQuadEdge >
void
QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge >
::SetPoint(const Superclass & point)
{
  this->Superclass::operator=(point);
}
}

#endif
