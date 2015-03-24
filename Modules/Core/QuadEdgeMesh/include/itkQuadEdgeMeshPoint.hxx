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
#ifndef itkQuadEdgeMeshPoint_hxx
#define itkQuadEdgeMeshPoint_hxx

#include "itkQuadEdgeMeshPoint.h"

namespace itk
{
// ---------------------------------------------------------------------
template< typename TCoordRep, unsigned int VPointDimension, typename TQuadEdge >
void
QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge >
::Initialize()
{
  m_Edge = static_cast< TQuadEdge * >( ITK_NULLPTR );
}

// ---------------------------------------------------------------------
template< typename TCoordRep, unsigned int VPointDimension, typename TQuadEdge >
QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge >
::QuadEdgeMeshPoint()
{
  this->Initialize();
}

// ---------------------------------------------------------------------
template< typename TCoordRep, unsigned int VPointDimension, typename TQuadEdge >
QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge >
::QuadEdgeMeshPoint(const Self & r):Superclass(r)
{
  this->Initialize();
  m_Edge = r.m_Edge;
}

// ---------------------------------------------------------------------
template< typename TCoordRep, unsigned int VPointDimension, typename TQuadEdge >
QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge >
::QuadEdgeMeshPoint(const Superclass & r):Superclass(r)
{
  this->Initialize();
}

// ---------------------------------------------------------------------
template< typename TCoordRep, unsigned int VPointDimension, typename TQuadEdge >
QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge > &
QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge >
::operator=(const Self & r)
{
  this->Superclass::operator=(r);
  m_Edge = r.m_Edge;
  return ( *this );
}

// ---------------------------------------------------------------------
template< typename TCoordRep, unsigned int VPointDimension, typename TQuadEdge >
QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge > &
QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge >
::operator=(const Superclass & r)
{
  this->Superclass::operator=(r);
  this->Initialize();
  return ( *this );
}

// ---------------------------------------------------------------------
template< typename TCoordRep, unsigned int VPointDimension, typename TQuadEdge >
QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge > &
QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge >
::operator=(const ValueType r[VPointDimension])
{
  this->Superclass::operator=(r);
  this->Initialize();
  return ( *this );
}

template< typename TCoordRep, unsigned int VPointDimension, typename TQuadEdge >
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
template< typename TCoordRep, unsigned int VPointDimension, typename TQuadEdge >
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
template< typename TCoordRep, unsigned int VPointDimension, typename TQuadEdge >
void
QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge >
::SetEdge(TQuadEdge *inputEdge)
{
  m_Edge = inputEdge;
}

/** Get Edge
 *
 */
template< typename TCoordRep, unsigned int VPointDimension, typename TQuadEdge >
TQuadEdge *
QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge >
::GetEdge() const
{
  return m_Edge;
}

/** Get Edge non-const version
 *
 */
template< typename TCoordRep, unsigned int VPointDimension, typename TQuadEdge >
TQuadEdge *
QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge >
::GetEdge()
{
  return ( m_Edge );
}

/** Set Point Coordinates
 *
 */
template< typename TCoordRep, unsigned int VPointDimension, typename TQuadEdge >
void
QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge >
::SetPoint(const Superclass & point)
{
  this->Superclass::operator=(point);
}
}

#endif
