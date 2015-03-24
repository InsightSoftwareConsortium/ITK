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
#ifndef itkQuadEdgeMeshLineCell_hxx
#define itkQuadEdgeMeshLineCell_hxx

#include "itkCellInterfaceVisitor.h"
#include "itkQuadEdgeMeshLineCell.h"

namespace itk
{
// ---------------------------------------------------------------------
template< typename TCellInterface >
QuadEdgeMeshLineCell< TCellInterface >
::QuadEdgeMeshLineCell()
{
  m_Identifier = 0;
  m_QuadEdgeGeom = new QEType;

  QEType *e2 = new QEType;
  QEDual *e1 = new QEDual;
  QEDual *e3 = new QEDual;
  this->m_QuadEdgeGeom->SetRot(e1);
  e1->SetRot(e2);
  e2->SetRot(e3);
  e3->SetRot(this->m_QuadEdgeGeom);
  this->m_QuadEdgeGeom->SetOnext(this->m_QuadEdgeGeom);
  e1->SetOnext(e3);
  e2->SetOnext(e2);
  e3->SetOnext(e1);
}

// ---------------------------------------------------------------------
template< typename TCellInterface >
QuadEdgeMeshLineCell< TCellInterface >
::~QuadEdgeMeshLineCell()
{
  // ALEX: for performance issues,
  // we will assume the user calls Disconnect beforehand
  // or else it is the mesh destructor, and we can proceed.
  // if( !m_QuadEdgeGeom->IsDisconnected( ) )
  //  {
  //  m_QuadEdgeGeom->Disconnect( );
  //  }

  bool FoundNullPointer = false;

  if ( m_QuadEdgeGeom )
    {
    if ( m_QuadEdgeGeom->GetRot() )
      {
      if ( m_QuadEdgeGeom->GetRot()->GetRot() )
        {
        if ( m_QuadEdgeGeom->GetRot()->GetRot()->GetRot() )
          {
          delete m_QuadEdgeGeom->GetRot()->GetRot()->GetRot();    //e3
          delete m_QuadEdgeGeom->GetRot()->GetRot();              //e2
          delete m_QuadEdgeGeom->GetRot();                        //e1
          delete m_QuadEdgeGeom;
          }
        else
          {
          FoundNullPointer = true;
          delete m_QuadEdgeGeom->GetRot()->GetRot();              //e2
          delete m_QuadEdgeGeom->GetRot();                        //e1
          delete m_QuadEdgeGeom;
          }
        }
      else
        {
        FoundNullPointer = true;
        delete m_QuadEdgeGeom->GetRot();                        //e1
        delete m_QuadEdgeGeom;
        }
      }
    else
      {
      FoundNullPointer = true;
      delete m_QuadEdgeGeom;
      }
    }
  else
    {
    FoundNullPointer = true;
    }

  if ( FoundNullPointer )
    {
    //Throw exception here
    }
}

// ---------------------------------------------------------------------
template< typename TCellInterface >
void
QuadEdgeMeshLineCell< TCellInterface >
::Accept(CellIdentifier cellId, MultiVisitor *mv)
{
  typedef CellInterfaceVisitor< PixelType, CellTraits > IntVis;
  typename IntVis::Pointer v = mv->GetVisitor( this->GetType() );
  if ( v )
    {
    v->VisitFromCell(cellId, this);
    }
}

// ---------------------------------------------------------------------
template< typename TCellInterface >
typename QuadEdgeMeshLineCell< TCellInterface >::CellFeatureCount
QuadEdgeMeshLineCell< TCellInterface >
::GetNumberOfBoundaryFeatures(int dimension) const
{
  if ( dimension == 0 )
    {
    return 2;
    }

  if ( dimension == 1 )
    {
    return 1;
    }

  return 0;
}

// ---------------------------------------------------------------------
template< typename TCellInterface >
bool
QuadEdgeMeshLineCell< TCellInterface >
::GetBoundaryFeature(int dimension, CellFeatureIdentifier cellId,
                     CellAutoPointer & cell)
{
  // TODO : FIXME
  (void)dimension;
  (void)cellId;
  (void)cell;
  return ( false );
}

// ---------------------------------------------------------------------
template< typename TCellInterface >
void
QuadEdgeMeshLineCell< TCellInterface >
::SetPointIds(PointIdConstIterator first)
{
  PointIdConstIterator i = first;

  this->GetQEGeom()->SetOrigin(*i);
  i++;
  this->GetQEGeom()->SetDestination(*i);
}

// ---------------------------------------------------------------------
template< typename TCellInterface >
void
QuadEdgeMeshLineCell< TCellInterface >
::InternalSetPointIds(PointIdInternalConstIterator first)
{
  PointIdInternalConstIterator i = first;

  this->GetQEGeom()->SetOrigin(*i);
  i++;
  this->GetQEGeom()->SetDestination(*i);
}

// ---------------------------------------------------------------------
template< typename TCellInterface >
void
QuadEdgeMeshLineCell< TCellInterface >
::SetPointIds(PointIdConstIterator first,
              PointIdConstIterator last)
{
  (void)last;
  this->GetQEGeom()->SetOrigin(*first);
  first++;
  this->GetQEGeom()->SetDestination(*first);
}

// ---------------------------------------------------------------------
template< typename TCellInterface >
void
QuadEdgeMeshLineCell< TCellInterface >
::InternalSetPointIds(PointIdInternalConstIterator first,
                      PointIdInternalConstIterator last)
{
  (void)last;
  this->GetQEGeom()->SetOrigin(*first);
  first++;
  this->GetQEGeom()->SetDestination(*first);
}

// ---------------------------------------------------------------------
template< typename TCellInterface >
void
QuadEdgeMeshLineCell< TCellInterface >
::SetPointId(int localId, PointIdentifier pId)
{
  if ( localId == 0 )
    {
    this->GetQEGeom()->SetOrigin(pId);
    }
  else if ( localId == 1 )
    {
    this->GetQEGeom()->SetDestination(pId);
    }
}

// ---------------------------------------------------------------------
template< typename TCellInterface >
typename QuadEdgeMeshLineCell< TCellInterface >::PointIdInternalIterator
QuadEdgeMeshLineCell< TCellInterface >
::InternalPointIdsBegin()
{
  return ( PointIdInternalIterator(this->m_QuadEdgeGeom,
                                   PointIdInternalIterator::OperatorSym, true) );
}

// ---------------------------------------------------------------------
template< typename TCellInterface >
typename QuadEdgeMeshLineCell< TCellInterface >::PointIdInternalIterator
QuadEdgeMeshLineCell< TCellInterface >
::InternalPointIdsEnd()
{
  return ( PointIdInternalIterator(this->m_QuadEdgeGeom,
                                   PointIdInternalIterator::OperatorSym, false) );
}

// ---------------------------------------------------------------------
template< typename TCellInterface >
typename QuadEdgeMeshLineCell< TCellInterface >::PointIdInternalConstIterator
QuadEdgeMeshLineCell< TCellInterface >
::InternalGetPointIds() const
{
  return ( PointIdInternalConstIterator(this->m_QuadEdgeGeom,
                                        PointIdInternalConstIterator::OperatorSym, true) );
}

// ---------------------------------------------------------------------
template< typename TCellInterface >
typename QuadEdgeMeshLineCell< TCellInterface >::PointIdInternalConstIterator
QuadEdgeMeshLineCell< TCellInterface >
::InternalPointIdsBegin() const
{
  return ( PointIdInternalConstIterator(this->m_QuadEdgeGeom,
                                        PointIdInternalConstIterator::OperatorSym, true) );
}

// ---------------------------------------------------------------------
template< typename TCellInterface >
typename QuadEdgeMeshLineCell< TCellInterface >::PointIdInternalConstIterator
QuadEdgeMeshLineCell< TCellInterface >
::InternalPointIdsEnd() const
{
  return ( PointIdInternalConstIterator(this->m_QuadEdgeGeom,
                                        PointIdInternalConstIterator::OperatorSym, false) );
}

// ---------------------------------------------------------------------
template< typename TCellInterface >
void
QuadEdgeMeshLineCell< TCellInterface >
::SetIdent(CellIdentifier cid)
{
  this->m_Identifier = cid;
  this->m_QuadEdgeGeom->SetIdent(cid);
  this->m_QuadEdgeGeom->GetSym()->SetIdent(cid);
}

// ---------------------------------------------------------------------
template< typename TCellInterface >
typename QuadEdgeMeshLineCell< TCellInterface >::CellIdentifier
QuadEdgeMeshLineCell< TCellInterface >
::GetIdent()
{
  return this->m_Identifier;
}

// ---------------------------------------------------------------------
template< typename TCellInterface >
typename QuadEdgeMeshLineCell< TCellInterface >::CellGeometry
QuadEdgeMeshLineCell< TCellInterface >
::GetType() const
{
  return Superclass::LINE_CELL;
}

// ---------------------------------------------------------------------
template< typename TCellInterface >
int
QuadEdgeMeshLineCell< TCellInterface >
::GetTopologyId()
{
  return Superclass::LINE_CELL;
}

// ---------------------------------------------------------------------
template< typename TCellInterface >
unsigned int
QuadEdgeMeshLineCell< TCellInterface >
::GetDimension() const
{
  return Self::CellDimension;
}

// ---------------------------------------------------------------------
template< typename TCellInterface >
unsigned int
QuadEdgeMeshLineCell< TCellInterface >
::GetNumberOfPoints() const
{
  return 2;
}
} // end namespace itk

#endif
