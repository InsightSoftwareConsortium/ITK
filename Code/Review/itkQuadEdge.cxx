/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdge.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkQuadEdge.h"

namespace itk
{
// ---------------------------------------------------------------------
QuadEdge
::QuadEdge()
{
  this->m_Onext = this;
  this->m_Rot   = NULL;
}

// ---------------------------------------------------------------------
QuadEdge
::~QuadEdge()
{
  this->m_Onext = NULL;
  this->m_Rot   = NULL;
}

// ---------------------------------------------------------------------
QuadEdge *
QuadEdge
::GetLnext()
{
#ifdef NDEBUG
  return this->GetInvRot()->GetOnext()->GetRot();
#else
  Self *p1 = this->GetInvRot();
  if ( p1 == NULL )
    {
    return NULL;
    }

  Self *p2 = p1->GetOnext();
  if ( p2 == NULL )
    {
    return NULL;
    }

  Self *p3 = p2->GetRot();
  if ( p3 == NULL )
    {
    return NULL;
    }

  return p3;
#endif
}

// ---------------------------------------------------------------------
const QuadEdge *
QuadEdge
::GetLnext() const
{
#ifdef NDEBUG
  return this->GetInvRot()->GetOnext()->GetRot();
#else
  const Self *p1 = this->GetInvRot();
  if ( p1 == NULL )
    {
    return NULL;
    }

  const Self *p2 = p1->GetOnext();
  if ( p2 == NULL )
    {
    return NULL;
    }

  const Self *p3 = p2->GetRot();
  if ( p3 == NULL )
    {
    return NULL;
    }

  return p3;
#endif
}

// ---------------------------------------------------------------------
QuadEdge *
QuadEdge
::GetRnext()
{
#ifdef NDEBUG
  return this->GetRot()->GetOnext()->GetInvRot();
#else
  Self *p1 = this->GetRot();
  if ( p1 == NULL )
    {
    return NULL;
    }

  Self *p2 = p1->GetOnext();
  if ( p2 == NULL )
    {
    return NULL;
    }

  Self *p3 = p2->GetInvRot();
  if ( p3 == NULL )
    {
    return NULL;
    }

  return p3;
#endif
}

// ---------------------------------------------------------------------
const QuadEdge *
QuadEdge
::GetRnext() const
{
#ifdef NDEBUG
  return this->GetRot()->GetOnext()->GetInvRot();
#else
  const Self *p1 = this->GetRot();
  if ( p1 == NULL )
    {
    return NULL;
    }

  const Self *p2 = p1->GetOnext();
  if ( p2 == NULL )
    {
    return NULL;
    }

  const Self *p3 = p2->GetInvRot();
  if ( p3 == NULL )
    {
    return NULL;
    }

  return p3;
#endif
}

// ---------------------------------------------------------------------
QuadEdge *
QuadEdge
::GetDnext()
{
#ifdef NDEBUG
  return this->GetSym()->GetOnext()->GetSym();
#else
  Self *p1 = this->GetSym();
  if ( p1 == NULL )
    {
    return NULL;
    }

  Self *p2 = p1->GetOnext();
  if ( p2 == NULL )
    {
    return NULL;
    }

  Self *p3 = p2->GetSym();
  if ( p3 == NULL )
    {
    return NULL;
    }

  return p3;
#endif
}

// ---------------------------------------------------------------------
const QuadEdge *
QuadEdge
::GetDnext() const
{
#ifdef NDEBUG
  return this->GetSym()->GetOnext()->GetSym();
#else
  const Self *p1 = this->GetSym();
  if ( p1 == NULL )
    {
    return NULL;
    }

  const Self *p2 = p1->GetOnext();
  if ( p2 == NULL )
    {
    return NULL;
    }

  const Self *p3 = p2->GetSym();
  if ( p3 == NULL )
    {
    return NULL;
    }

  return p3;
#endif
}

// ---------------------------------------------------------------------
QuadEdge *
QuadEdge
::GetOprev()
{
#ifdef NDEBUG
  return this->GetRot()->GetOnext()->GetRot();
#else
  Self *p1 = this->GetRot();
  if ( p1 == NULL )
    {
    return NULL;
    }

  Self *p2 = p1->GetOnext();
  if ( p2 == NULL )
    {
    return NULL;
    }

  Self *p3 = p2->GetRot();
  if ( p3 == NULL )
    {
    return NULL;
    }

  return p3;
#endif
}

// ---------------------------------------------------------------------
const QuadEdge *
QuadEdge
::GetOprev() const
{
#ifdef NDEBUG
  return this->GetRot()->GetOnext()->GetRot();
#else
  const Self *p1 = this->GetRot();
  if ( p1 == NULL )
    {
    return NULL;
    }

  const Self *p2 = p1->GetOnext();
  if ( p2 == NULL )
    {
    return NULL;
    }

  const Self *p3 = p2->GetRot();
  if ( p3 == NULL )
    {
    return NULL;
    }

  return p3;
#endif
}

// ---------------------------------------------------------------------
QuadEdge *
QuadEdge
::GetLprev()
{
#ifdef NDEBUG
  return this->GetOnext()->GetSym();
#else
  Self *p1 = this->GetOnext();
  if ( p1 == NULL )
    {
    return NULL;
    }

  Self *p2 = p1->GetSym();
  if ( p2 == NULL )
    {
    return NULL;
    }

  return p2;
#endif
}

// ---------------------------------------------------------------------
const QuadEdge *
QuadEdge
::GetLprev() const
{
#ifdef NDEBUG
  return this->GetOnext()->GetSym();
#else
  const Self *p1 = this->GetOnext();
  if ( p1 == NULL )
    {
    return NULL;
    }

  const Self *p2 = p1->GetSym();
  if ( p2 == NULL )
    {
    return NULL;
    }

  return p2;
#endif
}

// ---------------------------------------------------------------------
QuadEdge *
QuadEdge
::GetRprev()
{
#ifdef NDEBUG
  return this->GetSym()->GetOnext();
#else
  Self *p1 = this->GetSym();
  if ( p1 == NULL )
    {
    return NULL;
    }

  Self *p2 = p1->GetOnext();
  if ( p2 == NULL )
    {
    return NULL;
    }

  return p2;
#endif
}

// ---------------------------------------------------------------------
const QuadEdge *
QuadEdge
::GetRprev() const
{
#ifdef NDEBUG
  return this->GetSym()->GetOnext();
#else
  const Self *p1 = this->GetSym();
  if ( p1 == NULL )
    {
    return NULL;
    }

  const Self *p2 = p1->GetOnext();
  if ( p2 == NULL )
    {
    return NULL;
    }

  return p2;
#endif
}

// ---------------------------------------------------------------------
QuadEdge *
QuadEdge
::GetDprev()
{
#ifdef NDEBUG
  return this->GetInvRot()->GetOnext()->GetInvRot();
#else
  Self *p1 = this->GetInvRot();
  if ( p1 == NULL )
    {
    return NULL;
    }

  Self *p2 = p1->GetOnext();
  if ( p2 == NULL )
    {
    return NULL;
    }

  Self *p3 = p2->GetInvRot();
  if ( p3 == NULL )
    {
    return NULL;
    }

  return p3;
#endif
}

// ---------------------------------------------------------------------
const QuadEdge *
QuadEdge
::GetDprev() const
{
#ifdef NDEBUG
  return this->GetInvRot()->GetOnext()->GetInvRot();
#else
  const Self *p1 = this->GetInvRot();
  if ( p1 == NULL )
    {
    return NULL;
    }

  const Self *p2 = p1->GetOnext();
  if ( p2 == NULL )
    {
    return NULL;
    }

  const Self *p3 = p2->GetInvRot();
  if ( p3 == NULL )
    {
    return NULL;
    }

  return p3;
#endif
}

bool
QuadEdge
::IsEdgeInOnextRing(Self *testEdge) const
{
  if ( !this->IsIsolated() )
    {
    ConstIterator it = this->BeginOnext();
    while ( it != this->EndOnext() )
      {
      if ( it.Value() == NULL )
        {
        return false;
        }
      if ( it.Value() == testEdge )
        {
        return true;
        }
      it++;
      }
    }
  return false;
}

bool
QuadEdge
::IsLnextGivenSizeCyclic(const int size) const
{
  const Self *iterated = this;

  for ( int i = 0; i < size; i++ )
    {
    iterated = iterated->GetLnext();
    if ( !iterated ) { return false; }
    }
  return ( this == iterated );
}

unsigned int
QuadEdge
::GetOrder() const
{
  if ( !( this->IsIsolated() ) )
    {
    unsigned int order = 1; // count this edge
    const Self * it = this->GetOnext();
    while ( it && it != this )
      {
      order++;
      it = it->GetOnext();
      }
    return order;
    }
  return 0;
}
} // end namespace itk
