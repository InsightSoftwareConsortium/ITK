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
#include "itkQuadEdge.h"

namespace itk
{
// ---------------------------------------------------------------------
QuadEdge
::QuadEdge()
{
  this->m_Onext = this;
  this->m_Rot   = ITK_NULLPTR;
}

// ---------------------------------------------------------------------
QuadEdge
::~QuadEdge()
{
  this->m_Onext = ITK_NULLPTR;
  this->m_Rot   = ITK_NULLPTR;
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
  if ( p1 == ITK_NULLPTR )
    {
    return ITK_NULLPTR;
    }

  Self *p2 = p1->GetOnext();
  if ( p2 == ITK_NULLPTR )
    {
    return ITK_NULLPTR;
    }

  Self *p3 = p2->GetRot();
  if ( p3 == ITK_NULLPTR )
    {
    return ITK_NULLPTR;
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
  if ( p1 == ITK_NULLPTR )
    {
    return ITK_NULLPTR;
    }

  const Self *p2 = p1->GetOnext();
  if ( p2 == ITK_NULLPTR )
    {
    return ITK_NULLPTR;
    }

  const Self *p3 = p2->GetRot();
  if ( p3 == ITK_NULLPTR )
    {
    return ITK_NULLPTR;
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
  if ( p1 == ITK_NULLPTR )
    {
    return ITK_NULLPTR;
    }

  Self *p2 = p1->GetOnext();
  if ( p2 == ITK_NULLPTR )
    {
    return ITK_NULLPTR;
    }

  Self *p3 = p2->GetInvRot();
  if ( p3 == ITK_NULLPTR )
    {
    return ITK_NULLPTR;
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
  if ( p1 == ITK_NULLPTR )
    {
    return ITK_NULLPTR;
    }

  const Self *p2 = p1->GetOnext();
  if ( p2 == ITK_NULLPTR )
    {
    return ITK_NULLPTR;
    }

  const Self *p3 = p2->GetInvRot();
  if ( p3 == ITK_NULLPTR )
    {
    return ITK_NULLPTR;
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
  if ( p1 == ITK_NULLPTR )
    {
    return ITK_NULLPTR;
    }

  Self *p2 = p1->GetOnext();
  if ( p2 == ITK_NULLPTR )
    {
    return ITK_NULLPTR;
    }

  Self *p3 = p2->GetSym();
  if ( p3 == ITK_NULLPTR )
    {
    return ITK_NULLPTR;
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
  if ( p1 == ITK_NULLPTR )
    {
    return ITK_NULLPTR;
    }

  const Self *p2 = p1->GetOnext();
  if ( p2 == ITK_NULLPTR )
    {
    return ITK_NULLPTR;
    }

  const Self *p3 = p2->GetSym();
  if ( p3 == ITK_NULLPTR )
    {
    return ITK_NULLPTR;
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
  if ( p1 == ITK_NULLPTR )
    {
    return ITK_NULLPTR;
    }

  Self *p2 = p1->GetOnext();
  if ( p2 == ITK_NULLPTR )
    {
    return ITK_NULLPTR;
    }

  Self *p3 = p2->GetRot();
  if ( p3 == ITK_NULLPTR )
    {
    return ITK_NULLPTR;
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
  if ( p1 == ITK_NULLPTR )
    {
    return ITK_NULLPTR;
    }

  const Self *p2 = p1->GetOnext();
  if ( p2 == ITK_NULLPTR )
    {
    return ITK_NULLPTR;
    }

  const Self *p3 = p2->GetRot();
  if ( p3 == ITK_NULLPTR )
    {
    return ITK_NULLPTR;
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
  if ( p1 == ITK_NULLPTR )
    {
    return ITK_NULLPTR;
    }

  Self *p2 = p1->GetSym();
  if ( p2 == ITK_NULLPTR )
    {
    return ITK_NULLPTR;
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
  if ( p1 == ITK_NULLPTR )
    {
    return ITK_NULLPTR;
    }

  const Self *p2 = p1->GetSym();
  if ( p2 == ITK_NULLPTR )
    {
    return ITK_NULLPTR;
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
  if ( p1 == ITK_NULLPTR )
    {
    return ITK_NULLPTR;
    }

  Self *p2 = p1->GetOnext();
  if ( p2 == ITK_NULLPTR )
    {
    return ITK_NULLPTR;
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
  if ( p1 == ITK_NULLPTR )
    {
    return ITK_NULLPTR;
    }

  const Self *p2 = p1->GetOnext();
  if ( p2 == ITK_NULLPTR )
    {
    return ITK_NULLPTR;
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
  if ( p1 == ITK_NULLPTR )
    {
    return ITK_NULLPTR;
    }

  Self *p2 = p1->GetOnext();
  if ( p2 == ITK_NULLPTR )
    {
    return ITK_NULLPTR;
    }

  Self *p3 = p2->GetInvRot();
  if ( p3 == ITK_NULLPTR )
    {
    return ITK_NULLPTR;
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
  if ( p1 == ITK_NULLPTR )
    {
    return ITK_NULLPTR;
    }

  const Self *p2 = p1->GetOnext();
  if ( p2 == ITK_NULLPTR )
    {
    return ITK_NULLPTR;
    }

  const Self *p3 = p2->GetInvRot();
  if ( p3 == ITK_NULLPTR )
    {
    return ITK_NULLPTR;
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
      if ( it.Value() == ITK_NULLPTR )
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
