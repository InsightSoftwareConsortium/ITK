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
  Self * p1 = this->GetInvRot();
  if( p1 == NULL )
    {
    return NULL;
    }

  Self * p2 = p1->GetOnext();
  if( p2 == NULL )
    {
    return NULL;
    }

  Self * p3 = p2->GetRot();
  if( p3 == NULL )
    {
    return NULL;
    }

  return p3;
}


// ---------------------------------------------------------------------
const QuadEdge * 
QuadEdge
::GetLnext() const
{
  const Self * p1 = this->GetInvRot();
  if( p1 == NULL )
    {
    return NULL;
    }

  const Self * p2 = p1->GetOnext();
  if( p2 == NULL )
    {
    return NULL;
    }

  const Self * p3 = p2->GetRot();
  if( p3 == NULL )
    {
    return NULL;
    }

  return p3;
}


// ---------------------------------------------------------------------
QuadEdge * 
QuadEdge
::GetRnext() 
{
  Self * p1 = this->GetRot();
  if( p1 == NULL )
    {
    return NULL;
    }
 
  Self * p2 = p1->GetOnext();
  if( p2 == NULL )
    {
    return NULL;
    }

  Self * p3 = p2->GetInvRot();
  if( p3 == NULL )
    {
    return NULL;
    }
  
  return p3;
}


// ---------------------------------------------------------------------
const QuadEdge * 
QuadEdge
::GetRnext() const
{
  const Self * p1 = this->GetRot();
  if( p1 == NULL )
    {
    return NULL;
    }

  const Self * p2 = p1->GetOnext();
  if( p2 == NULL )
    {
    return NULL;
    }

  const Self * p3 = p2->GetInvRot();
  if( p3 == NULL )
    {
    return NULL;
    }

  return p3;
}


// ---------------------------------------------------------------------
QuadEdge * 
QuadEdge
::GetDnext() 
{
  Self * p1 = this->GetSym();
  if( p1 == NULL )
    {
    return NULL;
    }

  Self * p2 = p1->GetOnext();
  if( p2 == NULL )
    {
    return NULL;
    }

  Self * p3 = p2->GetSym();
  if( p3 == NULL )
    {
    return NULL;
    }

  return p3;
}


// ---------------------------------------------------------------------
const QuadEdge * 
QuadEdge
::GetDnext() const
{
  const Self * p1 = this->GetSym();
  if( p1 == NULL )
    {
    return NULL;
    }

  const Self * p2 = p1->GetOnext();
  if( p2 == NULL )
    {
    return NULL;
    }

  const Self * p3 = p2->GetSym();
  if( p3 == NULL )
    {
    return NULL;
    }

  return p3;
}


// ---------------------------------------------------------------------
QuadEdge * 
QuadEdge
::GetOprev() 
{
  Self * p1 = this->GetRot();
  if( p1 == NULL )
    {
    return NULL;
    }

  Self * p2 = p1->GetOnext();
  if( p2 == NULL )
    {
    return NULL;
    }

  Self * p3 = p2->GetRot();
  if( p3 == NULL )
    {
    return NULL;
    }

  return p3;
}


// ---------------------------------------------------------------------
const QuadEdge * 
QuadEdge
::GetOprev() const
{
  const Self * p1 = this->GetRot();
  if( p1 == NULL )
    {
    return NULL;
    }

  const Self * p2 = p1->GetOnext();
  if( p2 == NULL )
    {
    return NULL;
    }

  const Self * p3 = p2->GetRot();
  if( p3 == NULL )
    {
    return NULL;
    }

  return p3;
}


// ---------------------------------------------------------------------
QuadEdge * 
QuadEdge
::GetLprev() 
{
  Self * p1 = this->GetOnext();
  if( p1 == NULL )
    {
    return NULL;
    }

  Self * p2 = p1->GetSym();
  if( p2 == NULL )
    {
    return NULL;
    }

  return p2;
}


// ---------------------------------------------------------------------
const QuadEdge * 
QuadEdge
::GetLprev() const
{
  const Self * p1 = this->GetOnext();
  if( p1 == NULL )
    {
    return NULL;
    }

  const Self * p2 = p1->GetSym();
  if( p2 == NULL )
    {
    return NULL;
    }

  return p2;
}


// ---------------------------------------------------------------------
QuadEdge * 
QuadEdge
::GetRprev() 
{
  Self * p1 = this->GetSym();
  if( p1 == NULL )
    {
    return NULL;
    }

  Self * p2 = p1->GetOnext();
  if( p2 == NULL )
    {
    return NULL;
    }

  return p2;
}


// ---------------------------------------------------------------------
const QuadEdge * 
QuadEdge
::GetRprev() const
{
  const Self * p1 = this->GetSym();
  if( p1 == NULL )
    {
    return NULL;
    }

  const Self * p2 = p1->GetOnext();
  if( p2 == NULL )
    {
    return NULL;
    }

  return p2;
}


// ---------------------------------------------------------------------
QuadEdge * 
QuadEdge
::GetDprev() 
{
  Self * p1 = this->GetInvRot();
  if( p1 == NULL )
    {
    return NULL;
    }

  Self * p2 = p1->GetOnext();
  if( p2 == NULL )
    {
    return NULL;
    }

  Self * p3 = p2->GetInvRot();
  if( p3 == NULL )
    {
    return NULL;
    }

  return p3;
}


// ---------------------------------------------------------------------
const QuadEdge * 
QuadEdge
::GetDprev() const
{
  const Self * p1 = this->GetInvRot();
  if( p1 == NULL )
    {
    return NULL;
    }

  const Self * p2 = p1->GetOnext();
  if( p2 == NULL )
    {
    return NULL;
    }

  const Self * p3 = p2->GetInvRot();
  if( p3 == NULL )
    {
    return NULL;
    }

  return p3;
} 

bool
QuadEdge
::IsEdgeInOnextRing( Self* testEdge ) const
{ 
  if( !this->IsIsolated( ) )
    {
    ConstIterator it = this->BeginOnext(); 
    while( it != this->EndOnext() )
      {
      if( it.Value() == NULL )
        {
        return false;
        }
      if( it.Value() == testEdge ) 
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
::IsLnextGivenSizeCyclic( const int size ) const
{ 
  const Self* iterated = this;
  for( int i = 0; i < size; i++ )
    {
    iterated = iterated->GetLnext();
    if( !iterated ) return false;
    }  
  return ( this == iterated ); 
}

unsigned int
QuadEdge
::GetOrder() const
{ 
  if( !(this->IsIsolated( ) ) )
    {
    unsigned int order = 1; // count this edge
    const Self * it = this->GetOnext();
    while( it && it != this )
      {
      order++;
      it = it->GetOnext();
      }
    return order;
    }
  return 0;
}

} // end namespace itk
