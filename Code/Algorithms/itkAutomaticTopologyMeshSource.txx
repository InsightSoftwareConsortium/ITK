/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAutomaticTopologyMeshSource.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkAutomaticTopologyMeshSource_txx
#define _itkAutomaticTopologyMeshSource_txx

// For debugging.
#include <iostream>
#include <algorithm>

#include "itkAutomaticTopologyMeshSource.h"
#include "itkNumericTraits.h"

namespace itk
{

unsigned long
IdentifierArrayHashFunction
::operator()(
  Array< unsigned long > identifierArray
  ) const 
{
  typedef unsigned long Ulong;

  Ulong size = identifierArray.Size();

  std::sort( identifierArray.begin(), identifierArray.end() );

  Ulong hash = 0;
  Ulong* id = &identifierArray[ 0 ];

  while( size-- )
    {
    hash += *id++;
    hash = (hash << 7) | (hash >> 25); // Rotate left by 7.
    }

  return hash;

}

bool
IdentifierArrayEqualsFunction
::operator()(
  Array< unsigned long > identifierArray1,
  Array< unsigned long > identifierArray2
  ) const
{
  typedef unsigned long Ulong;

  
  Ulong size1 = identifierArray1.Size();
  Ulong size2 = identifierArray2.Size();

  if( size1 != size2 )
    {
    return false;
    }

  std::sort( identifierArray1.begin(), identifierArray1.end() );
  std::sort( identifierArray2.begin(), identifierArray2.end() );

  return ( identifierArray1 == identifierArray2 );

}

template<class TOutputMesh>
AutomaticTopologyMeshSource<TOutputMesh>
::AutomaticTopologyMeshSource()
{
  /** Create the output. */
  m_OutputMesh = TOutputMesh::New();
  this->ProcessObject::SetNumberOfRequiredOutputs(1);
  this->ProcessObject::SetNthOutput(0, m_OutputMesh.GetPointer());
}

template<class TOutputMesh>
AutomaticTopologyMeshSource<TOutputMesh>
::~AutomaticTopologyMeshSource()
{
}

template<class TOutputMesh>
typename AutomaticTopologyMeshSource< TOutputMesh >::IdentifierType
AutomaticTopologyMeshSource<TOutputMesh>
::AddVertex( const CellArrayType& cellArray )
{
  IdentifierType nextNewID = m_OutputMesh->GetNumberOfCells();

  // m_PointsHashTable[ foo ] is set to 0 if foo is not found, but I
  // want the initial identifier to be 0.  
  IdentifierType& identifierPlusOne = m_CellsHashTable[ cellArray ];
  IdentifierType identifier;
  
  if( identifierPlusOne == 0 )
    {
    identifier = nextNewID;

    // Set the value in the hash table 
    identifierPlusOne = identifier + 1;

    CellType::CellAutoPointer cellPointer;
    cellPointer.TakeOwnership( new VertexCell );
    cellPointer->SetPointId( 0, cellArray[0] );
    m_OutputMesh->SetCell( identifier, cellPointer ); 
    }
  else
    {
    identifier = identifierPlusOne - 1;
    }
  return identifier;
}

template<class TOutputMesh>
typename AutomaticTopologyMeshSource< TOutputMesh >::IdentifierType
AutomaticTopologyMeshSource<TOutputMesh>
::AddLine( const CellArrayType& cellArray )
{
  IdentifierType nextNewID = m_OutputMesh->GetNumberOfCells();

  // m_PointsHashTable[ foo ] is set to 0 if foo is not found, but I
  // want the initial identifier to be 0.  
  IdentifierType& identifierPlusOne = m_CellsHashTable[ cellArray ];
  IdentifierType identifier;


  if( identifierPlusOne == 0 )
    {
    const size_t size = cellArray.Size();
    identifier = nextNewID;

    CellArrayType vertexArray( size );

    // Set the value in the hash table 
    identifierPlusOne = identifier + 1;

    CellType::CellAutoPointer cellPointer;
    cellPointer.TakeOwnership( new LineCell );

    int i;
    for( i = 0; i < size; i++ )
      {
      IdentifierType pointId = cellArray[i];
      vertexArray[i] = AddVertex( pointId );
      cellPointer->SetPointId( i, pointId );
      }
    m_OutputMesh->SetCell( identifier, cellPointer ); 
    for( i = 0; i < size; i++ )
      {
      m_OutputMesh->SetBoundaryAssignment( 0, identifier, i, vertexArray[i] );
      }
    }
  else
    {
    identifier = identifierPlusOne - 1;
    }
  return identifier;
}

template<class TOutputMesh>
typename AutomaticTopologyMeshSource< TOutputMesh >::IdentifierType
AutomaticTopologyMeshSource<TOutputMesh>
::AddLine( const IdentifierType cellArray[ 2 ] )
{
  CellArrayType ca( 2 );
  std::copy( &cellArray[0], &cellArray[2], ca.begin() );
  return AddLine( ca );
}

template<class TOutputMesh>
typename AutomaticTopologyMeshSource< TOutputMesh >::IdentifierType
AutomaticTopologyMeshSource< TOutputMesh >
::AddVertex( IdentifierType pointId )
{
  Array<IdentifierType> cellArray( 1 );
  cellArray[ 0 ] = pointId;
  return AddVertex( cellArray );
}

template<class TOutputMesh>
typename AutomaticTopologyMeshSource< TOutputMesh >::IdentifierType
AutomaticTopologyMeshSource< TOutputMesh >
::AddVertex( const PointType& p0 )
{
  IdentifierType pointId = AddPoint( p0 );
  return AddVertex( pointId );
}

template<class TOutputMesh>
typename AutomaticTopologyMeshSource< TOutputMesh >::IdentifierType
AutomaticTopologyMeshSource< TOutputMesh >
::AddVertex( const CoordinateType p0[ PointDimension ] )
{
  IdentifierType pointId = AddPoint( p0 );
  return AddVertex( pointId );
}

template<class TOutputMesh>
typename AutomaticTopologyMeshSource< TOutputMesh >::IdentifierType
AutomaticTopologyMeshSource<TOutputMesh>
::AddPoint( const PointType& p0 )
{
  IdentifierType nextNewID = m_OutputMesh->GetNumberOfPoints();

  // m_PointsHashTable[ foo ] is set to 0 if foo is not found, but I
  // want the initial identifier to be 0.  
  IdentifierType& identifierPlusOne = m_PointsHashTable[ p0 ];
  IdentifierType identifier;
  if( identifierPlusOne == 0 )
    {
    identifier = nextNewID;

    // Set the value in the hash table 
    identifierPlusOne = identifier + 1;

    m_OutputMesh->SetPoint( identifier, p0 );
    }
  else
    {
    identifier = identifierPlusOne - 1;
    }
  return identifier;
}

template<class TOutputMesh>
typename AutomaticTopologyMeshSource< TOutputMesh >::IdentifierType
AutomaticTopologyMeshSource<TOutputMesh>
::AddPoint( const CoordinateType p0[ PointDimension ] )
{
  PointType newPoint;
  int i;
  for( i = 0; i < PointDimension; i++ ) { newPoint[ i ] = p0[ i ]; }
  return AddPoint( newPoint );
}

#if 0
template<class TOutputMesh>
typename AutomaticTopologyMeshSource< TOutputMesh >::IdentifierType
AutomaticTopologyMeshSource<TOutputMesh>
::AddLine( IdentifierType p0id, IdentifierType p1id )
{
  CellAutoPointer line;
  line.TakeOwnership( new LineType );
  line.SetPointId( 0, p0id );
  line.SetPointId( 1, p1id );
}
#endif

template<class TOutputMesh>
void
AutomaticTopologyMeshSource<TOutputMesh>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf(os, indent);
}

} /** end namespace itk. */

#endif
