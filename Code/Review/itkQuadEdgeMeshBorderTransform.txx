/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshBorderTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkQuadEdgeMeshBorderTransform_txx
#define __itkQuadEdgeMeshBorderTransform_txx

#include "itkQuadEdgeMeshBorderTransform.h"

namespace itk
{

// ----------------------------------------------------------------------------
template< class TInputMesh, class TOutputMesh >
QuadEdgeMeshBorderTransform< TInputMesh, TOutputMesh >
::QuadEdgeMeshBorderTransform( ) :  m_TransformType( SQUARE_BORDER_TRANSFORM ), m_Radius( 10000. )
{
}

// ----------------------------------------------------------------------------
template< class TInputMesh, class TOutputMesh >
typename QuadEdgeMeshBorderTransform< TInputMesh, TOutputMesh >::MapPointIdentifier
QuadEdgeMeshBorderTransform< TInputMesh, TOutputMesh >
::GetBoundaryPtMap( )
{
  return m_BoundaryPtMap;
}

// ----------------------------------------------------------------------------
template< class TInputMesh, class TOutputMesh >
typename QuadEdgeMeshBorderTransform< TInputMesh, TOutputMesh >::InputVectorPointType
QuadEdgeMeshBorderTransform< TInputMesh, TOutputMesh >
::GetBorder( )
{
  return m_Border;
}


// ----------------------------------------------------------------------------
template< class TInputMesh, class TOutputMesh >
void
QuadEdgeMeshBorderTransform< TInputMesh, TOutputMesh >
::ComputeBoundary( )
{
  BoundaryRepresentativeEdgesPointer
    boundaryRepresentativeEdges = BoundaryRepresentativeEdgesType::New( );

  InputMeshPointer input = this->GetInput( );
  InputEdgeListType* list = boundaryRepresentativeEdges->Evaluate( *input );

  InputQEType* bdryEdge = ( *list->begin( ) );

  InputPointIdentifier i = 0;

  for( InputIteratorGeom it = bdryEdge->BeginGeomLnext( );
        it != bdryEdge->EndGeomLnext( );
        ++it, i++ )
    {
    m_BoundaryPtMap[it.Value( )->GetOrigin( )] = i;
    }

  m_Border.resize( i );
}


// ----------------------------------------------------------------------------
template< class TInputMesh, class TOutputMesh >
void QuadEdgeMeshBorderTransform< TInputMesh, TOutputMesh >::
GenerateData( )
{
  ComputeTransform();
}


// ----------------------------------------------------------------------------
// *** under testing ***
template< class TInputMesh, class TOutputMesh >
typename QuadEdgeMeshBorderTransform< TInputMesh, TOutputMesh >::InputEdgeListIterator
QuadEdgeMeshBorderTransform< TInputMesh, TOutputMesh >::
ComputeLongestBorder( )
{
  BoundaryRepresentativeEdgesPointer
        boundaryRepresentativeEdges = BoundaryRepresentativeEdgesType::New( );

  InputMeshPointer input = this->GetInput( );

  InputEdgeListPointerType list = boundaryRepresentativeEdges->Evaluate( *input );

  InputCoordRepType max_length( 0. ), length( 0. );
  InputEdgeListIterator oborder_it = list->begin( );

  for( InputEdgeListIterator b_it = list->begin( );
         b_it != list->end( );
         ++b_it )
    {
    length = 0.;

    for( InputIteratorGeom e_it = b_it->BeginGeomLnext( );
           e_it != b_it->EndGeomLnext( );
           ++e_it )
      {
      length += e_it->GetOrigin( ).EuclideanDistanceTo(
                  e_it->GetDestinationination( ) );
      }
    if( length > max_length )
      {
      max_length = length;
      oborder_it = b_it;
      }
    }
  return oborder_it;
}


// ----------------------------------------------------------------------------
template< class TInputMesh, class TOutputMesh >
typename QuadEdgeMeshBorderTransform< TInputMesh, TOutputMesh >::InputEdgeListIterator
QuadEdgeMeshBorderTransform< TInputMesh, TOutputMesh >::
ComputeLargestBorder( )
{
  BoundaryRepresentativeEdgesPointer
            boundaryRepresentativeEdges = BoundaryRepresentativeEdgesType::New( );

  InputMeshPointer input = this->GetInput( );
    
  InputEdgeListType* list = boundaryRepresentativeEdges->Evaluate( *input );

  unsigned long max_id = 0L;
  unsigned long k = 0L;

  InputEdgeListIterator oborder_it = list->begin( );

  for( InputEdgeListIterator b_it = list->begin( );
         b_it != list->end( );
         ++b_it )
    {
    k = 0;

    for( InputIteratorGeom e_it = b_it->BeginGeomLnext( );
         e_it != b_it->EndGeomLnext( );
         ++e_it )
      {
      k++;
      }
      
    if( k > max_id )
      {
      max_id = k;
      oborder_it = b_it;
      }
    }
  return oborder_it;

}


// ----------------------------------------------------------------------------
template< class TInputMesh, class TOutputMesh >
void
QuadEdgeMeshBorderTransform< TInputMesh, TOutputMesh >
::DiskTransform( )
{
  InputMeshPointer input = this->GetInput( );

  size_t NbBoundaryPt = m_BoundaryPtMap.size( );

  InputCoordRepType r = RadiusMaxSquare( );

  InputCoordRepType two_r = 2. * r;
  InputCoordRepType inv_two_r = 1. / two_r;

  InputPointIdentifier id = m_BoundaryPtMap.begin( )->first;
  InputPointType pt1 = input->GetPoint( id );
  
  id = ( --m_BoundaryPtMap.end( ) )->first;
  InputPointType pt2 = input->GetPoint( id );

  InputCoordRepType dist = pt1.SquaredEuclideanDistanceTo( pt2 );

  std::vector< InputCoordRepType > tetas( NbBoundaryPt, 0. );
  tetas[0] = static_cast< InputCoordRepType >(
          vcl_acos( ( two_r - dist ) * inv_two_r ) );

  MapPointIdentifierIterator BoundaryPtIterator = m_BoundaryPtMap.begin( );

  ++BoundaryPtIterator;

  OutputPointIdentifier j = 1;

  while( BoundaryPtIterator !=  m_BoundaryPtMap.end() )
    {
    pt1 = pt2;

    id = BoundaryPtIterator->first;
    pt2 = input->GetPoint( id );

    dist = pt1.SquaredEuclideanDistanceTo( pt2 );

    tetas[j] = tetas[j-1] + vcl_acos( ( two_r - dist ) * inv_two_r );

    ++j;
    ++BoundaryPtIterator;
    }

  InputCoordRepType a = ( 2. * vnl_math::pi ) / tetas[NbBoundaryPt-1];

  m_Radius = vcl_pow( vcl_sqrt( r ), a );

  for( MapPointIdentifierIterator
        BoundaryPtMapIterator = m_BoundaryPtMap.begin( );
       BoundaryPtMapIterator != m_BoundaryPtMap.end( );
       ++BoundaryPtMapIterator )
    {
    id = BoundaryPtMapIterator->first;
    j = BoundaryPtMapIterator->second;

    pt1[0] =  m_Radius *
      static_cast< InputCoordRepType >( vcl_cos( a * tetas[j] ) );
    pt1[1] =  m_Radius *
      static_cast< InputCoordRepType >( vcl_sin( a * tetas[j] ) );
    pt1[2] = 0.;

    m_Border[j] = pt1;
    }

}


// ----------------------------------------------------------------------------
template< class TInputMesh, class TOutputMesh >
typename QuadEdgeMeshBorderTransform< TInputMesh, TOutputMesh >::InputCoordRepType
QuadEdgeMeshBorderTransform< TInputMesh, TOutputMesh >
::RadiusMaxSquare( )
{
  InputMeshPointer input = this->GetInput( );

  InputPointType center = GetMeshBarycentre( );

  InputCoordRepType oRmax( 0.), r;

  for( MapPointIdentifierIterator
        BoundaryPtIterator = m_BoundaryPtMap.begin( );
       BoundaryPtIterator != m_BoundaryPtMap.end( );
       ++BoundaryPtIterator )
    {
//    InputPointIdentifier pid = BoundaryPtIterator->first;

    r = static_cast< InputCoordRepType >(
        center.SquaredEuclideanDistanceTo(
          input->GetPoint( BoundaryPtIterator->first ) ) );

    if( r > oRmax )
      {
      oRmax = r;
      }
    }

  oRmax *= 2.25;

  return oRmax;

}


// ----------------------------------------------------------------------------
template< class TInputMesh, class TOutputMesh >
typename QuadEdgeMeshBorderTransform< TInputMesh, TOutputMesh >::InputPointType
QuadEdgeMeshBorderTransform< TInputMesh, TOutputMesh >
::GetMeshBarycentre( )
{
  InputMeshPointer input = this->GetInput( );

  InputPointType oCenter;
  oCenter.Fill( 0. );

  InputPointsContainer* points = input->GetPoints( );

  InputPointType pt;
  unsigned int i;
  
  for( InputPointsContainerConstIterator PointIterator = points->Begin( );
       PointIterator != points->End( );
       ++PointIterator )
    {
    pt = PointIterator.Value( );

    for( i = 0; i < PointDimension; ++i )
      {
      oCenter[i] += pt[i];
      }
    }

  InputCoordRepType invNbOfPoints = 1. /
          static_cast< InputCoordRepType >( input->GetNumberOfPoints( ) );

  for( i = 0; i < PointDimension; ++i )
    {
    oCenter[i] *= invNbOfPoints;
    }

  return oCenter;
}


// ----------------------------------------------------------------------------
template< class TInputMesh, class TOutputMesh >
void QuadEdgeMeshBorderTransform< TInputMesh, TOutputMesh >
::ComputeTransform( )
{
  this->ComputeBoundary( );

  switch( m_TransformType )
    {
    default:
    case SQUARE_BORDER_TRANSFORM:
        ArcLengthSquareTransform( );
        break;

    case DISK_BORDER_TRANSFORM:
        DiskTransform( );
        break;
    }
}


// ----------------------------------------------------------------------------
template< class TInputMesh, class TOutputMesh >
void
QuadEdgeMeshBorderTransform< TInputMesh, TOutputMesh >
::ArcLengthSquareTransform( )
{
  BoundaryRepresentativeEdgesPointer
      boundaryRepresentativeEdges = BoundaryRepresentativeEdgesType::New( );

  InputMeshPointer input = this->GetInput( );
  InputEdgeListType* list = boundaryRepresentativeEdges->Evaluate( *input );

  InputQEType* bdryEdge = ( *list->begin( ) );

  size_t NbBoundaryPt = m_BoundaryPtMap.size( );
  std::vector< InputCoordRepType > Length( NbBoundaryPt + 1, 0. );

  InputCoordRepType TotalLength( 0. ), distance;

  InputPointIdentifier i(0), org(0), dest(0);
  InputPointType PtOrg, PtDest;

  for( InputIteratorGeom it = bdryEdge->BeginGeomLnext( );
       it != bdryEdge->EndGeomLnext( );
       ++it, ++i )
    {
    org = it.Value( )->GetOrigin( );
    dest = it.Value( )->GetDestination( );

    PtOrg = input->GetPoint( org );
    PtDest = input->GetPoint( dest );

    distance = PtOrg.EuclideanDistanceTo( PtDest );
    TotalLength += distance;
    Length[i] = TotalLength;
    }

  InputCoordRepType EdgeLength = 2. * m_Radius;
  InputCoordRepType ratio = 4. * EdgeLength / TotalLength;

  for( i = 0; i < NbBoundaryPt + 1; ++i )
    {
    Length[i] *= ratio;
    }

  InputPointType pt;
  pt[0] = -m_Radius;
  pt[1] = m_Radius;
  pt[2] = 0.;

  m_Border[0] = pt;

  i = 1;
  while( Length[i] < EdgeLength )
    {
    pt[0] = -m_Radius + Length[i];
    m_Border[ i++ ] = pt;
    }

  pt[0] = m_Radius;
  pt[1] = m_Radius;
  m_Border[ i++ ] = pt;

  while( Length[i] < ( 2. * EdgeLength ) )
    {
    pt[1] = m_Radius - ( Length[i] - EdgeLength );
    m_Border[ i++ ] = pt;
    }

  pt[0] = m_Radius;
  pt[1] = - m_Radius;
  m_Border[ i++ ] = pt;

  while( Length[i] < ( 3. * EdgeLength ) )
    {
    pt[0] = m_Radius - ( Length[i] - 2. * EdgeLength );
    m_Border[ i++ ] = pt;
    }

  pt[0] = - m_Radius;
  pt[1] = - m_Radius;
  m_Border[ i++ ] = pt;

  while( i < NbBoundaryPt )
    {
    pt[1] = -m_Radius + ( Length[i] - 3. * EdgeLength );
    m_Border[ i++ ] = pt;
    }
}


}

#endif
