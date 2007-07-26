// -------------------------------------------------------------------------
// itkQuadEdgeMeshEulerOperatorCreateCenterVertexFunction.txx
// $Revision: 1.1 $
// $Author: hanfei $
// $Name:  $
// $Date: 2007-07-26 06:30:26 $
// -------------------------------------------------------------------------
// This code is an implementation of the well known quad edge (QE) data
// structure in the ITK library. Although the original QE can handle non
// orientable 2-manifolds and its dual and its mirror, this implementation
// is specifically dedicated to handle orientable 2-manifolds along with
// their dual.
//
// Any comment, criticism and/or donation is welcome.
//
// Please contact any member of the team:
//
// - The frog master (Eric Boix)       eboix@ens-lyon.fr
// - The duck master (Alex Gouaillard) alexandre.gouaillard@sun.com
// - The cow  master (Leonardo Florez) florez@creatis.insa-lyon.fr
// -------------------------------------------------------------------------
#ifndef __itkQuadEdgeMeshEulerOperatorCreateCenterVertexFunction_txx
#define __itkQuadEdgeMeshEulerOperatorCreateCenterVertexFunction_txx

namespace itk
{
template < class TMesh, class TQEType >
typename QuadEdgeMeshEulerOperatorCreateCenterVertexFunction< TMesh, TQEType >::OutputType
QuadEdgeMeshEulerOperatorCreateCenterVertexFunction< TMesh, TQEType >::
Evaluate( QEType* e )
{
  // Is there any input ?
  if( !this->m_Mesh )
    {
    itkDebugMacro( "No mesh present." );
    return( (OutputType) 0 );
    }

  // Is left face set ?
  if ( !e->IsLeftSet( ) )
    {
    itkDebugMacro( "Argument edge has no left face." );
    return( (OutputType) 0 );
    }
   
  // remove left face
  this->m_Mesh->DeleteFace( e->GetLeft( ) );
   
  // create new point geometry  
  unsigned int sum = 0;
  VectorType vec;
  vec.Fill( 0 );
  PointIdentifier pid = this->m_Mesh->FindFirstUnusedPointIndex( );
  typedef std::map< QEType*, PointIdentifier >       AssociatedBarycenters;
  AssociatedBarycenters m_AssocBary;
  typedef typename QEType::IteratorGeom QEIterator;
  QEIterator lit = e->BeginGeomLnext( );
  while( lit != e->EndGeomLnext( ) )
    {
    QEType* g = lit.Value( );
    vec += this->m_Mesh->GetVector( g->GetOrigin( ) );
    sum++;
    m_AssocBary[ g ] = pid;
    lit++;
    } // rof
  vec /= CoordRepType( sum );
  PointType p;
  for( unsigned int i = 0; i<3; i++)
    {
    p[i] = vec[i];
    }
 
  // add new point to mesh
  this->m_NewPointID = this->m_Mesh->AddPoint( p );
  PointIdentifier tempPoint;
   
  // create edges and faces
  tempPoint = e->GetDestination( );
  this->m_Mesh->AddFaceTriangle( this->m_NewPointID, 
                                 e->GetOrigin( ), 
                                 tempPoint );
  QEType* edgeRef = this->m_Mesh->FindEdge( this->m_NewPointID, tempPoint );
  while ( !edgeRef->IsLeftSet( ) )
    {
    tempPoint = edgeRef->GetLnext( )->GetDestination( );
    this->m_Mesh->AddFaceTriangle( this->m_NewPointID,
                                   edgeRef->GetLnext( )->GetOrigin( ),
                                   tempPoint );
    edgeRef = this->m_Mesh->FindEdge( this->m_NewPointID, tempPoint );
    }

   
  return( e->GetLnext( ) );

}

} // namespace itk

#endif

// eof - itkQuadEdgeMeshEulerOperatorCreateCenterVertexFunction.txx
