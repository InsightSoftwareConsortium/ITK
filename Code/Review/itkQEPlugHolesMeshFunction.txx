// -------------------------------------------------------------------------
// itkQEPlugHolesMeshFunction.txx
// $Revision: 1.2 $
// $Author: ibanez $
// $Name:  $
// $Date: 2007-01-13 12:42:15 $
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
// - The duck master (Alex Gouaillard) gouaillard@creatis.insa-lyon.fr
// - The cow  master (Leonardo Florez) florez@creatis.insa-lyon.fr
// -------------------------------------------------------------------------
#ifndef __ITKQUADEDGEMESH__ITKQEPLUGHOLESMESHFUNCTION__TXX__
#define __ITKQUADEDGEMESH__ITKQEPLUGHOLESMESHFUNCTION__TXX__

#include "itkQEMesh.h"  // Just to mark the dependance towards this class.
#include "itkQuadEdgeMeshBoundaryEdgesMeshFunction.h"

namespace itk
{

template < class TMesh >
  typename PlugHolesMeshFunction< TMesh >::OutputType
  PlugHolesMeshFunction< TMesh >::
  Evaluate( )
{
  if( !this->m_Mesh )
    {
    itkWarningMacro( "No mesh present." );
    return;
    } 

  typedef QuadEdgeMeshBoundaryEdgesMeshFunction< MeshType > BoundaryEdges;
  typename BoundaryEdges::Pointer boundaryEdges = BoundaryEdges::New( );

  typename MeshType::EdgeListType* boundaries =
                       boundaryEdges->Evaluate( *this->m_Mesh );

  if( boundaries->empty( ) )
    {
    return;
    }

  typedef typename MeshType::QEPrimal QEPrimal;
  while( ! boundaries->empty( ) )
    {
    QEPrimal* boundaryEdge = boundaries->front( );
    boundaries->pop_front( );
    // Follow, with Lnext(), the boundary:
    typename MeshType::PointIdList pList;
    typename QEPrimal::IteratorGeom it = boundaryEdge->BeginGeomLnext( );
    for( ; it != boundaryEdge->EndGeomLnext( ); it++ )
      {
      pList.push_back( it.Value( )->GetOrg( ) );
      }
    this->m_Mesh->AddFace( pList );
    } 
}

} 

#endif 


